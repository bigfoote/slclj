;;; slclj-scratch.el --- Object clipboard for SLCLJ
;;
;; Author: Helmut Eller  <heller@common-lisp.net>
;; License: GNU GPL (same license as Emacs)
;;
;; This add a few commands to put objects into a clipboard and
;; to insert textual references to those objects.
;;
;; The clipboard command prefix is C-c @.
;;
;; C-c @ +   adds an object to the clipboard
;; C-c @ @   inserts a reference to an object in the clipboard
;; C-c @ ?   displays the clipboard
;;
;; This package also also binds the + key in the inspector and
;; debugger to add the object at point to the clipboard.
;;

(require 'slclj)

(define-derived-mode slclj-clipboard-mode fundamental-mode
  "Slclj-Clipboard"
  "SLCLJ Clipboad Mode.

\\{slclj-clipboard-mode-map}")

(slclj-define-keys slclj-clipboard-mode-map
  ("g" 'slclj-clipboard-redisplay)
  ((kbd "C-k") 'slclj-clipboard-delete-entry)
  ("i" 'slclj-clipboard-inspect))

(defvar slclj-clipboard-map (make-sparse-keymap))

(slclj-define-keys slclj-clipboard-map
  ("?" 'slclj-clipboard-display)
  ("+" 'slclj-clipboard-add)
  ("@" 'slclj-clipboard-ref))

(define-key slclj-mode-map (kbd "C-c @") slclj-clipboard-map)
(define-key slclj-repl-mode-map (kbd "C-c @") slclj-clipboard-map)

(slclj-define-keys slclj-inspector-mode-map
  ("+" 'slclj-clipboard-add-from-inspector))

(slclj-define-keys sldb-mode-map
  ("+" 'slclj-clipboard-add-from-sldb))

(defun slclj-clipboard-add (exp package)
  "Add an object to the clipboard."
  (interactive (list (slclj-read-from-minibuffer 
                      "Add to clipboard (evaluated): "
                      (slclj-sexp-at-point))
		     (slclj-current-package)))
  (slclj-clipboard-add-internal `(:string ,exp ,package)))

(defun slclj-clipboard-add-internal (datum)
  (slclj-eval-async `(swank-clipboard:add ',datum) 
		    (lambda (result) (message "%s" result))))

(defun slclj-clipboard-display ()
  "Display the content of the clipboard."
  (interactive)
  (slclj-eval-async `(swank-clipboard:entries) 
		    #'slclj-clipboard-display-entries))

(defun slclj-clipboard-display-entries (entries)
  (slclj-with-popup-buffer ("*Slclj Clipboard*")
    (slclj-clipboard-mode)
    (slclj-clipboard-insert-entries entries)))

(defun slclj-clipboard-insert-entries (entries)
  (let ((fstring "%2s %3s %s\n"))
    (insert (format fstring "Nr" "Id" "Value")
            (format fstring "--" "--" "-----" ))
    (save-excursion
      (loop for i from 0 for (ref . value) in entries do
	    (slclj-insert-propertized `(slclj-clipboard-entry ,i
					slclj-clipboard-ref ,ref)
				      (format fstring i ref value))))))

(defun slclj-clipboard-redisplay ()
  "Update the clipboard buffer."
  (interactive)
  (slclj-eval-async 
   `(swank-clipboard:entries) 
   (lambda (entries) 
     (let ((inhibit-read-only t))
       (slclj-save-coordinates (point)
	 (erase-buffer)
	 (slclj-clipboard-insert-entries entries))))))

(defun slclj-clipboard-entry-at-point ()
  (or (get-text-property (point) 'slclj-clipboard-entry)
      (error "No clipboard entry at point")))

(defun slclj-clipboard-ref-at-point ()
  (or (get-text-property (point) 'slclj-clipboard-ref)
      (error "No clipboard ref at point")))

(defun slclj-clipboard-inspect (&optional entry)
  "Inspect the current clipboard entry."
  (interactive (list (slclj-clipboard-ref-at-point)))
  (slclj-inspect (prin1-to-string `(swank-clipboard::clipboard-ref ,entry))))

(defun slclj-clipboard-delete-entry (&optional entry)
  "Delete the current entry from the clipboard."
  (interactive (list (slclj-clipboard-entry-at-point)))
  (slclj-eval-async `(swank-clipboard:delete-entry ,entry)
		    (lambda (result) 
		      (slclj-clipboard-redisplay)
		      (message "%s" result))))

(defun slclj-clipboard-ref ()
  "Ask for a clipboard entry number and insert a reference to it."
  (interactive)
  (slclj-clipboard-read-entry-number #'slclj-clipboard-insert-ref))
  
;; insert a reference to clipboard entry ENTRY at point.  The text
;; receives a special 'display property to make it look nicer.  We
;; remove this property in a modification when a user tries to modify
;; he real text.
(defun slclj-clipboard-insert-ref (entry)
  (destructuring-bind (ref . string) 
      (slclj-eval `(swank-clipboard:entry-to-ref ,entry))
    (slclj-insert-propertized
     `(display ,(format "#@%d%s" ref string)
	       modification-hooks (slclj-clipboard-ref-modified)
	       rear-nonsticky t)
     (format "(swank-clipboard::clipboard-ref %d)" ref))))

(defun slclj-clipboard-ref-modified (start end)
  (when (get-text-property start 'display)
    (let ((inhibit-modification-hooks t))
      (save-excursion
	(goto-char start)
	(destructuring-bind (dstart dend) (slclj-property-bounds 'display)
	  (unless (and (= start dstart) (= end dend))
	    (remove-list-of-text-properties 
	     dstart dend '(display modification-hooks))))))))

;; Read a entry number.
;; Written in CPS because the display the clipboard before reading.
(defun slclj-clipboard-read-entry-number (k)
  (slclj-eval-async 
   `(swank-clipboard:entries) 
   (slclj-rcurry
    (lambda (entries window-config k)
      (slclj-clipboard-display-entries entries)
      (let ((entry (unwind-protect
		       (read-from-minibuffer "Entry number: " nil nil t)
		     (set-window-configuration window-config))))
	(funcall k entry)))
    (current-window-configuration)
    k)))

(defun slclj-clipboard-add-from-inspector ()
  (interactive)
  (let ((part (or (get-text-property (point) 'slclj-part-number)
		  (error "No part at point"))))
    (slclj-clipboard-add-internal `(:inspector ,part))))

(defun slclj-clipboard-add-from-sldb ()
  (interactive)
  (slclj-clipboard-add-internal 
   `(:sldb ,(sldb-frame-number-at-point) 
	   ,(sldb-var-number-at-point))))

(defun slclj-clipboard-init ()
  (slclj-require :swank-clipboard))

(provide 'slclj-clipboard)
