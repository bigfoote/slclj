;;; slclj-xref-browser.el --- xref browsing with tree-widget
;;
;; Author: Rui Patrocínio <rui.patrocinio@netvisao.pt>
;; Licencse: GNU GPL (same license as Emacs)
;; 
;;; Installation:
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (slclj-setup '(slclj-xref-browser ... possibly other packages ...))
;;


;;;; classes browser

(defun slclj-expand-class-node (widget)
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
	(loop for kid in (slclj-eval `(swank:mop :subclasses ,name))
	      collect `(tree-widget :tag ,kid
				    :expander slclj-expand-class-node
				    :has-children t)))))

(defun slclj-browse-classes (name)
  "Read the name of a class and show its subclasses."
  (interactive (list (slclj-read-symbol-name "Class Name: ")))
  (slclj-call-with-browser-setup 
   "*slclj class browser*" (slclj-current-package) "Class Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name 
                    :expander 'slclj-expand-class-node 
                    :has-echildren t))))

(defvar slclj-browser-map nil
  "Keymap for tree widget browsers")

(require 'tree-widget)
(unless slclj-browser-map
  (setq slclj-browser-map (make-sparse-keymap))
  (set-keymap-parent slclj-browser-map widget-keymap)
  (define-key slclj-browser-map "q" 'bury-buffer))

(defun slclj-call-with-browser-setup (buffer package title fn)
  (switch-to-buffer buffer)
  (kill-all-local-variables)
  (setq slclj-buffer-package package)
  (let ((inhibit-read-only t)) (erase-buffer))
  (widget-insert title "\n\n")
  (save-excursion
    (funcall fn))
  (lisp-mode-variables t)
  (slclj-mode t)
  (use-local-map slclj-browser-map)
  (widget-setup))


;;;; Xref browser

(defun slclj-fetch-browsable-xrefs (type name)
  "Return a list ((LABEL DSPEC)).
LABEL is just a string for display purposes. 
DSPEC can be used to expand the node."
  (let ((xrefs '()))
    (loop for (_file . specs) in (slclj-eval `(swank:xref ,type ,name)) do
          (loop for (dspec . _location) in specs do
                (let ((exp (ignore-errors (read (downcase dspec)))))
                  (cond ((and (consp exp) (eq 'flet (car exp)))
                         ;; we can't expand FLET references so they're useless
                         )
                        ((and (consp exp) (eq 'method (car exp)))
                         ;; this isn't quite right, but good enough for now
                         (push (list dspec (string (second exp))) xrefs))
                        (t
                         (push (list dspec dspec) xrefs))))))
    xrefs))

(defun slclj-expand-xrefs (widget)
  (or (widget-get widget :args)
      (let* ((type (widget-get widget :xref-type))
             (dspec (widget-get widget :xref-dspec))
             (xrefs (slclj-fetch-browsable-xrefs type dspec)))
        (loop for (label dspec) in xrefs
              collect `(tree-widget :tag ,label
                                    :xref-type ,type
                                    :xref-dspec ,dspec
                                    :expander slclj-expand-xrefs
                                    :has-children t)))))

(defun slclj-browse-xrefs (name type)
  "Show the xref graph of a function in a tree widget."
  (interactive 
   (list (slclj-read-from-minibuffer "Name: "
                                     (slclj-symbol-at-point))
         (read (completing-read "Type: " (slclj-bogus-completion-alist
                                          '(":callers" ":callees" ":calls"))
                                nil t ":"))))
  (slclj-call-with-browser-setup 
   "*slclj xref browser*" (slclj-current-package) "Xref Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name :xref-type type :xref-dspec name 
                    :expander 'slclj-expand-xrefs :has-echildren t))))

(provide 'slclj-xref-browser)
