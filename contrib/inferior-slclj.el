;;; inferior-slclj.el --- Minor mode with Slclj keys for comint buffers
;;
;; Author: Luke Gorrie  <luke@synap.se>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slclj-load-hook (lambda () (require 'inferior-slclj)))
;;   (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slclj-mode 1)))

(define-minor-mode inferior-slclj-mode
  "\\<slclj-mode-map>\
Inferior SLCLJ mode: The Inferior Superior Lisp Mode for Emacs.

This mode is intended for use with `inferior-lisp-mode'. It provides a
subset of the bindings from `slclj-mode'.

\\{inferior-slclj-mode-map}"
  nil
  nil
  ;; Fake binding to coax `define-minor-mode' to create the keymap
  '((" " 'undefined)))

(add-to-list 'minor-mode-alist
             '(inferior-slclj-mode
               (" Inf-Slclj" slclj-state-name)))

(defun inferior-slclj-return ()
  "Handle the return key in the inferior-lisp buffer.
The current input should only be sent if a whole expression has been
entered, i.e. the parenthesis are matched.

A prefix argument disables this behaviour."
  (interactive)
  (if (or current-prefix-arg (inferior-slclj-input-complete-p))
      (comint-send-input)
    (insert "\n")
    (inferior-slclj-indent-line)))

(defun inferior-slclj-indent-line ()
  "Indent the current line, ignoring everything before the prompt."
  (interactive)
  (save-restriction
    (let ((indent-start
           (save-excursion
             (goto-char (process-mark (get-buffer-process (current-buffer))))
             (let ((inhibit-field-text-motion t))
               (beginning-of-line 1))
             (point))))
      (narrow-to-region indent-start (point-max)))
    (lisp-indent-line)))

(defun inferior-slclj-input-complete-p ()
  "Return true if the input is complete in the inferior lisp buffer."
  (slclj-input-complete-p (process-mark (get-buffer-process (current-buffer)))
                          (point-max)))

(defun inferior-slclj-closing-return ()
  "Send the current expression to Lisp after closing any open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region (process-mark (get-buffer-process (current-buffer)))
                      (point-max))
    (while (ignore-errors (save-excursion (backward-up-list 1) t))
      (insert ")")))
  (comint-send-input))

(defun inferior-slclj-change-directory (directory)
  "Set default-directory in the *inferior-lisp* buffer to DIRECTORY."
  (let* ((proc (slclj-process))
	 (buffer (and proc (process-buffer proc))))
    (when buffer 
      (with-current-buffer buffer
	(cd-absolute directory)))))

(defun inferior-slclj-init-keymap ()
  (let ((map inferior-slclj-mode-map))
    (set-keymap-parent map slclj-parent-map)
    (slclj-define-keys map
      ([return]			'inferior-slclj-return)
      ([(control return)]	'inferior-slclj-closing-return)
      ([(meta control ?m)]	'inferior-slclj-closing-return)
      ("\t"			'slclj-indent-and-complete-symbol)
      (" "			'slclj-space))))

(inferior-slclj-init-keymap)

(defun inferior-slclj-hook-function ()
  (inferior-slclj-mode 1))

(defun inferior-slclj-switch-to-repl-buffer ()
  (switch-to-buffer (process-buffer (slclj-inferior-process))))

(defun inferior-slclj-show-transcript (string)
  (remove-hook 'comint-output-filter-functions
	       'inferior-slclj-show-transcript t)
  (display-buffer (process-buffer (slclj-inferior-process)) t))

(defun inferior-slclj-start-transcript ()
  (let ((proc (slclj-inferior-process)))
    (when proc
      (with-current-buffer (process-buffer proc)
	(add-hook 'comint-output-filter-functions 
		  'inferior-slclj-show-transcript
		  nil t)))))

(defun inferior-slclj-stop-transcript ()
  (let ((proc (slclj-inferior-process)))
    (when proc
      (with-current-buffer (process-buffer (slclj-inferior-process))
	(run-with-timer 0.2 nil 
			(lambda (buffer) 
			  (with-current-buffer buffer
			    (remove-hook 'comint-output-filter-functions
					 'inferior-slclj-show-transcript t)))
			(current-buffer))))))

(defun inferior-slclj-init ()
  (add-hook 'slclj-inferior-process-start-hook 'inferior-slclj-hook-function)
  (add-hook 'slclj-change-directory-hooks 'inferior-slclj-change-directory)
  (add-hook 'slclj-transcript-start-hook 'inferior-slclj-start-transcript)
  (add-hook 'slclj-transcript-stop-hook 'inferior-slclj-stop-transcript)
  (def-slclj-selector-method ?r
    "SLCLJ Read-Eval-Print-Loop."
    (process-buffer (slclj-inferior-process))))

(provide 'inferior-slclj)
