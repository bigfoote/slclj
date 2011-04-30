;;; slclj-scratch.el --- imitate Emacs' *scratch* buffer
;;
;; Author: Helmut Eller  <heller@common-lisp.net>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path ".../slclj/contrib")
;;   (add-hook 'slclj-load-hook (lambda () (require 'slclj-scratch)))
;;


;;; Code

(defvar slclj-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-map)
    map))

(defun slclj-scratch ()
  (interactive)
  (slclj-switch-to-scratch-buffer))

(defun slclj-switch-to-scratch-buffer ()
  (set-buffer (slclj-scratch-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t)))

(defvar slclj-scratch-file nil)

(defun slclj-scratch-buffer ()
  "Return the scratch buffer, create it if necessary."
  (or (get-buffer "*slclj-scratch*")
      (with-current-buffer (if slclj-scratch-file
                               (find-file slclj-scratch-file)
                             (get-buffer-create "*slclj-scratch*"))
        (rename-buffer "*slclj-scratch*")
	(lisp-mode)
	(use-local-map slclj-scratch-mode-map)
	(slclj-mode t)
	(current-buffer))))

(slclj-define-keys slclj-scratch-mode-map
  ("\C-j" 'slclj-eval-print-last-expression))

(defun slclj-scratch-init ()
  (def-slclj-selector-method ?s
    "*slclj-scratch* buffer."
    (slclj-scratch-buffer)))

(provide 'slclj-scratch)