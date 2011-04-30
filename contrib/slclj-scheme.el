;;; slclj-scheme.el --- Support Scheme programs running under Common Lisp
;;
;; Authors: Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;; 
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slclj-load-hook (lambda () (require 'slclj-scheme)))
;;

(defun slclj-scheme-mode-hook ()
  (slclj-mode 1))

(defun slclj-scheme-indentation-update (symbol indent)
  ;; Does the symbol have an indentation value that we set?
  (when (equal (get symbol 'scheme-indent-function)
	       (get symbol 'slclj-scheme-indent))
    (put symbol 'slclj-scheme-indent indent)
    (put symbol 'scheme-indent-function indent)))


;;; Initialization

(defun slclj-scheme-init ()
  (add-hook 'scheme-mode-hook 'slclj-scheme-mode-hook)
  (add-hook 'slclj-indentation-update-hooks 'slclj-scheme-indentation-update)
  (add-to-list 'slclj-lisp-modes 'scheme-mode))

(defun slclj-scheme-unload ()
  (remove-hook 'scheme-mode-hook 'slclj-scheme-mode-hook)
  (remove-hook 'slclj-indentation-update-hooks 'slclj-scheme-indentation-update)
  (setq slclj-lisp-modes (remove 'scheme-mode slclj-lisp-modes)))

(provide 'slclj-scheme)
