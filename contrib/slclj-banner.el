;;; slclj-banner.el -- Persistent header line and startup animation
;;
;; Authors: Helmut Eller  <heller@common-lisp.net>
;;          Luke Gorrie  <luke@synap.se>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path ".../slclj/contrib")
;;   (add-hook 'slclj-load-hook (lambda () (require 'slclj-banner)))

(defcustom slclj-startup-animation (fboundp 'animate-string)
   "Enable the startup animation."
   :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
   :group 'slclj-ui)

(defcustom slclj-header-line-p (boundp 'header-line-format)
  "If non-nil, display a header line in Slclj buffers."
  :type 'boolean
  :group 'slclj-repl)

(defun slclj-startup-message ()
  (when slclj-header-line-p
    (setq header-line-format 
          (format "%s  Port: %s  Pid: %s"
                  (slclj-lisp-implementation-type)
                  (slclj-connection-port (slclj-connection))
                  (slclj-pid))))
  (when (zerop (buffer-size))
    (let ((welcome (concat "; SLCLJ " (or (slclj-changelog-date) 
                                          "- ChangeLog file not found"))))
      (if slclj-startup-animation
          (animate-string welcome 0 0) 
        (insert welcome)))))

(defun slclj-banner-init ()
  (setq slclj-repl-banner-function 'slclj-startup-message))

(defun slclj-banner-unload ()
  (setq slclj-repl-banner-function 'slclj-repl-insert-banner))

(provide 'slclj-banner)
