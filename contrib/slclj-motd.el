;;; slclj-motd.el --- Message Of The Day in a slclj repl
;;
;; Authors: Marco Baringer <mb@bese.it>
;;
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation
;;
;; Add slclj-motd to your slclj-setup call.

(require 'slclj-banner)

(defcustom slclj-motd-pathname nil
  "The local pathname the motd is read from."
  :group 'slclj-mode
  :type '(file :must-match t))

(defun slclj-insert-motd ()
  (slclj-eval-async `(swank::read-motd ,slclj-motd-pathname)
                    (lambda (motd)
                      (when motd
                        (slclj-repl-insert-result (list :values motd))))))

(defun slclj-motd-init ()
  (swank:swank-require :swank-motd)
  (add-hook 'slclj-connected-hook 'slclj-insert-motd))

(provide 'slclj-motd)

