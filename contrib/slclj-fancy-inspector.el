;;; slclj-fancy-inspector.el --- Fancy inspector for CLOS objects
;;
;; Author: Marco Baringer <mb@bese.it> and others
;; License: GNU GPL (same license as Emacs)
;;

(defun slclj-fancy-inspector-init ()
  (slclj-require :swank-fancy-inspector))

(provide 'slclj-fancy-inspector)