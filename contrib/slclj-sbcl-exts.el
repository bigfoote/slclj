;;; slclj-package-fu.el --- Misc extensions for SBCL
;;
;; Author:  Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;

(require 'slclj-references)

(defun slclj-sbcl-bug-at-point ()
  (save-excursion
    (save-match-data
      (unless (looking-at "#[0-9]\\{6\\}")
        (search-backward-regexp "#\\<" (line-beginning-position) t))
      (when (looking-at "#[0-9]\\{6\\}")
        (buffer-substring-no-properties (match-beginning 0) (match-end 0))))))

(defun slclj-read-sbcl-bug (prompt &optional query)
  "Either read a sbcl bug or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil."
  (let ((bug (slclj-sbcl-bug-at-point)))
    (cond ((or current-prefix-arg query (not bug))
           (slclj-read-from-minibuffer prompt bug))
          (t bug))))

(defun slclj-visit-sbcl-bug (bug)
  "Visit the Launchpad site that describes `bug' (#nnnnnn)."
  (interactive (list (slclj-read-sbcl-bug "Bug number (#nnnnnn): ")))
  (browse-url (format "http://bugs.launchpad.net/sbcl/+bug/%s" 
                      (substring bug 1))))

(defun slclj-sbcl-exts-init ()
  (slclj-require :swank-sbcl-exts))

(provide 'slclj-sbcl-exts)