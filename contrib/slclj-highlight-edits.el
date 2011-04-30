;;; slclj-higlight-edits --- highlight edited, i.e. not yet compiled, code 
;;
;; Author: William Bland <doctorbill.news@gmail.com> and others
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation: 
;; 
;; Add something like this your .emacs: 
;;
;;   (add-to-list 'load-path "<contrib-dir>")
;;   (autoload 'slclj-highlight-edits-mode "slclj-highlight-edits")
;;   (add-hook 'slclj-mode-hook (lambda () (slclj-highlight-edits-mode 1)))

(defface slclj-highlight-edits-face
    `((((class color) (background light))
       (:background "lightgray"))
      (((class color) (background dark))
       (:background "dimgray"))
      (t (:background "yellow")))
  "Face for displaying edit but not compiled code."
  :group 'slclj-mode-faces)

(define-minor-mode slclj-highlight-edits-mode 
  "Minor mode to highlight not-yet-compiled code." nil)

(add-hook 'slclj-highlight-edits-mode-on-hook
          'slclj-highlight-edits-init-buffer)

(add-hook 'slclj-highlight-edits-mode-off-hook
          'slclj-highlight-edits-reset-buffer)

(defun slclj-highlight-edits-init-buffer ()
  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions 
               'slclj-highlight-edits)
  (add-to-list 'slclj-before-compile-functions
               'slclj-highlight-edits-compile-hook))

(defun slclj-highlight-edits-reset-buffer ()
  (setq after-change-functions  
        (remove 'slclj-highlight-edits after-change-functions))
  (slclj-remove-edits (point-min) (point-max)))

;; FIXME: what's the LEN arg for?
(defun slclj-highlight-edits (beg end &optional len) 
  (save-match-data
    (when (and (slclj-connected-p)
               (not (slclj-inside-comment-p))
               (not (slclj-only-whitespace-p beg end)))
      (let ((overlay (make-overlay beg end)))
        (overlay-put overlay 'face 'slclj-highlight-edits-face)
        (overlay-put overlay 'slclj-edit t)))))

(defun slclj-remove-edits (start end)
  "Delete the existing Slclj edit hilights in the current buffer."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (dolist (o (overlays-at (point)))
        (when (overlay-get o 'slclj-edit)
          (delete-overlay o)))
      (goto-char (next-overlay-change (point))))))

(defun slclj-highlight-edits-compile-hook (start end)
  (when slclj-highlight-edits-mode
    (let ((start (save-excursion (goto-char start) 
				 (skip-chars-backward " \t\n\r")
				 (point)))
	  (end (save-excursion (goto-char end) 
			       (skip-chars-forward " \t\n\r")
			       (point))))
      (slclj-remove-edits start end))))

(defun slclj-only-whitespace-p (beg end)
  "Contains the region from BEG to END only whitespace?"
  (save-excursion
    (goto-char beg)
    (skip-chars-forward " \n\t\r" end)
    (<= end (point))))

(defun slclj-highlight-edits-mode-on () (slclj-highlight-edits-mode 1))

(defun slclj-highlight-edits-init ()
  (add-hook 'slclj-mode-hook 'slclj-highlight-edits-mode-on))

(defun slclj-highlight-edits-unload ()
  (remove-hook 'slclj-mode-hook 'slclj-highlight-edits-mode-on))

(provide 'slclj-highlight-edits)