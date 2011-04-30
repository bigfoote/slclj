;;; slclj-typeout-frame.el --- display some message in a dedicated frame
;;
;; Author: Luke Gorrie  <luke@synap.se>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (slclj-setup '(slclj-typeout-frame))


;;;; Typeout frame

;; When a "typeout frame" exists it is used to display certain
;; messages instead of the echo area or pop-up windows.

(defvar slclj-typeout-window nil
  "The current typeout window.")

(defvar slclj-typeout-frame-properties
  '((height . 10) (minibuffer . nil))
  "The typeout frame properties (passed to `make-frame').")

(defun slclj-typeout-buffer ()
  (with-current-buffer (get-buffer-create "*SLCLJ Typeout*")
    (setq buffer-read-only t)
    (current-buffer)))

(defun slclj-typeout-active-p ()
  (and slclj-typeout-window
       (window-live-p slclj-typeout-window)))

(defun slclj-typeout-message-aux (format-string &rest format-args)
  (slclj-ensure-typeout-frame)
  (with-current-buffer (slclj-typeout-buffer)
    (let ((inhibit-read-only t)
          (msg (apply #'format format-string format-args)))
      (unless (string= msg "")
	(erase-buffer)
	(insert msg)))))

(defun slclj-typeout-message (format-string &rest format-args)
  (apply #'slclj-typeout-message-aux format-string format-args))

(defun slclj-make-typeout-frame ()
  "Create a frame for displaying messages (e.g. arglists)."
  (interactive)
  (let ((frame (make-frame slclj-typeout-frame-properties)))
    (save-selected-window
      (select-window (frame-selected-window frame))
      (switch-to-buffer (slclj-typeout-buffer))
      (setq slclj-typeout-window (selected-window)))))

(defun slclj-ensure-typeout-frame ()
  "Create the typeout frame unless it already exists."
  (interactive)
  (if (slclj-typeout-active-p)
      (save-selected-window
        (select-window slclj-typeout-window)
        (switch-to-buffer (slclj-typeout-buffer)))
    (slclj-make-typeout-frame)))

(defun slclj-typeout-autodoc-message (doc)
  ;; No need for refreshing per `slclj-autodoc-pre-command-refresh-echo-area'.
  ;; FIXME: eldoc doesn't know anything about this
  (setq slclj-autodoc-last-message "")
  (slclj-typeout-message-aux "%s" doc))

(defun slclj-typeout-autodoc-dimensions ()
  (cond ((slclj-typeout-active-p)
	 (list (window-width slclj-typeout-window) nil))
	(t
	 (list 75 nil))))


;;; Initialization

(defvar slclj-typeout-frame-unbind-stack ())

(defun slclj-typeout-frame-init ()
  (unless (slclj-typeout-tty-only-p)
    (add-hook 'slclj-connected-hook 'slclj-ensure-typeout-frame)
    (loop for (var value) in 
	  '((slclj-message-function slclj-typeout-message)
	    (slclj-background-message-function slclj-typeout-message)
	    (slclj-autodoc-message-function slclj-typeout-autodoc-message)
	    (slclj-autodoc-dimensions-function
	     slclj-typeout-autodoc-dimensions))
	  do (slclj-typeout-frame-init-var var value))))

(defun slclj-typeout-frame-init-var (var value)
  (push (list var (if (boundp var) (symbol-value var) 'slclj-unbound))
	slclj-typeout-frame-unbind-stack)
  (set var value))

(defun slclj-typeout-tty-only-p ()
  (cond ((featurep 'xemacs)
	 (null (remove 'tty (mapcar #'device-type (console-device-list)))))
	(t (not (window-system)))))

(defun slclj-typeout-frame-unload ()
  (remove-hook 'slclj-connected-hook 'slclj-ensure-typeout-frame)
  (loop for (var value) in slclj-typeout-frame-unbind-stack 
	do (cond ((eq var 'slclj-unbound) (makunbound var))
		 (t (set var value))))
  (setq slclj-typeout-frame-unbind-stack nil))
  
(provide 'slclj-typeout-frame)
