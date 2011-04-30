;;; slclj-mrepl.el --- Multiple REPLs
;;
;; An experimental implementation of multiple REPLs multiplexed over a
;; single Slclj socket.  M-x slclj-open-listener creates a new REPL
;; buffer.
;;
;; Some copy&pasting from slclj-repl.el

(require 'slclj-repl)

(slclj-define-channel-type listener)

(slclj-define-channel-method listener :prompt (package prompt)
  (with-current-buffer (slclj-channel-get self 'buffer)
    (setf slclj-buffer-package package)
    (letf (((slclj-lisp-package-prompt-string) prompt))
      (slclj-repl-insert-prompt))))

(slclj-define-channel-method listener :write-result (result)
  (letf (((slclj-connection-output-buffer) (slclj-channel-get self 'buffer)))
    (slclj-repl-emit-result result t)))

(slclj-define-channel-method listener :evaluation-aborted (package prompt)
  (with-current-buffer (slclj-channel-get self 'buffer)
    (setq slclj-buffer-package package)
    (letf (((slclj-connection-output-buffer) (current-buffer))
	   ((slclj-lisp-package-prompt-string) prompt))
      (slclj-repl-show-abort))))

(slclj-define-channel-method listener :write-string (string)
  (slclj-mrepl-write-string self string))

(defun slclj-mrepl-write-string (self string)
  (letf (((slclj-connection-output-buffer) (slclj-channel-get self 'buffer)))
    (slclj-repl-emit string)))

(byte-compile 'slclj-mrepl-write-string)

(slclj-define-channel-method listener :read-string (thread tag)
  (letf (((slclj-connection-output-buffer) (slclj-channel-get self 'buffer)))
    (slclj-repl-read-string thread tag)))

(define-derived-mode slclj-mrepl-mode slclj-repl-mode "mrepl")

(slclj-define-keys slclj-mrepl-mode-map
  ((kbd "RET") 'slclj-mrepl-return)
  ([return] 'slclj-mrepl-return))

(defun slclj-mrepl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.  
Send the current input ony if a whole expression has been entered,
i.e. the parenthesis are matched. 

With prefix argument send the input even if the parenthesis are not
balanced."
  (interactive "P")
  (slclj-check-connected)
  (cond (end-of-input
         (slclj-mrepl-send-input))
        (slclj-repl-read-mode ; bad style?
         (slclj-mrepl-send-input t))
        ((and (get-text-property (point) 'slclj-repl-old-input)
              (< (point) slclj-repl-input-start-mark))
         (slclj-repl-grab-old-input end-of-input)
         (slclj-repl-recenter-if-needed))
        ((slclj-input-complete-p slclj-repl-input-start-mark (point-max))
         (slclj-mrepl-send-input t))
        (t 
         (slclj-repl-newline-and-indent)
         (message "[input not complete]"))))

(defun slclj-mrepl-send-input (&optional newline)
  "Goto to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (slclj-repl-in-input-area-p)
    (error "No input at point."))
  (goto-char (point-max))
  (let ((end (point))) ; end of input, without the newline
    (slclj-repl-add-to-input-history 
     (buffer-substring slclj-repl-input-start-mark end))
    (when newline 
      (insert "\n")
      (slclj-repl-show-maximum-output))
    (let ((inhibit-modification-hooks t))
      (add-text-properties slclj-repl-input-start-mark 
                           (point)
                           `(slclj-repl-old-input
                             ,(incf slclj-repl-old-input-counter))))
    (let ((overlay (make-overlay slclj-repl-input-start-mark end)))
      ;; These properties are on an overlay so that they won't be taken
      ;; by kill/yank.
      (overlay-put overlay 'read-only t)
      (overlay-put overlay 'face 'slclj-repl-input-face)))
  (let ((input (slclj-repl-current-input)))
    (goto-char (point-max))
    (slclj-mark-input-start)
    (slclj-mark-output-start)
    (slclj-mrepl-send-string input)))

(defun slclj-mrepl-send-string (string &optional command-string)
  (cond (slclj-repl-read-mode
         (slclj-repl-return-string string))
        (t (slclj-mrepl-send `(:eval ,string)))))

(defun slclj-mrepl-send (msg)
  "Send MSG to the remote channel."
  (slclj-send-to-remote-channel slclj-mrepl-remote-channel msg))

(defun slclj-open-listener ()
  "Create a new listener window."
  (interactive)
  (let ((channel (slclj-make-channel slclj-listener-channel-methods)))
    (slclj-eval-async
     `(swank:create-listener ,(slclj-channel.id channel))
     (slclj-rcurry 
      (lambda (result channel)
	(destructuring-bind (remote thread-id package prompt) result
	  (pop-to-buffer (generate-new-buffer "*slclj-listener*"))
	  (slclj-mrepl-mode)
	  (setq slclj-current-thread thread-id)
	  (setq slclj-buffer-connection (slclj-connection))
	  (set (make-local-variable 'slclj-mrepl-remote-channel) remote)
	  (slclj-channel-put channel 'buffer (current-buffer))
	  (slclj-reset-repl-markers)
	  (slclj-channel-send channel `(:prompt ,package ,prompt))
	  (slclj-repl-show-maximum-output)))
      channel))))

(provide 'slclj-mrepl)
