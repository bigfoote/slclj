;;; slclj-repl.el --- Read-Eval-Print Loop written in Emacs Lisp
;;
;; Original Author: Helmut Eller
;; Contributors: to many to mention
;; License: GNU GPL (same license as Emacs)
;;
;;; Description:
;;
;; This file implements a Lisp Listener along with some niceties like
;; a persistent history and various "shortcut" commands.  Nothing here
;; depends on comint.el; I/O is multiplexed over SLCLJ's socket.
;;
;; This used to be the default REPL for SLCLJ, but it was hard to
;; maintain.
;;
;;; Installation:
;;
;; Call slclj-setup and include 'slclj-repl as argument: 
;;
;;  (slclj-setup '(slclj-repl [others conribs ...]))
;;

;;;;; slclj-repl

(defgroup slclj-repl nil
  "The Read-Eval-Print Loop (*slclj-repl* buffer)."
  :prefix "slclj-repl-"
  :group 'slclj)

(defcustom slclj-repl-shortcut-dispatch-char ?\,
  "Character used to distinguish repl commands from lisp forms."
  :type '(character)
  :group 'slclj-repl)

(defcustom slclj-repl-only-save-lisp-buffers t
  "When T we only attempt to save lisp-mode file buffers. When
  NIL slclj will attempt to save all buffers (as per
  save-some-buffers). This applies to all ASDF related repl
  shortcuts."
  :type '(boolean)
  :group 'slclj-repl)

(defface slclj-repl-prompt-face
  (if (slclj-face-inheritance-possible-p)
      '((t (:inherit font-lock-keyword-face)))
    '((((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:weight bold))))
  "Face for the prompt in the SLCLJ REPL."
  :group 'slclj-repl)

(defface slclj-repl-output-face
  (if (slclj-face-inheritance-possible-p)
      '((t (:inherit font-lock-string-face)))
    '((((class color) (background light)) (:foreground "RosyBrown"))
      (((class color) (background dark)) (:foreground "LightSalmon"))
      (t (:slant italic))))
  "Face for Lisp output in the SLCLJ REPL."
  :group 'slclj-repl)

(defface slclj-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the SLCLJ REPL."
  :group 'slclj-repl)

(defface slclj-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the SLCLJ REPL."
  :group 'slclj-repl)

(defcustom slclj-repl-history-file "~/.slclj-history.eld"
  "File to save the persistent REPL history to."
  :type 'string
  :group 'slclj-repl)

(defcustom slclj-repl-history-size 200
  "*Maximum number of lines for persistent REPL history."
  :type 'integer
  :group 'slclj-repl)

(defcustom slclj-repl-history-file-coding-system 
  (cond ((slclj-find-coding-system 'utf-8-unix) 'utf-8-unix)
        (t slclj-net-coding-system))
  "*The coding system for the history file."
  :type 'symbol
  :group 'slclj-repl)


;; dummy defvar for compiler
(defvar slclj-repl-read-mode)

(defun slclj-reading-p ()
  "True if Lisp is currently reading input from the REPL."
  (with-current-buffer (slclj-output-buffer)
    slclj-repl-read-mode))


;;;; Stream output

(slclj-def-connection-var slclj-connection-output-buffer nil
  "The buffer for the REPL.  May be nil or a dead buffer.")

(make-variable-buffer-local
 (defvar slclj-output-start nil
   "Marker for the start of the output for the evaluation."))

(make-variable-buffer-local
 (defvar slclj-output-end nil
   "Marker for end of output. New output is inserted at this mark."))

;; dummy definitions for the compiler
(defvar slclj-repl-package-stack)
(defvar slclj-repl-directory-stack)
(defvar slclj-repl-input-start-mark)
(defvar slclj-repl-prompt-start-mark)

(defun slclj-output-buffer (&optional noprompt)
  "Return the output buffer, create it if necessary."
  (let ((buffer (slclj-connection-output-buffer)))
    (or (if (buffer-live-p buffer) buffer)
        (setf (slclj-connection-output-buffer)
              (let ((connection (slclj-connection)))
                (with-current-buffer (slclj-repl-buffer t connection)
                  (unless (eq major-mode 'slclj-repl-mode) 
                    (slclj-repl-mode))
                  (setq slclj-buffer-connection connection)
		  (setq slclj-buffer-package (slclj-lisp-package connection))
                  (slclj-reset-repl-markers)
                  (unless noprompt 
                    (slclj-repl-insert-prompt))
                  (current-buffer)))))))

(defvar slclj-repl-banner-function 'slclj-repl-insert-banner)

(defun slclj-repl-update-banner ()
  (funcall slclj-repl-banner-function)
  (goto-char (point-max))
  (slclj-mark-output-start)
  (slclj-mark-input-start)
  (slclj-repl-insert-prompt))

(defun slclj-repl-insert-banner ()
  (when (zerop (buffer-size))
    (let ((welcome (concat "; SLCLJ " (or (slclj-changelog-date)
                                          "- ChangeLog file not found"))))
      (insert welcome))))

(defun slclj-init-output-buffer (connection)
  (with-current-buffer (slclj-output-buffer t)
    (setq slclj-buffer-connection connection
          slclj-repl-directory-stack '()
          slclj-repl-package-stack '())
    (slclj-repl-update-banner)))

(defun slclj-display-output-buffer ()
  "Display the output buffer and scroll to bottom."
  (with-current-buffer (slclj-output-buffer)
    (goto-char (point-max))
    (unless (get-buffer-window (current-buffer) t)
      (display-buffer (current-buffer) t))
    (slclj-repl-show-maximum-output)))

(defmacro slclj-with-output-end-mark (&rest body)
  "Execute BODY at `slclj-output-end'.  

If point is initially at `slclj-output-end' and the buffer is visible
update window-point afterwards.  If point is initially not at
`slclj-output-end, execute body inside a `save-excursion' block."
  `(let ((body.. (lambda () ,@body))
         (updatep.. (and (eobp) (pos-visible-in-window-p))))
     (cond ((= (point) slclj-output-end)
            (let ((start.. (point)))
              (funcall body..)
              (set-marker slclj-output-end (point))
              (when (= start.. slclj-repl-input-start-mark) 
                (set-marker slclj-repl-input-start-mark (point)))))
           (t 
            (save-excursion 
              (goto-char slclj-output-end)
              (funcall body..))))
     (when updatep..
       (slclj-repl-show-maximum-output))))

(defun slclj-output-filter (process string)
  (with-current-buffer (process-buffer process)
    (when (and (plusp (length string))
               (eq (process-status slclj-buffer-connection) 'open))
      (slclj-write-string string))))

(defvar slclj-open-stream-hooks)

(defun slclj-open-stream-to-lisp (port)
  (let ((stream (open-network-stream "*lisp-output-stream*" 
                                     (slclj-with-connection-buffer ()
                                       (current-buffer))
				     slclj-lisp-host port)))
    (slclj-set-query-on-exit-flag stream)
    (set-process-filter stream 'slclj-output-filter)
    (let ((pcs (process-coding-system (slclj-current-connection))))
      (set-process-coding-system stream (car pcs) (cdr pcs)))
    (when-let (secret (slclj-secret))
      (slclj-net-send secret stream))
    (run-hook-with-args 'slclj-open-stream-hooks stream)
    stream))

(defun slclj-io-speed-test (&optional profile)
  "A simple minded benchmark for stream performance.
If a prefix argument is given, instrument the slclj package for
profiling before running the benchmark."
  (interactive "P")
  (eval-and-compile
    (require 'elp))
  (elp-reset-all)
  (elp-restore-all)
  (load "slclj.el")
  ;;(byte-compile-file "slclj-net.el" t)
  ;;(setq slclj-log-events nil)
  (setq slclj-enable-evaluate-in-emacs t)
  ;;(setq slclj-repl-enable-presentations nil)
  (when profile
    (elp-instrument-package "slclj-"))
  (kill-buffer (slclj-output-buffer))
  (switch-to-buffer (slclj-output-buffer))
  (delete-other-windows)
  (sit-for 0)
  (slclj-repl-send-string "(swank:io-speed-test 4000 1)")
  (let ((proc (slclj-inferior-process)))
    (when proc
      (display-buffer (process-buffer proc) t)
      (goto-char (point-max)))))

(defvar slclj-write-string-function 'slclj-repl-write-string)

(defun slclj-write-string (string &optional target)
  "Insert STRING in the REPL buffer or some other TARGET.
If TARGET is nil, insert STRING as regular process
output.  If TARGET is :repl-result, insert STRING as the result of the
evaluation.  Other values of TARGET map to an Emacs marker via the 
hashtable `slclj-output-target-to-marker'; output is inserted at this marker."
  (funcall slclj-write-string-function string target))

(defun slclj-repl-write-string (string &optional target)
  (case target
    ((nil) (slclj-repl-emit string))
    (:repl-result (slclj-repl-emit-result string))
    (t (slclj-emit-string string target))))

(defvar slclj-repl-popup-on-output nil
  "Display the output buffer when some output is written.
This is set to nil after displaying the buffer.")

(defmacro slclj-save-marker (marker &rest body)
  (let ((pos (gensym "pos")))
  `(let ((,pos (marker-position ,marker)))
     (prog1 (progn . ,body)
       (set-marker ,marker ,pos)))))

(put 'slclj-save-marker 'lisp-indent-function 1)

(defun slclj-repl-emit (string)
  ;; insert the string STRING in the output buffer
  (with-current-buffer (slclj-output-buffer)
    (save-excursion
      (goto-char slclj-output-end)
      (slclj-save-marker slclj-output-start
        (slclj-propertize-region '(face slclj-repl-output-face 
                                        rear-nonsticky (face))
          (insert-before-markers string)
          (when (and (= (point) slclj-repl-prompt-start-mark)
                     (not (bolp)))
            (insert-before-markers "\n")
            (set-marker slclj-output-end (1- (point)))))))
    (when slclj-repl-popup-on-output
      (setq slclj-repl-popup-on-output nil)
      (display-buffer (current-buffer)))
    (slclj-repl-show-maximum-output)))

(defun slclj-repl-emit-result (string &optional bol)
  ;; insert STRING and mark it as evaluation result
  (with-current-buffer (slclj-output-buffer)
    (save-excursion
      (slclj-save-marker slclj-output-start
        (slclj-save-marker slclj-output-end
          (goto-char slclj-repl-input-start-mark)
          (when (and bol (not (bolp))) (insert-before-markers "\n"))
          (slclj-propertize-region `(face slclj-repl-result-face
                                          rear-nonsticky (face))
            (insert-before-markers string)))))
    (slclj-repl-show-maximum-output)))

(defvar slclj-last-output-target-id 0
  "The last integer we used as a TARGET id.")

(defvar slclj-output-target-to-marker
  (make-hash-table)
  "Map from TARGET ids to Emacs markers.
The markers indicate where output should be inserted.")

(defun slclj-output-target-marker (target)
  "Return the marker where output for TARGET should be inserted."
  (case target
    ((nil)
     (with-current-buffer (slclj-output-buffer)
       slclj-output-end))
    (:repl-result
     (with-current-buffer (slclj-output-buffer)
       slclj-repl-input-start-mark))
    (t
     (gethash target slclj-output-target-to-marker))))

(defun slclj-emit-string (string target)
  "Insert STRING at target TARGET.
See `slclj-output-target-to-marker'."
  (let* ((marker (slclj-output-target-marker target))
         (buffer (and marker (marker-buffer marker))))
    (when buffer
      (with-current-buffer buffer
        (save-excursion 
          ;; Insert STRING at MARKER, then move MARKER behind
          ;; the insertion.
          (goto-char marker)
          (insert-before-markers string)
          (set-marker marker (point)))))))

(defun slclj-switch-to-output-buffer ()
  "Select the output buffer, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear."
  (interactive)
  (slclj-pop-to-buffer (slclj-output-buffer))
  (goto-char (point-max)))


;;;; REPL
;;
;; The REPL uses some markers to separate input from output.  The
;; usual configuration is as follows:
;; 
;;    ... output ...    ... result ...    prompt> ... input ...
;;    ^            ^                      ^       ^           ^
;;    output-start output-end  prompt-start       input-start point-max
;;
;; input-start is a right inserting marker, because
;; we want it to stay behind when the user inserts text.
;;
;; We maintain the following invariant:
;;
;;  output-start <= output-end <= input-start.
;;
;; This invariant is important, because we must be prepared for
;; asynchronous output and asynchronous reads.  ("Asynchronous" means,
;; triggered by Lisp and not by Emacs.)
;;
;; All output is inserted at the output-end marker.  Some care must be
;; taken when output-end and input-start are at the same position: if
;; we insert at that point, we must move the right markers.  We should
;; also not leave (window-)point in the middle of the new output.  The
;; idiom we use is a combination to slclj-save-marker,
;; insert-before-markers, and manually updating window-point
;; afterwards.
;;
;; A "synchronous" evaluation request proceeds as follows: the user
;; inserts some text between input-start and point-max and then hits
;; return.  We send that region to Lisp, move the output and input
;; makers to the line after the input and wait.  When we receive the
;; result, we insert it together with a prompt between the output-end
;; and input-start mark.  See `slclj-repl-insert-prompt'.
;;
;; It is possible that some output for such an evaluation request
;; arrives after the result.  This output is inserted before the
;; result (and before the prompt). 
;;
;; If we are in "reading" state, e.g., during a call to Y-OR-N-P,
;; there is no prompt between output-end and input-start.
;;

;; FIXME: slclj-lisp-package should be local in a REPL buffer
(slclj-def-connection-var slclj-lisp-package
    "COMMON-LISP-USER"
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(slclj-def-connection-var slclj-lisp-package-prompt-string
    "CL-USER"
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(slclj-make-variables-buffer-local
 (defvar slclj-repl-package-stack nil
   "The stack of packages visited in this repl.")

 (defvar slclj-repl-directory-stack nil
   "The stack of default directories associated with this repl.")

 (defvar slclj-repl-prompt-start-mark)
 (defvar slclj-repl-input-start-mark)
 (defvar slclj-repl-old-input-counter 0
   "Counter used to generate unique `slclj-repl-old-input' properties.
This property value must be unique to avoid having adjacent inputs be
joined together."))

(defun slclj-reset-repl-markers ()
  (dolist (markname '(slclj-output-start
                      slclj-output-end
                      slclj-repl-prompt-start-mark
                      slclj-repl-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))

;;;;; REPL mode setup

(defvar slclj-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-map)
    map))

(slclj-define-keys slclj-prefix-map
  ("\C-z" 'slclj-switch-to-output-buffer)
  ("\M-p" 'slclj-repl-set-package))

(slclj-define-keys slclj-mode-map 
  ("\C-c~" 'slclj-sync-package-and-default-directory)
  ("\C-c\C-y" 'slclj-call-defun))

(slclj-define-keys slclj-connection-list-mode-map
  ((kbd "RET") 'slclj-goto-connection)
  ([return] 'slclj-goto-connection))

(slclj-define-keys slclj-repl-mode-map
  ("\C-m" 'slclj-repl-return)
  ([return] 'slclj-repl-return)
  ("\C-j" 'slclj-repl-newline-and-indent)
  ("\C-\M-m" 'slclj-repl-closing-return)
  ([(control return)] 'slclj-repl-closing-return)
  ("\C-a" 'slclj-repl-bol)
  ([home] 'slclj-repl-bol)
  ("\M-p" 'slclj-repl-previous-input)
  ((kbd "C-<up>") 'slclj-repl-backward-input)
  ("\M-n" 'slclj-repl-next-input)
  ((kbd "C-<down>") 'slclj-repl-forward-input)
  ("\M-r" 'slclj-repl-previous-matching-input)
  ("\M-s" 'slclj-repl-next-matching-input)
  ("\C-c\C-c" 'slclj-interrupt)
  ;("\t"   'slclj-complete-symbol)
  ("\t"   'slclj-indent-and-complete-symbol)
  ("\M-\t" 'slclj-complete-symbol)
  (" "    'slclj-space)
  ("\C-c\C-o" 'slclj-repl-clear-output)
  ("\C-c\M-o" 'slclj-repl-clear-buffer)
  ("\C-c\C-u" 'slclj-repl-kill-input)
  ("\C-c\C-n" 'slclj-repl-next-prompt)
  ("\C-c\C-p" 'slclj-repl-previous-prompt)
  ("\C-c\C-z" 'slclj-nop))

(slclj-define-keys slclj-inspector-mode-map
  ((kbd "M-RET") 'slclj-inspector-copy-down-to-repl))

(slclj-define-keys sldb-mode-map
  ("\C-y" 'sldb-insert-frame-call-to-repl))

(def-slclj-selector-method ?r
  "SLCLJ Read-Eval-Print-Loop."
  (slclj-output-buffer))

(define-minor-mode slclj-repl-map-mode
  "Minor mode which makes slclj-repl-mode-map available.
\\{slclj-repl-mode-map}"
  nil
  nil
  slclj-repl-mode-map)

(defun slclj-repl-mode () 
  "Major mode for interacting with a superior Lisp.
\\{slclj-repl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'slclj-repl-mode)
  (slclj-editing-mode 1)
  (slclj-repl-map-mode 1)
  (lisp-mode-variables t)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
  (setq font-lock-defaults nil)
  (setq mode-name "REPL")
  (setq slclj-current-thread :repl-thread)
  (set (make-local-variable 'scroll-conservatively) 20)
  (set (make-local-variable 'scroll-margin) 0)
  (when slclj-repl-history-file
    (slclj-repl-safe-load-history)
    (slclj-add-local-hook 'kill-buffer-hook 
                          'slclj-repl-safe-save-merged-history))
  (add-hook 'kill-emacs-hook 'slclj-repl-save-all-histories)
  (slclj-setup-command-hooks)
  ;; At the REPL, we define beginning-of-defun and end-of-defun to be
  ;; the start of the previous prompt or next prompt respectively.
  ;; Notice the interplay with SLCLJ-REPL-BEGINNING-OF-DEFUN.
  (set (make-local-variable 'beginning-of-defun-function) 
       'slclj-repl-mode-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 
       'slclj-repl-mode-end-of-defun)
  (slclj-run-mode-hooks 'slclj-repl-mode-hook))

(defun slclj-repl-buffer (&optional create connection)
  "Get the REPL buffer for the current connection; optionally create."
  (funcall (if create #'get-buffer-create #'get-buffer)
           (format "*slclj-repl %s*" (slclj-connection-name connection))))

(defun slclj-repl ()
  (interactive)
  (slclj-switch-to-output-buffer))

(defun slclj-repl-mode-beginning-of-defun ()
  (slclj-repl-previous-prompt)
  t)

(defun slclj-repl-mode-end-of-defun ()
  (slclj-repl-next-prompt)
  t)

(defun slclj-repl-send-string (string &optional command-string)
  (cond (slclj-repl-read-mode
         (slclj-repl-return-string string))
        (t (slclj-repl-eval-string string))))

(defun slclj-repl-eval-string (string)
  (slclj-rex ()
      ((list 'swank:listener-eval string) (slclj-lisp-package))
    ((:ok result)
     (slclj-repl-insert-result result))
    ((:abort)
     (slclj-repl-show-abort))))

(defun slclj-repl-insert-result (result)
  (with-current-buffer (slclj-output-buffer)
    (save-excursion
      (when result
        (destructure-case result
          ((:values &rest strings)
           (cond ((null strings)
                  (slclj-repl-emit-result "; No value\n" t))
                 (t
                  (dolist (s strings)
                    (slclj-repl-emit-result s t)))))))
      (slclj-repl-insert-prompt))
    (slclj-repl-show-maximum-output)))

(defun slclj-repl-show-abort ()
  (with-current-buffer (slclj-output-buffer)
    (save-excursion
      (slclj-save-marker slclj-output-start
        (slclj-save-marker slclj-output-end
          (goto-char slclj-output-end)
          (insert-before-markers "; Evaluation aborted.\n")
          (slclj-repl-insert-prompt))))
    (slclj-repl-show-maximum-output)))

(defun slclj-repl-insert-prompt ()
  "Insert the prompt (before markers!).
Set point after the prompt.  
Return the position of the prompt beginning."
  (goto-char slclj-repl-input-start-mark)
  (slclj-save-marker slclj-output-start
    (slclj-save-marker slclj-output-end
      (unless (bolp) (insert-before-markers "\n"))
      (let ((prompt-start (point))
            (prompt (format "%s> " (slclj-lisp-package-prompt-string))))
        (slclj-propertize-region
            '(face slclj-repl-prompt-face read-only t intangible t
                   slclj-repl-prompt t
                   ;; emacs stuff
                   rear-nonsticky (slclj-repl-prompt read-only face intangible)
                   ;; xemacs stuff
                   start-open t end-open t)
          (insert-before-markers prompt))
        (set-marker slclj-repl-prompt-start-mark prompt-start)
        prompt-start))))

(defun slclj-repl-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max)) 
          (recenter -1))))))

(defvar slclj-repl-current-input-hooks)

(defun slclj-repl-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer."
  (or (run-hook-with-args-until-success 'slclj-repl-current-input-hooks 
                                        until-point-p)
      (buffer-substring-no-properties slclj-repl-input-start-mark 
                                      (if until-point-p 
                                          (point) 
                                        (point-max)))))

(defun slclj-property-position (text-property &optional object)
  "Return the first position of TEXT-PROPERTY, or nil."
  (if (get-text-property 0 text-property object)
      0
    (next-single-property-change 0 text-property object)))
  
(defun slclj-mark-input-start ()
  (set-marker slclj-repl-input-start-mark (point) (current-buffer)))

(defun slclj-mark-output-start ()
  (set-marker slclj-output-start (point))
  (set-marker slclj-output-end (point)))

(defun slclj-mark-output-end ()
  ;; Don't put slclj-repl-output-face again; it would remove the
  ;; special presentation face, for instance in the SBCL inspector.
  (add-text-properties slclj-output-start slclj-output-end
                       '(;;face slclj-repl-output-face 
                         rear-nonsticky (face))))

(defun slclj-repl-bol ()
  "Go to the beginning of line or the prompt."
  (interactive)
  (cond ((and (>= (point) slclj-repl-input-start-mark)
              (slclj-same-line-p (point) slclj-repl-input-start-mark))
         (goto-char slclj-repl-input-start-mark))
        (t (beginning-of-line 1)))
  (slclj-preserve-zmacs-region))

(defun slclj-preserve-zmacs-region ()
  "In XEmacs, ensure that the zmacs-region stays active after this command."
  (when (boundp 'zmacs-region-stays)
    (set 'zmacs-region-stays t)))

(defun slclj-repl-in-input-area-p ()
   (<= slclj-repl-input-start-mark (point)))

(defun slclj-repl-at-prompt-start-p ()
  ;; This will not work on non-current prompts.
  (= (point) slclj-repl-input-start-mark))

(defun slclj-repl-beginning-of-defun ()
  "Move to beginning of defun."
  (interactive)
  ;; We call BEGINNING-OF-DEFUN if we're at the start of a prompt
  ;; already, to trigger SLCLJ-REPL-MODE-BEGINNING-OF-DEFUN by means
  ;; of the locally bound BEGINNING-OF-DEFUN-FUNCTION, in order to
  ;; jump to the start of the previous prompt.
  (if (and (not (slclj-repl-at-prompt-start-p))
           (slclj-repl-in-input-area-p))
      (goto-char slclj-repl-input-start-mark)
    (beginning-of-defun))
  t)

;; FIXME: this looks very strange
(defun slclj-repl-end-of-defun ()
  "Move to next of defun."
  (interactive)
  ;; C.f. SLCLJ-REPL-BEGINNING-OF-DEFUN.
  (if (and (not (= (point) (point-max))) 
           (slclj-repl-in-input-area-p))
      (goto-char (point-max))
    (end-of-defun))
  t)

(defun slclj-repl-previous-prompt ()
  "Move backward to the previous prompt."
  (interactive)
  (slclj-repl-find-prompt t))

(defun slclj-repl-next-prompt ()
  "Move forward to the next prompt."
  (interactive)
  (slclj-repl-find-prompt))
 
(defun slclj-repl-find-prompt (&optional backward)
  (let ((origin (point))
        (prop 'slclj-repl-prompt))
    (while (progn 
             (slclj-search-property-change prop backward)
             (not (or (slclj-end-of-proprange-p prop) (bobp) (eobp)))))
    (unless (slclj-end-of-proprange-p prop)
      (goto-char origin))))

(defun slclj-search-property-change (prop &optional backward)
  (cond (backward 
         (goto-char (previous-single-char-property-change (point) prop)))
        (t 
         (goto-char (next-single-char-property-change (point) prop)))))

(defun slclj-end-of-proprange-p (property)
  (and (get-char-property (max 1 (1- (point))) property)
       (not (get-char-property (point) property))))

(defvar slclj-repl-return-hooks)

(defun slclj-repl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.  
Send the current input only if a whole expression has been entered,
i.e. the parenthesis are matched. 

With prefix argument send the input even if the parenthesis are not
balanced."
  (interactive "P")
  (slclj-check-connected)
  (cond (end-of-input
         (slclj-repl-send-input))
        (slclj-repl-read-mode ; bad style?
         (slclj-repl-send-input t))
        ((and (get-text-property (point) 'slclj-repl-old-input)
              (< (point) slclj-repl-input-start-mark))
         (slclj-repl-grab-old-input end-of-input)
         (slclj-repl-recenter-if-needed))
        ((run-hook-with-args-until-success 'slclj-repl-return-hooks))
        ((slclj-input-complete-p slclj-repl-input-start-mark (point-max))
         (slclj-repl-send-input t))
        (t 
         (slclj-repl-newline-and-indent)
         (message "[input not complete]"))))

(defun slclj-repl-recenter-if-needed ()
  "Make sure that (point) is visible."
  (unless (pos-visible-in-window-p (point-max))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun slclj-repl-send-input (&optional newline)
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
    (slclj-repl-send-string input)))

(defun slclj-repl-grab-old-input (replace)
  "Resend the old REPL input at point.  
If replace is non-nil the current input is replaced with the old
input; otherwise the new input is appended.  The old input has the
text property `slclj-repl-old-input'."
  (multiple-value-bind (beg end) (slclj-property-bounds 'slclj-repl-old-input)
    (let ((old-input (buffer-substring beg end)) ;;preserve
          ;;properties, they will be removed later
          (offset (- (point) beg)))
      ;; Append the old input or replace the current input
      (cond (replace (goto-char slclj-repl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (save-excursion 
        (insert old-input)
        (when (equal (char-before) ?\n) 
          (delete-char -1)))
      (forward-char offset))))

(defun slclj-repl-closing-return ()
  "Evaluate the current input string after closing all open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region slclj-repl-input-start-mark (point))
    (while (ignore-errors (save-excursion (backward-up-list 1)) t)
      (insert ")")))
  (slclj-repl-return))

(defun slclj-repl-newline-and-indent ()
  "Insert a newline, then indent the next line.
Restrict the buffer from the prompt for indentation, to avoid being
confused by strange characters (like unmatched quotes) appearing
earlier in the buffer."
  (interactive)
  (save-restriction
    (narrow-to-region slclj-repl-prompt-start-mark (point-max))
    (insert "\n")
    (lisp-indent-line)))

(defun slclj-repl-delete-current-input ()
  "Delete all text from the prompt."
  (interactive)
  (delete-region slclj-repl-input-start-mark (point-max)))

(defun slclj-repl-kill-input ()
  "Kill all text from the prompt to point."
  (interactive)
  (cond ((< (marker-position slclj-repl-input-start-mark) (point))
         (kill-region slclj-repl-input-start-mark (point)))
        ((= (point) (marker-position slclj-repl-input-start-mark))
         (slclj-repl-delete-current-input))))

(defun slclj-repl-replace-input (string)
  (slclj-repl-delete-current-input)
  (insert-and-inherit string))

(defun slclj-repl-input-line-beginning-position ()
  (save-excursion
    (goto-char slclj-repl-input-start-mark)
    (line-beginning-position)))

(defvar slclj-repl-clear-buffer-hook)

(defun slclj-repl-clear-buffer ()
  "Delete the output generated by the Lisp process."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) slclj-repl-prompt-start-mark)
    (delete-region slclj-output-start slclj-output-end)
    (when (< (point) slclj-repl-input-start-mark)
      (goto-char slclj-repl-input-start-mark))
    (recenter t))
  (run-hooks 'slclj-repl-clear-buffer-hook))

(defun slclj-repl-clear-output ()
  "Delete the output inserted since the last input."
  (interactive)
  (let ((start (save-excursion 
                 (slclj-repl-previous-prompt)
                 (ignore-errors (forward-sexp))
                 (forward-line)
                 (point)))
        (end (1- (slclj-repl-input-line-beginning-position))))
    (when (< start end)
      (let ((inhibit-read-only t))
        (delete-region start end)
        (save-excursion
          (goto-char start)
          (insert ";;; output flushed"))))))

(defun slclj-repl-set-package (package)
  "Set the package of the REPL buffer to PACKAGE."
  (interactive (list (let* ((p (slclj-current-package))
                            (p (and p (slclj-pretty-package-name p)))
                            (p (and (not (equal p (slclj-lisp-package))) p)))
                       (slclj-read-package-name "Package: " p))))
  (with-current-buffer (slclj-output-buffer)
    (let ((previouse-point (- (point) slclj-repl-input-start-mark)))
      (destructuring-bind (name prompt-string)
          (slclj-repl-shortcut-eval `(swank:set-package ,package))
        (setf (slclj-lisp-package) name)
        (setf (slclj-lisp-package-prompt-string) prompt-string)
        (setf slclj-buffer-package name)
        (slclj-repl-insert-prompt)
        (when (plusp previouse-point)
          (goto-char (+ previouse-point slclj-repl-input-start-mark)))))))


;;;;; History

(defcustom slclj-repl-wrap-history nil
  "*T to wrap history around when the end is reached."
  :type 'boolean
  :group 'slclj-repl)

(defcustom slclj-repl-history-remove-duplicates nil
  "*When T all duplicates are removed except the last one."
  :type 'boolean
  :group 'slclj-repl)

(defcustom slclj-repl-history-trim-whitespaces nil
  "*When T strip all whitespaces from the beginning and end."
  :type 'boolean
  :group 'slclj-repl)

(make-variable-buffer-local
 (defvar slclj-repl-input-history '()
   "History list of strings read from the REPL buffer."))

(defun slclj-string-trim (character-bag string)
  (flet ((find-bound (&optional from-end)
           (position-if-not (lambda (char) (memq char character-bag))
                            string :from-end from-end)))
    (let ((start (find-bound))
          (end (find-bound t)))
      (if start
          (subseq string start (1+ end))
          ""))))

(defun slclj-repl-add-to-input-history (string)
  "Add STRING to the input history.
Empty strings and duplicates are ignored."
  (when slclj-repl-history-trim-whitespaces
    (setq string (slclj-string-trim '(?\n ?\ ?\t) string)))
  (unless (equal string "")
    (when slclj-repl-history-remove-duplicates
      (setq slclj-repl-input-history
            (remove string slclj-repl-input-history)))
    (unless (equal string (car slclj-repl-input-history))
      (push string slclj-repl-input-history))))

;; These two vars contain the state of the last history search.  We
;; only use them if `last-command' was 'slclj-repl-history-replace,
;; otherwise we reinitialize them.

(defvar slclj-repl-input-history-position -1
  "Newer items have smaller indices.")

(defvar slclj-repl-history-pattern nil
  "The regexp most recently used for finding input history.")

(defun slclj-repl-history-replace (direction &optional regexp)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list).
If REGEXP is non-nil, only lines matching REGEXP are considered."
  (setq slclj-repl-history-pattern regexp)
  (let* ((min-pos -1)
         (max-pos (length slclj-repl-input-history))
         (pos0 (cond ((slclj-repl-history-search-in-progress-p)
                      slclj-repl-input-history-position)
                     (t min-pos)))
         (pos (slclj-repl-position-in-history pos0 direction (or regexp "")
                                              (slclj-repl-current-input)))
         (msg nil))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (slclj-repl-replace-input (nth pos slclj-repl-input-history))
           (setq msg (format "History item: %d" pos)))
          ((not slclj-repl-wrap-history)
           (setq msg (cond ((= pos min-pos) "End of history")
                           ((= pos max-pos) "Beginning of history"))))
          (slclj-repl-wrap-history
           (setq pos (if (= pos min-pos) max-pos min-pos))
           (setq msg "Wrapped history")))
    (when (or (<= pos min-pos) (<= max-pos pos))
      (when regexp
        (setq msg (concat msg "; no matching item"))))
    ;;(message "%s [%d %d %s]" msg start-pos pos regexp)
    (message "%s%s" msg (cond ((not regexp) "")
                              (t (format "; current regexp: %s" regexp))))
    (setq slclj-repl-input-history-position pos)
    (setq this-command 'slclj-repl-history-replace)))

(defun slclj-repl-history-search-in-progress-p ()
  (eq last-command 'slclj-repl-history-replace))

(defun slclj-repl-terminate-history-search ()
  (setq last-command this-command))

(defun slclj-repl-position-in-history (start-pos direction regexp
                                       &optional exclude-string)
  "Return the position of the history item matching REGEXP.
Return -1 resp. the length of the history if no item matches.
If EXCLUDE-STRING is specified then it's excluded from the search."
  ;; Loop through the history list looking for a matching line
  (let* ((step (ecase direction
                 (forward -1)
                 (backward 1)))
         (history slclj-repl-input-history)
         (len (length history)))
    (loop for pos = (+ start-pos step) then (+ pos step)
          if (< pos 0) return -1
          if (<= len pos) return len
          for history-item = (nth pos history)
          if (and (string-match regexp history-item)
                  (not (equal history-item exclude-string)))
          return pos)))

(defun slclj-repl-previous-input ()
  "Cycle backwards through input history.
If the `last-command' was a history navigation command use the
same search pattern for this command.
Otherwise use the current input as search pattern."
  (interactive)
  (slclj-repl-history-replace 'backward (slclj-repl-history-pattern t)))

(defun slclj-repl-next-input ()
  "Cycle forwards through input history.
See `slclj-repl-previous-input'."
  (interactive)
  (slclj-repl-history-replace 'forward (slclj-repl-history-pattern t)))

(defun slclj-repl-forward-input ()
  "Cycle forwards through input history."
  (interactive)
  (slclj-repl-history-replace 'forward (slclj-repl-history-pattern)))

(defun slclj-repl-backward-input ()
  "Cycle backwards through input history."
  (interactive)
  (slclj-repl-history-replace 'backward (slclj-repl-history-pattern)))

(defun slclj-repl-previous-matching-input (regexp)
  (interactive (list (slclj-read-from-minibuffer
		      "Previous element matching (regexp): ")))
  (slclj-repl-terminate-history-search)
  (slclj-repl-history-replace 'backward regexp))

(defun slclj-repl-next-matching-input (regexp)
  (interactive (list (slclj-read-from-minibuffer
		      "Next element matching (regexp): ")))
  (slclj-repl-terminate-history-search)
  (slclj-repl-history-replace 'forward regexp))

(defun slclj-repl-history-pattern (&optional use-current-input)
  "Return the regexp for the navigation commands."
  (cond ((slclj-repl-history-search-in-progress-p)
         slclj-repl-history-pattern)
        (use-current-input
         (assert (<= slclj-repl-input-start-mark (point)))
         (let ((str (slclj-repl-current-input t)))
           (cond ((string-match "^[ \n]*$" str) nil)
                 (t (concat "^" (regexp-quote str))))))
        (t nil)))

(defun slclj-repl-delete-from-input-history (string)
  "Delete STRING from the repl input history. 

When string is not provided then clear the current repl input and
use it as an input.  This is useful to get rid of unwanted repl
history entries while navigating the repl history."
  (interactive (list (slclj-repl-current-input)))
  (let ((merged-history 
         (slclj-repl-merge-histories slclj-repl-input-history
                                     (slclj-repl-read-history nil t))))
    (setq slclj-repl-input-history
          (delete* string merged-history :test #'string=))
    (slclj-repl-save-history))
  (slclj-repl-delete-current-input))

;;;;; Persistent History 

(defun slclj-repl-merge-histories (old-hist new-hist)
  "Merge entries from OLD-HIST and NEW-HIST."
  ;; Newer items in each list are at the beginning.
  (let* ((ht (make-hash-table :test #'equal))
         (test (lambda (entry)
                 (or (gethash entry ht)
                     (progn (setf (gethash entry ht) t)
                            nil)))))
    (append (remove-if test new-hist)
            (remove-if test old-hist))))

(defun slclj-repl-load-history (&optional filename)
  "Set the current SLCLJ REPL history.
It can be read either from FILENAME or `slclj-repl-history-file' or
from a user defined filename."
  (interactive (list (slclj-repl-read-history-filename)))
  (let ((file (or filename slclj-repl-history-file)))
    (setq slclj-repl-input-history (slclj-repl-read-history file t))))

(defun slclj-repl-read-history (&optional filename noerrer)
  "Read and return the history from FILENAME.  
The default value for FILENAME is `slclj-repl-history-file'.
If NOERROR is true return and the file doesn't exits return nil."
  (let ((file (or filename slclj-repl-history-file)))
    (cond ((not (file-readable-p file)) '())
          (t (with-temp-buffer
               (insert-file-contents file)
               (read (current-buffer)))))))

(defun slclj-repl-read-history-filename ()
  (read-file-name "Use SLCLJ REPL history from file: " 
                  slclj-repl-history-file))

(defun slclj-repl-save-merged-history (&optional filename)
  "Read the history file, merge the current REPL history and save it.
This tries to be smart in merging the history from the file and the
current history in that it tries to detect the unique entries using
`slclj-repl-merge-histories'."
  (interactive (list (slclj-repl-read-history-filename)))
  (let ((file (or filename slclj-repl-history-file)))
    (with-temp-message "saving history..."
      (let ((hist (slclj-repl-merge-histories (slclj-repl-read-history file t)
                                              slclj-repl-input-history)))
        (slclj-repl-save-history file hist)))))

(defun slclj-repl-save-history (&optional filename history)
  "Simply save the current SLCLJ REPL history to a file.
When SLCLJ is setup to always load the old history and one uses only
one instance of slclj all the time, there is no need to merge the
files and this function is sufficient.

When the list is longer than `slclj-repl-history-size' it will be
truncated.  That part is untested, though!"
  (interactive (list (slclj-repl-read-history-filename)))
  (let ((file (or filename slclj-repl-history-file))
        (hist (or history slclj-repl-input-history)))
    (unless (file-writable-p file)
      (error (format "History file not writable: %s" file)))
    (let ((hist (subseq hist 0 (min (length hist) slclj-repl-history-size))))
      ;;(message "saving %s to %s\n" hist file)
      (with-temp-file file
        (let ((cs slclj-repl-history-file-coding-system)
              (print-length nil) (print-level nil))
          (setq buffer-file-coding-system cs)
          (insert (format ";; -*- coding: %s -*-\n" cs))
          (insert ";; History for SLCLJ REPL. Automatically written.\n"
                  ";; Edit only if you know what you're doing\n")
          (prin1 (mapcar #'substring-no-properties hist) (current-buffer)))))))

(defun slclj-repl-save-all-histories ()
  "Save the history in each repl buffer."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq major-mode 'slclj-repl-mode)
        (slclj-repl-safe-save-merged-history)))))

(defun slclj-repl-safe-save-merged-history ()
  (slclj-repl-call-with-handler 
   #'slclj-repl-save-merged-history
   "%S while saving the history. Continue? "))

(defun slclj-repl-safe-load-history ()
  (slclj-repl-call-with-handler 
   #'slclj-repl-load-history
   "%S while loading the history. Continue? "))

(defun slclj-repl-call-with-handler (fun query)
  "Call FUN in the context of an error handler.
The handler will use qeuery to ask the use if the error should be ingored."
  (condition-case err
      (funcall fun)
    (error 
     (if (y-or-n-p (format query (error-message-string err)))
         nil
       (signal (car err) (cdr err))))))


;;;;; REPL Read Mode

(define-key slclj-repl-mode-map
  (string slclj-repl-shortcut-dispatch-char) 'slclj-handle-repl-shortcut)

(define-minor-mode slclj-repl-read-mode 
  "Mode the read input from Emacs
\\{slclj-repl-read-mode-map}"
  nil
  "[read]"
  '(("\C-m" . slclj-repl-return)
    ([return] . slclj-repl-return)
    ("\C-c\C-b" . slclj-repl-read-break)
    ("\C-c\C-c" . slclj-repl-read-break)))

(make-variable-buffer-local
 (defvar slclj-read-string-threads nil))

(make-variable-buffer-local
 (defvar slclj-read-string-tags nil))

(defun slclj-repl-read-string (thread tag)
  (slclj-switch-to-output-buffer)
  (push thread slclj-read-string-threads)
  (push tag slclj-read-string-tags)
  (goto-char (point-max))
  (slclj-mark-output-end)
  (slclj-mark-input-start)
  (slclj-repl-read-mode 1))

(defun slclj-repl-return-string (string)
  (slclj-dispatch-event `(:emacs-return-string 
                          ,(pop slclj-read-string-threads)
                          ,(pop slclj-read-string-tags)
                          ,string))
  (slclj-repl-read-mode -1))

(defun slclj-repl-read-break ()
  (interactive)
  (slclj-dispatch-event `(:emacs-interrupt ,(car slclj-read-string-threads))))

(defun slclj-repl-abort-read (thread tag)
  (with-current-buffer (slclj-output-buffer)
    (pop slclj-read-string-threads)
    (pop slclj-read-string-tags)
    (slclj-repl-read-mode -1)
    (message "Read aborted")))


;;;;; REPL handlers

(defstruct (slclj-repl-shortcut (:conc-name slclj-repl-shortcut.))
  symbol names handler one-liner)

(defvar slclj-repl-shortcut-table nil
  "A list of slclj-repl-shortcuts")

(defvar slclj-repl-shortcut-history '()
  "History list of shortcut command names.")

(defvar slclj-within-repl-shortcut-handler-p nil
  "Bound to T if we're in a REPL shortcut handler invoked from the REPL.")

(defun slclj-handle-repl-shortcut ()
  (interactive)
  (if (> (point) slclj-repl-input-start-mark)
      (insert (string slclj-repl-shortcut-dispatch-char))
      (let ((shortcut (slclj-lookup-shortcut
                       (completing-read "Command: " 
                                        (slclj-bogus-completion-alist
                                         (slclj-list-all-repl-shortcuts))
                                        nil t nil
                                        'slclj-repl-shortcut-history))))
        (with-struct (slclj-repl-shortcut. handler) shortcut
          (let ((slclj-within-repl-shortcut-handler-p t))
            (call-interactively handler))))))

(defun slclj-list-all-repl-shortcuts ()
  (loop for shortcut in slclj-repl-shortcut-table
        append (slclj-repl-shortcut.names shortcut)))

(defun slclj-lookup-shortcut (name)
  (find-if (lambda (s) (member name (slclj-repl-shortcut.names s)))
           slclj-repl-shortcut-table))

(defmacro defslclj-repl-shortcut (elisp-name names &rest options)
  "Define a new repl shortcut. ELISP-NAME is a symbol specifying
the name of the interactive function to create, or NIL if no
function should be created. 

NAMES is a list of \(full-name . aliases\). 

OPTIONS is an plist specifying the handler doing the actual work
of the shortcut \(`:handler'\), and a help text \(`:one-liner'\)."
  `(progn
     ,(when elisp-name
        `(defun ,elisp-name ()
           (interactive)
           (call-interactively ,(second (assoc :handler options)))))
     (let ((new-shortcut (make-slclj-repl-shortcut
                          :symbol ',elisp-name
                          :names (list ,@names)
                          ,@(apply #'append options))))
       (setq slclj-repl-shortcut-table
             (remove-if (lambda (s)
                          (member ',(car names) (slclj-repl-shortcut.names s)))
                        slclj-repl-shortcut-table))
       (push new-shortcut slclj-repl-shortcut-table)
       ',elisp-name)))

(defun slclj-repl-shortcut-eval (sexp &optional package)
  "This function should be used by REPL shortcut handlers instead
of `slclj-eval' to evaluate their final expansion. (This
expansion will be added to the REPL's history.)"
  (when slclj-within-repl-shortcut-handler-p ; were we invoked via ,foo?
    (slclj-repl-add-to-input-history (prin1-to-string sexp)))
  (slclj-eval sexp package))

(defun slclj-repl-shortcut-eval-async (sexp &optional cont package)
  "This function should be used by REPL shortcut handlers instead
of `slclj-eval-async' to evaluate their final expansion. (This
expansion will be added to the REPL's history.)"
  (when slclj-within-repl-shortcut-handler-p ; were we invoked via ,foo?
    (slclj-repl-add-to-input-history (prin1-to-string sexp)))
  (slclj-eval-async sexp cont package))


(defun slclj-list-repl-short-cuts ()
  (interactive)
  (slclj-with-popup-buffer ("*slclj-repl-help*")
    (let ((table (sort* (copy-list slclj-repl-shortcut-table) #'string<
                        :key (lambda (x) 
                               (car (slclj-repl-shortcut.names x))))))
      (dolist (shortcut table)
        (let ((names (slclj-repl-shortcut.names shortcut)))
          (insert (pop names)) ;; first print the "full" name
          (when names
            ;; we also have aliases
            (insert " (aka ")
            (while (cdr names)
              (insert (pop names) ", "))
            (insert (car names) ")"))
        (insert "\n     " (slclj-repl-shortcut.one-liner shortcut)
                "\n"))))))

(defun slclj-save-some-lisp-buffers ()
  (if slclj-repl-only-save-lisp-buffers
      (save-some-buffers nil (lambda ()
                               (and (memq major-mode slclj-lisp-modes)
                                    (not (null buffer-file-name)))))
      (save-some-buffers)))
  

(defslclj-repl-shortcut slclj-repl-shortcut-help ("help" "?")
  (:handler 'slclj-list-repl-short-cuts)
  (:one-liner "Display the help."))

(defslclj-repl-shortcut nil ("change-directory" "!d" "cd")
  (:handler 'slclj-set-default-directory)
  (:one-liner "Change the current directory."))

(defslclj-repl-shortcut nil ("pwd")
  (:handler (lambda () 
              (interactive)
              (let ((dir (slclj-eval `(swank:default-directory))))
                (message "Directory %s" dir))))
  (:one-liner "Show the current directory."))

(defslclj-repl-shortcut slclj-repl-push-directory
    ("push-directory" "+d" "pushd")
  (:handler (lambda (directory)
              (interactive
               (list (read-directory-name
                      "Push directory: "
                      (slclj-eval '(swank:default-directory))
                      nil nil "")))
              (push (slclj-eval '(swank:default-directory))
                    slclj-repl-directory-stack)
              (slclj-set-default-directory directory)))
  (:one-liner "Save the current directory and set it to a new one."))

(defslclj-repl-shortcut slclj-repl-pop-directory
    ("pop-directory" "-d" "popd")
  (:handler (lambda ()
              (interactive)
              (if (null slclj-repl-directory-stack)
                  (message "Directory stack is empty.")
                  (slclj-set-default-directory
                   (pop slclj-repl-directory-stack)))))
  (:one-liner "Restore the last saved directory."))

(defslclj-repl-shortcut nil ("change-package" "!p" "in-package" "in")
  (:handler 'slclj-repl-set-package)
  (:one-liner "Change the current package."))

(defslclj-repl-shortcut slclj-repl-push-package ("push-package" "+p")
  (:handler (lambda (package)
              (interactive (list (slclj-read-package-name "Package: ")))
              (push (slclj-lisp-package) slclj-repl-package-stack)
              (slclj-repl-set-package package)))
  (:one-liner "Save the current package and set it to a new one."))

(defslclj-repl-shortcut slclj-repl-pop-package ("pop-package" "-p")
  (:handler (lambda ()
              (interactive)
              (if (null slclj-repl-package-stack)
                  (message "Package stack is empty.")
                  (slclj-repl-set-package
                   (pop slclj-repl-package-stack)))))
  (:one-liner "Restore the last saved package."))

(defslclj-repl-shortcut slclj-repl-resend ("resend-form")
  (:handler (lambda ()
              (interactive)
              (insert (car slclj-repl-input-history))
              (insert "\n")
              (slclj-repl-send-input)))
  (:one-liner "Resend the last form."))

(defslclj-repl-shortcut slclj-repl-disconnect ("disconnect")
  (:handler 'slclj-disconnect)
  (:one-liner "Disconnect the current connection."))

(defslclj-repl-shortcut slclj-repl-disconnect-all ("disconnect-all")
  (:handler 'slclj-disconnect-all)
  (:one-liner "Disconnect all connections."))

(defslclj-repl-shortcut slclj-repl-sayoonara ("sayoonara")
  (:handler (lambda ()
              (interactive)
              (when (slclj-connected-p)
                (slclj-quit-lisp))
              (slclj-kill-all-buffers)))
  (:one-liner "Quit all Lisps and close all SLCLJ buffers."))

(defslclj-repl-shortcut slclj-repl-quit ("quit")
  (:handler (lambda ()
	      (interactive)
              ;; `slclj-quit-lisp' determines the connection to quit
              ;; on behalf of the REPL's `slclj-buffer-connection'.
              (let ((repl-buffer (slclj-output-buffer)))
                (slclj-quit-lisp)
                (kill-buffer repl-buffer))))
  (:one-liner "Quit the current Lisp."))

(defslclj-repl-shortcut slclj-repl-defparameter ("defparameter" "!")
  (:handler (lambda (name value)
              (interactive (list (slclj-read-symbol-name "Name (symbol): " t)
                                 (slclj-read-from-minibuffer "Value: " "*")))
              (insert "(cl:defparameter " name " " value 
                      " \"REPL generated global variable.\")")
              (slclj-repl-send-input t)))
  (:one-liner "Define a new global, special, variable."))

(defslclj-repl-shortcut slclj-repl-compile-and-load ("compile-and-load" "cl")
  (:handler (lambda (filename)
              (interactive (list (expand-file-name
                                  (read-file-name "File: " nil nil nil nil))))
              (slclj-save-some-lisp-buffers)
              (slclj-repl-shortcut-eval-async
               `(swank:compile-file-if-needed 
                 ,(slclj-to-lisp-filename filename) t)
               #'slclj-compilation-finished)))
  (:one-liner "Compile (if neccessary) and load a lisp file."))

(defslclj-repl-shortcut nil  ("restart-inferior-lisp")
  (:handler 'slclj-restart-inferior-lisp)
  (:one-liner "Restart *inferior-lisp* and reconnect SLCLJ."))

(defun slclj-redirect-inferior-output (&optional noerror)
  "Redirect output of the inferior-process to the REPL buffer."
  (interactive)
  (let ((proc (slclj-inferior-process)))
    (cond (proc
           (let ((filter (slclj-rcurry #'slclj-inferior-output-filter 
                                       (slclj-current-connection))))
             (set-process-filter proc filter)))
	  (noerror)
	  (t (error "No inferior lisp process")))))

(defun slclj-inferior-output-filter (proc string conn)
  (cond ((eq (process-status conn) 'closed)
         (message "Connection closed.  Removing inferior output filter.")
         (message "Lost output: %S" string)
         (set-process-filter proc nil))
        (t
         (slclj-output-filter conn string))))

(defun slclj-redirect-trace-output ()
  "Redirect the trace output to a separate Emacs buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*SLCLJ Trace Output*")))
    (with-current-buffer buffer
      (let ((marker (copy-marker (buffer-size)))
            (target (incf slclj-last-output-target-id)))
        (puthash target marker slclj-output-target-to-marker)
        (slclj-eval `(swank:redirect-trace-output ,target))))
    ;; Note: We would like the entries in
    ;; slclj-output-target-to-marker to disappear when the buffers are
    ;; killed.  We cannot just make the hash-table ":weakness 'value"
    ;; -- there is no reference from the buffers to the markers in the
    ;; buffer, so entries would disappear even though the buffers are
    ;; alive.  Best solution might be to make buffer-local variables
    ;; that keep the markers. --mkoeppe
    (pop-to-buffer buffer)))

(defun slclj-call-defun ()
  "Insert a call to the toplevel form defined around point into the REPL."
  (interactive)
  (flet ((insert-call (symbol &key (function t)
                              defclass)
           (let* ((qualified-symbol-name (slclj-qualify-cl-symbol-name symbol))
                  (symbol-name (slclj-cl-symbol-name qualified-symbol-name))
                  (symbol-package (slclj-cl-symbol-package qualified-symbol-name))
                  (call (if (equalp (slclj-lisp-package) symbol-package)
                            symbol-name
                            qualified-symbol-name)))
             (slclj-switch-to-output-buffer)
             (goto-char slclj-repl-input-start-mark)
             (insert (if function
                         "("
                         " "))
             (if defclass
                 (insert "make-instance '"))
             (insert call)
             (when function
               (insert " ")
               (save-excursion (insert ")")))
             (unless function
               (goto-char slclj-repl-input-start-mark)))))           
    (let ((toplevel (slclj-parse-toplevel-form)))
      (if (symbolp toplevel)
          (error "Not in a function definition")
          (destructure-case toplevel
            (((:defun :defgeneric :defmacro :define-compiler-macro) symbol)
             (insert-call symbol))
            ((:defmethod symbol &rest args)
             (declare (ignore args))
             (insert-call symbol))
            (((:defparameter :defvar :defconstant) symbol)
             (insert-call symbol :function nil))
            (((:defclass) symbol)
             (insert-call symbol :defclass t))
            (t
             (error "Not in a function definition")))))))

(defun slclj-inspector-copy-down-to-repl (number)
   "Evaluate the inspector slot at point via the REPL (to set `*')."
   (interactive (list (or (get-text-property (point) 'slclj-part-number)
                          (error "No part at point"))))
   (slclj-repl-send-string (format "%s" `(swank:inspector-nth-part ,number)))
   (slclj-repl))

(defun sldb-insert-frame-call-to-repl ()
  "Insert a call to a frame at point."
  (interactive)
  (let ((call (slclj-eval `(swank-backend::frame-call
                            ,(sldb-frame-number-at-point)))))
    (slclj-switch-to-output-buffer)
    (if (>= (point) slclj-repl-prompt-start-mark)
        (insert call)
	(save-excursion
	  (goto-char (point-max))
	  (insert call))))
  (slclj-repl))

(defun slclj-set-default-directory (directory)
  "Make DIRECTORY become Lisp's current directory."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (let ((dir (expand-file-name directory)))
    (message "default-directory: %s"
             (slclj-from-lisp-filename
              (slclj-repl-shortcut-eval `(swank:set-default-directory
                                          ,(slclj-to-lisp-filename dir)))))
    (with-current-buffer (slclj-output-buffer)
      (setq default-directory dir))))

(defun slclj-sync-package-and-default-directory ()
  "Set Lisp's package and directory to the values in current buffer."
  (interactive)
  (let* ((package (slclj-current-package))
         (exists-p (or (null package)
                       (slclj-eval `(cl:packagep (swank::guess-package ,package)))))
         (directory default-directory))
    (when (and package exists-p)
      (slclj-repl-set-package package))
    (slclj-set-default-directory directory)
    ;; Sync *inferior-lisp* dir
    (let* ((proc (slclj-process))
           (buffer (and proc (process-buffer proc))))
      (when buffer
        (with-current-buffer buffer
          (setq default-directory directory))))
    (message "package: %s%s  directory: %s"
             (with-current-buffer (slclj-output-buffer)
               (slclj-lisp-package))
             (if exists-p "" (format " (package %s doesn't exist)" package))
             directory)))

(defun slclj-goto-connection ()
  "Switch to the REPL buffer for the connection at point."
  (interactive)
  (let ((slclj-dispatching-connection (slclj-connection-at-point)))
    (switch-to-buffer (slclj-output-buffer))))

(defvar slclj-repl-easy-menu
  (let ((C '(slclj-connected-p)))
    `("REPL"
      [ "Send Input"             slclj-repl-return ,C ]
      [ "Close and Send Input "  slclj-repl-closing-return ,C ]
      [ "Interrupt Lisp process" slclj-interrupt ,C ]
      "--"
      [ "Previous Input"         slclj-repl-previous-input t ]
      [ "Next Input"             slclj-repl-next-input t ]
      [ "Goto Previous Prompt "  slclj-repl-previous-prompt t ]
      [ "Goto Next Prompt "      slclj-repl-next-prompt t ]
      [ "Clear Last Output"      slclj-repl-clear-output t ]
      [ "Clear Buffer "          slclj-repl-clear-buffer t ]
      [ "Kill Current Input"     slclj-repl-kill-input t ])))

(defun slclj-repl-add-easy-menu ()
  (easy-menu-define menubar-slclj-repl slclj-repl-mode-map
    "REPL" slclj-repl-easy-menu)
  (easy-menu-define menubar-slclj slclj-repl-mode-map 
    "SLCLJ" slclj-easy-menu)
  (easy-menu-add slclj-repl-easy-menu 'slclj-repl-mode-map))

(add-hook 'slclj-repl-mode-hook 'slclj-repl-add-easy-menu)

(defun slclj-hide-inferior-lisp-buffer ()
  "Display the REPL buffer instead of the *inferior-lisp* buffer."
  (let* ((buffer (if (slclj-process) 
                     (process-buffer (slclj-process))))
         (window (if buffer (get-buffer-window buffer t)))
         (repl-buffer (slclj-output-buffer t))
         (repl-window (get-buffer-window repl-buffer)))
    (when buffer
      (bury-buffer buffer))
    (cond (repl-window
           (when window
             (delete-window window)))
          (window
           (set-window-buffer window repl-buffer))
          (t
           (pop-to-buffer repl-buffer)
           (goto-char (point-max))))))

(defun slclj-repl-connected-hook-function ()
  (destructuring-bind (package prompt) 
      (let ((slclj-current-thread t))
	(slclj-eval '(swank:create-repl nil)))
    (setf (slclj-lisp-package) package)
    (setf (slclj-lisp-package-prompt-string) prompt))
  (slclj-hide-inferior-lisp-buffer)
  (slclj-init-output-buffer (slclj-connection)))

(defun slclj-repl-event-hook-function (event)
  (destructure-case event
    ((:write-string output &optional target)
     (slclj-write-string output target)
     t)
    ((:read-string thread tag)
     (assert thread)
     (slclj-repl-read-string thread tag)
     t)
    ((:read-aborted thread tag)
     (slclj-repl-abort-read thread tag)
     t)
    ((:open-dedicated-output-stream port)
     (slclj-open-stream-to-lisp port)
     t)
    ((:new-package package prompt-string)
     (setf (slclj-lisp-package) package)
     (setf (slclj-lisp-package-prompt-string) prompt-string)
     (let ((buffer (slclj-connection-output-buffer)))
       (when (buffer-live-p buffer)
	 (with-current-buffer buffer
	   (setq slclj-buffer-package package))))
     t)
    (t nil)))

(defun slclj-repl-find-buffer-package ()
  (or (slclj-search-buffer-package)
      (slclj-lisp-package)))

(defun slclj-repl-init ()
  (add-hook 'slclj-event-hooks 'slclj-repl-event-hook-function)
  (add-hook 'slclj-connected-hook 'slclj-repl-connected-hook-function)
  (setq slclj-find-buffer-package-function 'slclj-repl-find-buffer-package))

(defun slclj-repl-remove-hooks ()
  (remove-hook 'slclj-event-hooks 'slclj-repl-event-hook-function)
  (remove-hook 'slclj-connected-hook 'slclj-repl-connected-hook-function))

(def-slclj-test package-updating
    (package-name nicknames)
    "Test if slclj-lisp-package is updated."
    '(("COMMON-LISP" ("CL"))
      ("KEYWORD" ("" "KEYWORD" "||"))
      ("COMMON-LISP-USER" ("CL-USER")))
  (with-current-buffer (slclj-output-buffer)
    (let ((p (slclj-eval 
              `(swank:listener-eval 
                ,(format 
                  "(cl:setq cl:*print-case* :upcase)
                   (cl:setq cl:*package* (cl:find-package %S))
                   (cl:package-name cl:*package*)" package-name))
              (slclj-lisp-package))))
      (slclj-check ("slclj-lisp-package is %S." package-name)
        (equal (slclj-lisp-package) package-name))
      (slclj-check ("slclj-lisp-package-prompt-string is in %S." nicknames)
        (member (slclj-lisp-package-prompt-string) nicknames)))))

(defmacro with-canonicalized-slclj-repl-buffer (&rest body)
  "Evaluate BODY within a fresh REPL buffer. The REPL prompt is
canonicalized to \"SWANK\"---we do actually switch to that
package, though."
  `(let ((%old-prompt% (slclj-lisp-package-prompt-string)))
     (unwind-protect
          (progn (with-current-buffer (slclj-output-buffer)
                   (setf (slclj-lisp-package-prompt-string) "SWANK"))
                 (kill-buffer (slclj-output-buffer))
                 (with-current-buffer (slclj-output-buffer)
                   ,@body))
       (setf (slclj-lisp-package-prompt-string) %old-prompt%))))

(put 'with-canonicalized-slclj-repl-buffer 'lisp-indent-function 0)

(def-slclj-test repl-test
    (input result-contents)
    "Test simple commands in the minibuffer."
    '(("(+ 1 2)" "SWANK> (+ 1 2)
{}3
SWANK> *[]")
      ("(princ 10)" "SWANK> (princ 10)
{10
}10
SWANK> *[]")
      ("(princ 10)(princ 20)" "SWANK> (princ 10)(princ 20)
{1020
}20
SWANK> *[]")
      ("(dotimes (i 10 77) (princ i) (terpri))" 
       "SWANK> (dotimes (i 10 77) (princ i) (terpri))
{0
1
2
3
4
5
6
7
8
9
}77
SWANK> *[]")
      ("(abort)" "SWANK> (abort)
{}; Evaluation aborted.
SWANK> *[]")
      ("(progn (princ 10) (force-output) (abort))" 
       "SWANK> (progn (princ 10) (force-output) (abort))
{10}; Evaluation aborted.
SWANK> *[]")
      ("(progn (princ 10) (abort))" 
       ;; output can be flushed after aborting
       "SWANK> (progn (princ 10) (abort))
{10}; Evaluation aborted.
SWANK> *[]")
      ("(if (fresh-line) 1 0)"
       "SWANK> (if (fresh-line) 1 0)
{
}1
SWANK> *[]")
      ("(values 1 2 3)" "SWANK> (values 1 2 3)
{}1
2
3
SWANK> *[]")
      ("(with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)"
       "SWANK> (with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)
{((1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2)
 (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2))
}0
SWANK> *[]")
      ;; Two times to test the effect of FRESH-LINE.
      ("(with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)"
       "SWANK> (with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)
{((1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2)
 (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2))
}0
SWANK> *[]"))
  (with-canonicalized-slclj-repl-buffer
    (insert input)
    (slclj-check-buffer-contents "Buffer contains input" 
                                 (concat "{}SWANK> [" input "*]"))
    (call-interactively 'slclj-repl-return)
    (slclj-sync-to-top-level 5)
    (slclj-check-buffer-contents "Buffer contains result" result-contents)))

(defun slclj-check-buffer-contents (msg expected)
  (let* ((marks '((point . ?*) 
                  (slclj-output-start . ?{) (slclj-output-end . ?}) 
                  (slclj-repl-input-start-mark . ?\[) (point-max . ?\])))
         (marks (remove-if-not (lambda (m) (position (cdr m) expected))
                               marks))
         (marks (sort (copy-sequence marks) 
                      (lambda (x y)
                        (< (position (cdr x) expected)
                           (position (cdr y) expected)))))
         (content (remove-if (lambda (c) (member* c marks :key #'cdr))
                             expected))
         (marks (do ((result '() (acons (caar m) (1+ (position (cdar m) s))
                                        result))
                     (m marks (cdr m))
                     (s expected (remove* (cdar m) s)))
                    ((null m) (reverse result))))
         (point (point))
         (point-max (point-max)))
    (slclj-test-expect (concat msg " [content]") content (buffer-string))
    (macrolet ((test-mark 
                (mark)
                `(when (assoc ',mark marks)
                   (slclj-test-expect (format "%s [%s]" msg ',mark)
                                      (cdr (assoc ',mark marks))
                                      ,mark
                                      #'=))))
      (test-mark point)
      (test-mark slclj-output-end)
      (test-mark slclj-output-start)
      (test-mark slclj-repl-input-start-mark)
      (test-mark point-max))))

(def-slclj-test repl-return 
    (before after result-contents)
    "Test if slclj-repl-return sends the correct protion to Lisp even
if point is not at the end of the line."
    '(("(+ 1 2)" "" "SWANK> (+ 1 2)
3
SWANK> ")
("(+ 1 " "2)" "SWANK> (+ 1 2)
3
SWANK> ")

("(+ 1\n" "2)" "SWANK> (+ 1
2)
3
SWANK> "))
  (with-canonicalized-slclj-repl-buffer
    (insert before)
    (save-excursion (insert after))
    (slclj-test-expect "Buffer contains input" 
                       (concat "SWANK> " before after)
                       (buffer-string))
    (call-interactively 'slclj-repl-return)
    (slclj-sync-to-top-level 5)
    (slclj-test-expect "Buffer contains result" 
                       result-contents (buffer-string))))
  
(def-slclj-test repl-read
    (prompt input result-contents)
    "Test simple commands in the minibuffer."
    '(("(read-line)" "foo" "SWANK> (values (read-line))
foo
\"foo\"
SWANK> ")
      ("(read-char)" "1" "SWANK> (values (read-char))
1
#\\1
SWANK> ")
      ("(read)" "(+ 2 3
4)" "SWANK> (values (read))
\(+ 2 3
4)
\(+ 2 3 4)
SWANK> "))
  (with-canonicalized-slclj-repl-buffer
    (insert (format "(values %s)" prompt))
    (call-interactively 'slclj-repl-return)
    (slclj-wait-condition "reading" #'slclj-reading-p 5)
    (insert input)
    (call-interactively 'slclj-repl-return)
    (slclj-sync-to-top-level 5)
    (slclj-test-expect "Buffer contains result" 
                       result-contents (buffer-string))))

(def-slclj-test repl-read-lines
    (command inputs final-contents)
    "Test reading multiple lines from the repl."
    '(("(list (read-line) (read-line) (read-line))" 
       ("a" "b" "c")
       "SWANK> (list (read-line) (read-line) (read-line))
a
b
c
\(\"a\" \"b\" \"c\")
SWANK> "))
  (with-canonicalized-slclj-repl-buffer
    (insert command)
    (call-interactively 'slclj-repl-return)
    (dolist (input inputs) 
      (slclj-wait-condition "reading" #'slclj-reading-p 5)
      (insert input)
      (call-interactively 'slclj-repl-return))
    (slclj-sync-to-top-level 5)
    (slclj-test-expect "Buffer contains result"
                       final-contents 
                       (buffer-string)
                       #'equal)))

(def-slclj-test repl-type-ahead
    (command input final-contents)
    "Ensure that user input is preserved correctly.
In particular, input inserted while waiting for a result."
    '(("(sleep 0.1)" "foo*" "SWANK> (sleep 0.1)
{}NIL
SWANK> [foo*]")
      ("(sleep 0.1)" "*foo" "SWANK> (sleep 0.1)
{}NIL
SWANK> [*foo]")
      ("(progn (sleep 0.1) (abort))" "*foo" "SWANK> (progn (sleep 0.1) (abort))
{}; Evaluation aborted.
SWANK> [*foo]"))
  (with-canonicalized-slclj-repl-buffer
    (insert command)
    (call-interactively 'slclj-repl-return)
    (save-excursion (insert (delete* ?* input)))
    (forward-char (position ?* input))
    (slclj-sync-to-top-level 5)
    (slclj-check-buffer-contents "Buffer contains result" final-contents)))


(def-slclj-test interrupt-in-blocking-read
    ()
    "Let's see what happens if we interrupt a blocking read operation."
    '(())
  (slclj-check-top-level)
  (with-canonicalized-slclj-repl-buffer
    (insert "(read-char)")
    (call-interactively 'slclj-repl-return)
    (slclj-wait-condition "reading" #'slclj-reading-p 5)
    (slclj-interrupt)
    (slclj-wait-condition "Debugger visible" 
                          (lambda () 
                            (and (slclj-sldb-level= 1)
                                 (get-buffer-window (sldb-get-default-buffer))))
                          5)
    (with-current-buffer (sldb-get-default-buffer)
      (sldb-continue))
    (slclj-wait-condition "reading" #'slclj-reading-p 5)
    (with-current-buffer (slclj-output-buffer)
      (insert "X")
      (call-interactively 'slclj-repl-return)
      (slclj-sync-to-top-level 5)
      (slclj-test-expect "Buffer contains result" 
                         "SWANK> (read-char)
X
#\\X
SWANK> " (buffer-string)))))

(let ((byte-compile-warnings '()))
  (mapc #'byte-compile
	'(slclj-repl-event-hook-function
	  slclj-write-string
	  slclj-repl-write-string
	  slclj-repl-emit
	  slclj-repl-show-maximum-output)))

(provide 'slclj-repl)
