;;; slclj-fuzzy.el --- fuzzy symbol completion
;;
;; Authors: Brian Downing <bdowning@lavos.net>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          Attila Lendvai <attila.lendvai@gmail.com>
;;          and others
;;
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slclj-load-hook (lambda () (require 'slclj-fuzzy)))
;;


;;; Code

(defcustom slclj-fuzzy-completion-in-place t
  "When non-NIL the fuzzy symbol completion is done in place as
opposed to moving the point to the completion buffer."
  :group 'slclj-mode
  :type 'boolean)

(defcustom slclj-fuzzy-completion-limit 300
  "Only return and present this many symbols from swank."
  :group 'slclj-mode
  :type 'integer)

(defcustom slclj-fuzzy-completion-time-limit-in-msec 1500
  "Limit the time spent (given in msec) in swank while gathering
comletitions."
  :group 'slclj-mode
  :type 'integer)

(defcustom slclj-when-complete-filename-expand nil
  "Use comint-replace-by-expanded-filename instead of
comint-dynamic-complete-as-filename to complete file names"
  :group 'slclj-mode
  :type 'boolean)

(defvar slclj-fuzzy-target-buffer nil
  "The buffer that is the target of the completion activities.")
(defvar slclj-fuzzy-saved-window-configuration nil
  "The saved window configuration before the fuzzy completion
buffer popped up.")
(defvar slclj-fuzzy-start nil
  "The beginning of the completion slot in the target buffer.
This is a non-advancing marker.")
(defvar slclj-fuzzy-end nil
  "The end of the completion slot in the target buffer.
This is an advancing marker.")
(defvar slclj-fuzzy-original-text nil
  "The original text that was in the completion slot in the
target buffer.  This is what is put back if completion is
aborted.")
(defvar slclj-fuzzy-text nil
  "The text that is currently in the completion slot in the
target buffer.  If this ever doesn't match, the target buffer has
been modified and we abort without touching it.")
(defvar slclj-fuzzy-first nil
  "The position of the first completion in the completions buffer.
The descriptive text and headers are above this.")
(defvar slclj-fuzzy-last nil
    "The position of the last completion in the completions buffer.
If the time limit has exhausted during generation possible completion
choices inside SWANK, an indication is printed below this.")
(defvar slclj-fuzzy-current-completion nil
  "The current completion object.  If this is the same before and
after point moves in the completions buffer, the text is not
replaced in the target for efficiency.")
(defvar slclj-fuzzy-current-completion-overlay nil
  "The overlay representing the current completion in the completion
buffer. This is used to hightlight the text.")

;;;;;;; slclj-target-buffer-fuzzy-completions-mode
;; NOTE: this mode has to be able to override key mappings in slclj-mode

;; FIXME: clean this up

(defun mimic-key-bindings (from-keymap to-keymap bindings-or-operation operation)
  "Iterate on BINDINGS-OR-OPERATION. If an element is a symbol then
try to look it up (as an operation) in FROM-KEYMAP. Non symbols are taken
as default key bindings when none to be mimiced was found in FROM-KEYMAP.
Set the resulting list of keys in TO-KEYMAP to OPERATION."
  (let ((mimic-keys nil)
        (direct-keys nil))
    (dolist (key-or-operation bindings-or-operation)
      (if (symbolp key-or-operation)
          (setf mimic-keys (append mimic-keys (where-is-internal key-or-operation from-keymap nil t)))
          (push key-or-operation direct-keys)))
    (dolist (key (or mimic-keys direct-keys))
      (define-key to-keymap key operation))))

(defvar slclj-target-buffer-fuzzy-completions-map
  (let* ((map (make-sparse-keymap)))
    (flet ((remap (keys to)
             (mimic-key-bindings global-map map keys to)))
      
      (remap (list 'keyboard-quit (kbd "C-g")) 'slclj-fuzzy-abort)

      (remap (list 'slclj-fuzzy-indent-and-complete-symbol
                   'slclj-indent-and-complete-symbol
                   (kbd "<tab>"))
             'slclj-fuzzy-select-or-update-completions)
      (remap (list 'previous-line (kbd "<up>")) 'slclj-fuzzy-prev)
      (remap (list 'next-line (kbd "<down>")) 'slclj-fuzzy-next)
      (remap (list 'isearch-forward (kbd "C-s"))
             (lambda ()
               (interactive)
               (select-window (get-buffer-window (slclj-get-fuzzy-buffer)))
               (call-interactively 'isearch-forward)))

      ;; some unconditional direct bindings
      (dolist (key (list (kbd "<return>") (kbd "RET") (kbd "<SPC>") "(" ")" "[" "]"))
        (define-key map key 'slclj-fuzzy-select-and-process-event-in-target-buffer)))
    map
    )
  "Keymap for slclj-target-buffer-fuzzy-completions-mode. This will override the key
bindings in the target buffer temporarily during completion.")

;; Make sure slclj-fuzzy-target-buffer-completions-mode's map is
;; before everything else.
(setf minor-mode-map-alist
      (stable-sort minor-mode-map-alist
                   (lambda (a b)
                     (eq a 'slclj-fuzzy-target-buffer-completions-mode))
                   :key #'car))


(define-minor-mode slclj-fuzzy-target-buffer-completions-mode
  "This minor mode is intented to override key bindings during fuzzy
completions in the target buffer. Most of the bindings will do an implicit select
in the completion window and let the keypress be processed in the target buffer."
  nil
  nil
  slclj-target-buffer-fuzzy-completions-map)

(add-to-list 'minor-mode-alist
             '(slclj-fuzzy-target-buffer-completions-mode
               " Fuzzy Target Buffer Completions"))

(define-derived-mode slclj-fuzzy-completions-mode 
  fundamental-mode "Fuzzy Completions"
  "Major mode for presenting fuzzy completion results.

When you run `slclj-fuzzy-complete-symbol', the symbol token at
point is completed using the Fuzzy Completion algorithm; this
means that the token is taken as a sequence of characters and all
the various possibilities that this sequence could meaningfully
represent are offered as selectable choices, sorted by how well
they deem to be a match for the token. (For instance, the first
choice of completing on \"mvb\" would be \"multiple-value-bind\".)

Therefore, a new buffer (*Fuzzy Completions*) will pop up that
contains the different completion choices. Simultaneously, a
special minor-mode will be temporarily enabled in the original
buffer where you initiated fuzzy completion (also called the
``target buffer'') in order to navigate through the *Fuzzy
Completions* buffer without leaving.

With focus in *Fuzzy Completions*:
  Type `n' and `p' (`UP', `DOWN') to navigate between completions.
  Type `RET' or `TAB' to select the completion near point. 
  Type `q' to abort.

With focus in the target buffer:
  Type `UP' and `DOWN' to navigate between completions.
  Type a character that does not constitute a symbol name
  to insert the current choice and then that character (`(', `)',
  `SPACE', `RET'.) Use `TAB' to simply insert the current choice.
  Use C-g to abort.

Alternatively, you can click <mouse-2> on a completion to select it.


Complete listing of keybindings within the target buffer:

\\<slclj-target-buffer-fuzzy-completions-map>\
\\{slclj-target-buffer-fuzzy-completions-map}

Complete listing of keybindings with *Fuzzy Completions*:

\\<slclj-fuzzy-completions-map>\
\\{slclj-fuzzy-completions-map}"
  (use-local-map slclj-fuzzy-completions-map))

(defvar slclj-fuzzy-completions-map  
  (let* ((map (make-sparse-keymap)))
    (flet ((remap (keys to)
             (mimic-key-bindings global-map map keys to)))
      (remap (list 'keyboard-quit (kbd "C-g")) 'slclj-fuzzy-abort)
      (define-key map "q" 'slclj-fuzzy-abort)
    
      (remap (list 'previous-line (kbd "<up>")) 'slclj-fuzzy-prev)
      (remap (list 'next-line (kbd "<down>")) 'slclj-fuzzy-next)
    
      (define-key map "n" 'slclj-fuzzy-next)
      (define-key map "\M-n" 'slclj-fuzzy-next)
    
      (define-key map "p" 'slclj-fuzzy-prev)
      (define-key map "\M-p" 'slclj-fuzzy-prev)
    
      (define-key map "\d" 'scroll-down)

      (remap (list 'slclj-fuzzy-indent-and-complete-symbol
                   'slclj-indent-and-complete-symbol
                   (kbd "<tab>"))
             'slclj-fuzzy-select)

      (define-key map (kbd "<mouse-2>") 'slclj-fuzzy-select/mouse))
    
      (define-key map (kbd "RET") 'slclj-fuzzy-select)
      (define-key map (kbd "<SPC>") 'slclj-fuzzy-select)
    
    map)
  "Keymap for slclj-fuzzy-completions-mode when in the completion buffer.")

(defun slclj-fuzzy-completions (prefix &optional default-package)
  "Get the list of sorted completion objects from completing
`prefix' in `package' from the connected Lisp."
  (let ((prefix (etypecase prefix
		  (symbol (symbol-name prefix))
		  (string prefix))))
    (slclj-eval `(swank:fuzzy-completions ,prefix 
                                          ,(or default-package
                                               (slclj-current-package))
                  :limit ,slclj-fuzzy-completion-limit
                  :time-limit-in-msec ,slclj-fuzzy-completion-time-limit-in-msec))))

(defun slclj-fuzzy-selected (prefix completion)
  "Tell the connected Lisp that the user selected completion
`completion' as the completion for `prefix'."
  (let ((no-properties (copy-sequence prefix)))
    (set-text-properties 0 (length no-properties) nil no-properties)
    (slclj-eval `(swank:fuzzy-completion-selected ,no-properties 
                                                  ',completion))))

(defun slclj-fuzzy-indent-and-complete-symbol ()
  "Indent the current line and perform fuzzy symbol completion.  First
indent the line. If indenting doesn't move point, complete the
symbol. If there's no symbol at the point, show the arglist for the
most recently enclosed macro or function."
  (interactive)
  (let ((pos (point)))
    (unless (get-text-property (line-beginning-position) 'slclj-repl-prompt)
      (lisp-indent-line))
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (slclj-fuzzy-complete-symbol))
            ((memq (char-before) '(?\t ?\ ))
             (slclj-echo-arglist))))))

(defun* slclj-fuzzy-complete-symbol ()
  "Fuzzily completes the abbreviation at point into a symbol."
  (interactive)
  (when (save-excursion (re-search-backward "\"[^ \t\n]+\\=" nil t))
    (return-from slclj-fuzzy-complete-symbol
      ;; don't add space after completion
      (let ((comint-completion-addsuffix '("/" . "")))
        (if slclj-when-complete-filename-expand
            (comint-replace-by-expanded-filename)
            (comint-dynamic-complete-as-filename)))))
  (let* ((end (move-marker (make-marker) (slclj-symbol-end-pos)))
         (beg (move-marker (make-marker) (slclj-symbol-start-pos)))
         (prefix (buffer-substring-no-properties beg end)))
    (destructuring-bind (completion-set interrupted-p)
        (slclj-fuzzy-completions prefix)
      (if (null completion-set)
          (progn (slclj-minibuffer-respecting-message
                  "Can't find completion for \"%s\"" prefix)
                 (ding)
                 (slclj-fuzzy-done))
          (goto-char end)
          (cond ((slclj-length= completion-set 1)
                 (insert-and-inherit (caar completion-set)) ; insert completed string
                 (delete-region beg end)
                 (goto-char (+ beg (length (caar completion-set))))
                 (slclj-minibuffer-respecting-message "Sole completion")
                 (slclj-fuzzy-done))
                ;; Incomplete
                (t
                 (slclj-fuzzy-choices-buffer completion-set interrupted-p beg end)
                 (slclj-minibuffer-respecting-message "Complete but not unique")))))))


(defun slclj-get-fuzzy-buffer ()
  (get-buffer-create "*Fuzzy Completions*"))

(defvar slclj-fuzzy-explanation
  "For help on how the use this buffer, see `slclj-fuzzy-completions-mode'.

Flags: boundp fboundp generic-function class macro special-operator package
\n"
  "The explanation that gets inserted at the beginning of the
*Fuzzy Completions* buffer.")

(defun slclj-fuzzy-insert-completion-choice (completion max-length)
  "Inserts the completion object `completion' as a formatted
completion choice into the current buffer, and mark it with the
proper text properties."
  (destructuring-bind (symbol-name score chunks classification-string) completion
    (let ((start (point))
	  (end))
      (insert symbol-name)
      (setq end (point))
      (dolist (chunk chunks)
	(put-text-property (+ start (first chunk)) 
			   (+ start (first chunk) 
			      (length (second chunk)))
			   'face 'bold))
      (put-text-property start (point) 'mouse-face 'highlight)
      (dotimes (i (- max-length (- end start)))
	(insert " "))
      (insert (format " %s %s\n"
		      classification-string
                      score))
      (put-text-property start (point) 'completion completion))))

(defun slclj-fuzzy-insert (text)
  "Inserts `text' into the target buffer in the completion slot.
If the buffer has been modified in the meantime, abort the
completion process.  Otherwise, update all completion variables
so that the new text is present."
  (with-current-buffer slclj-fuzzy-target-buffer
    (cond 
     ((not (string-equal slclj-fuzzy-text 
                         (buffer-substring slclj-fuzzy-start
                                           slclj-fuzzy-end)))
      (slclj-fuzzy-done)
      (beep)
      (message "Target buffer has been modified!"))
     (t
      (goto-char slclj-fuzzy-start)
      (delete-region slclj-fuzzy-start slclj-fuzzy-end)
      (insert-and-inherit text)
      (setq slclj-fuzzy-text text)
      (goto-char slclj-fuzzy-end)))))

(defun slclj-fuzzy-choices-buffer (completions interrupted-p start end)
  "Creates (if neccessary), populates, and pops up the *Fuzzy
Completions* buffer with the completions from `completions' and
the completion slot in the current buffer bounded by `start' and
`end'.  This saves the window configuration before popping the
buffer so that it can possibly be restored when the user is
done."
  (let ((new-completion-buffer (not slclj-fuzzy-target-buffer))
        (connection (slclj-connection)))
    (when new-completion-buffer
      (setq slclj-fuzzy-saved-window-configuration
            (current-window-configuration)))
    (slclj-fuzzy-enable-target-buffer-completions-mode)
    (setq slclj-fuzzy-target-buffer (current-buffer))
    (setq slclj-fuzzy-start (move-marker (make-marker) start))
    (setq slclj-fuzzy-end (move-marker (make-marker) end))
    (set-marker-insertion-type slclj-fuzzy-end t)
    (setq slclj-fuzzy-original-text (buffer-substring start end))
    (setq slclj-fuzzy-text slclj-fuzzy-original-text)
    (slclj-fuzzy-fill-completions-buffer completions interrupted-p)
    (pop-to-buffer (slclj-get-fuzzy-buffer))
    (setq slclj-buffer-connection connection)
    (when new-completion-buffer
      ;; Hook to nullify window-config restoration if the user changes
      ;; the window configuration himself.
      (when (boundp 'window-configuration-change-hook)
        (add-hook 'window-configuration-change-hook
                  'slclj-fuzzy-window-configuration-change))
      (slclj-add-local-hook 'kill-buffer-hook 'slclj-fuzzy-abort)
      (set (make-local-variable 'cursor-type) nil)
      (setq buffer-quit-function 'slclj-fuzzy-abort)) ; M-Esc Esc
    (when slclj-fuzzy-completion-in-place
      ;; switch back to the original buffer
      (if (minibufferp slclj-fuzzy-target-buffer)
          (select-window (minibuffer-window))
          (switch-to-buffer-other-window slclj-fuzzy-target-buffer)))))

(defun slclj-fuzzy-fill-completions-buffer (completions interrupted-p)
  "Erases and fills the completion buffer with the given completions."
  (with-current-buffer (slclj-get-fuzzy-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (slclj-fuzzy-completions-mode)
    (insert slclj-fuzzy-explanation)
    (let ((max-length 12))
      (dolist (completion completions)
        (setf max-length (max max-length (length (first completion)))))

      (insert "Completion:")
      (dotimes (i (- max-length 10)) (insert " "))
      ;;     Flags:   Score:
      ;; ... -------  --------
      ;;     bfgctmsp
      (let* ((example-classification-string (fourth (first completions)))
	     (classification-length (length example-classification-string))
	     (spaces (- classification-length (length "Flags:"))))
	(insert "Flags:")
	(dotimes (i spaces) (insert " "))
	(insert " Score:\n")
	(dotimes (i max-length) (insert "-"))
	(insert " ")
	(dotimes (i classification-length) (insert "-"))
	(insert " --------\n")
	(setq slclj-fuzzy-first (point)))

      (dolist (completion completions)
        (setq slclj-fuzzy-last (point)) ; will eventually become the last entry
        (slclj-fuzzy-insert-completion-choice completion max-length))

      (when interrupted-p
        (insert "...\n")
        (insert "[Interrupted: time limit exhausted]"))

      (setq buffer-read-only t))
    (setq slclj-fuzzy-current-completion
          (caar completions))
    (goto-char 0)
    (slclj-fuzzy-next)))

(defun slclj-fuzzy-enable-target-buffer-completions-mode ()
  "Store the target buffer's local map, so that we can restore it."
  (unless slclj-fuzzy-target-buffer-completions-mode
;    (slclj-log-event "Enabling target buffer completions mode")
    (slclj-fuzzy-target-buffer-completions-mode 1)))

(defun slclj-fuzzy-disable-target-buffer-completions-mode ()
  "Restores the target buffer's local map when completion is finished."
  (when slclj-fuzzy-target-buffer-completions-mode
;    (slclj-log-event "Disabling target buffer completions mode")
    (slclj-fuzzy-target-buffer-completions-mode 0)))

(defun slclj-fuzzy-insert-from-point ()
  "Inserts the completion that is under point in the completions
buffer into the target buffer.  If the completion in question had
already been inserted, it does nothing."
  (with-current-buffer (slclj-get-fuzzy-buffer)
    (let ((current-completion (get-text-property (point) 'completion)))
      (when (and current-completion
                 (not (eq slclj-fuzzy-current-completion 
                          current-completion)))
        (slclj-fuzzy-insert 
         (first (get-text-property (point) 'completion)))
        (setq slclj-fuzzy-current-completion
              current-completion)))))

(defun slclj-fuzzy-post-command-hook ()
  "The post-command-hook for the *Fuzzy Completions* buffer.
This makes sure the completion slot in the target buffer matches
the completion that point is on in the completions buffer."
  (condition-case err
      (when slclj-fuzzy-target-buffer
        (slclj-fuzzy-insert-from-point))
    (error
     ;; Because this is called on the post-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in slclj-fuzzy-post-command-hook: %S" err))))

(defun slclj-fuzzy-next ()
  "Moves point directly to the next completion in the completions
buffer."
  (interactive)
  (with-current-buffer (slclj-get-fuzzy-buffer)
    (slclj-fuzzy-dehighlight-current-completion)
    (let ((point (next-single-char-property-change (point) 'completion nil slclj-fuzzy-last)))
      (set-window-point (get-buffer-window (current-buffer)) point)
      (goto-char point))
    (slclj-fuzzy-highlight-current-completion)))

(defun slclj-fuzzy-prev ()
  "Moves point directly to the previous completion in the
completions buffer."
  (interactive)
  (with-current-buffer (slclj-get-fuzzy-buffer)
    (slclj-fuzzy-dehighlight-current-completion)
    (let ((point (previous-single-char-property-change (point) 'completion nil slclj-fuzzy-first)))
      (set-window-point (get-buffer-window (current-buffer)) point)
      (goto-char point))
    (slclj-fuzzy-highlight-current-completion)))

(defun slclj-fuzzy-dehighlight-current-completion ()
  "Restores the original face for the current completion."
  (when slclj-fuzzy-current-completion-overlay
    (overlay-put slclj-fuzzy-current-completion-overlay 'face 'nil)))

(defun slclj-fuzzy-highlight-current-completion ()
  "Highlights the current completion, so that the user can see it on the screen."
  (let ((pos (point)))
    (setq slclj-fuzzy-current-completion-overlay 
          (make-overlay (point) (1- (search-forward " "))
                        (current-buffer) t nil))
    (overlay-put slclj-fuzzy-current-completion-overlay 'face 'secondary-selection)
    (goto-char pos)))

(defun slclj-fuzzy-abort ()
  "Aborts the completion process, setting the completions slot in
the target buffer back to its original contents."
  (interactive)
  (when slclj-fuzzy-target-buffer
    (slclj-fuzzy-done)))

(defun slclj-fuzzy-select ()
  "Selects the current completion, making sure that it is inserted 
into the target buffer.  This tells the connected Lisp what completion
was selected."
  (interactive)
  (when slclj-fuzzy-target-buffer
    (with-current-buffer (slclj-get-fuzzy-buffer)
      (let ((completion (get-text-property (point) 'completion)))
        (when completion
          (slclj-fuzzy-insert (first completion))
          (slclj-fuzzy-selected slclj-fuzzy-original-text
                                completion)
          (slclj-fuzzy-done))))))

(defun slclj-fuzzy-select-or-update-completions ()
  "If there were no changes since the last time fuzzy completion was started
this function will select the current completion. Otherwise refreshes the completion
list based on the changes made."
  (interactive)
;  (slclj-log-event "Selecting or updating completions")
  (if (string-equal slclj-fuzzy-original-text 
                    (buffer-substring slclj-fuzzy-start
                                      slclj-fuzzy-end))
      (slclj-fuzzy-select)
      (slclj-fuzzy-complete-symbol)))

(defun slclj-fuzzy-process-event-in-completions-buffer ()
  "Simply processes the event in the target buffer"
  (interactive)
  (with-current-buffer (slclj-get-fuzzy-buffer)
    (push last-input-event unread-command-events)))

(defun slclj-fuzzy-select-and-process-event-in-target-buffer ()
 "Selects the current completion, making sure that it is inserted
into the target buffer and processes the event in the target buffer."
 (interactive)
; (slclj-log-event "Selecting and processing event in target buffer")
 (when slclj-fuzzy-target-buffer
   (let ((buff slclj-fuzzy-target-buffer))
     (slclj-fuzzy-select)
     (with-current-buffer buff
       (slclj-fuzzy-disable-target-buffer-completions-mode)
       (push last-input-event unread-command-events)))))

(defun slclj-fuzzy-select/mouse (event)
  "Handle a mouse-2 click on a completion choice as if point were
on the completion choice and the slclj-fuzzy-select command was
run."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      (when (get-text-property (point) 'mouse-face)
        (slclj-fuzzy-insert-from-point)
        (slclj-fuzzy-select)))))

(defun slclj-fuzzy-done ()
  "Cleans up after the completion process.  This removes all hooks,
and attempts to restore the window configuration.  If this fails,
it just burys the completions buffer and leaves the window
configuration alone."
  (when slclj-fuzzy-target-buffer
    (set-buffer slclj-fuzzy-target-buffer)
    (slclj-fuzzy-disable-target-buffer-completions-mode)
    (if (slclj-fuzzy-maybe-restore-window-configuration)
        (bury-buffer (slclj-get-fuzzy-buffer))
        ;; We couldn't restore the windows, so just bury the fuzzy
        ;; completions buffer and let something else fill it in.
        (pop-to-buffer (slclj-get-fuzzy-buffer))
        (bury-buffer))
    (pop-to-buffer slclj-fuzzy-target-buffer)
    (goto-char slclj-fuzzy-end)
    (setq slclj-fuzzy-target-buffer nil)
    (remove-hook 'window-configuration-change-hook
		 'slclj-fuzzy-window-configuration-change)))

(defun slclj-fuzzy-maybe-restore-window-configuration ()
  "Restores the saved window configuration if it has not been
nullified."
  (when (boundp 'window-configuration-change-hook)
    (remove-hook 'window-configuration-change-hook
                 'slclj-fuzzy-window-configuration-change))
  (if (not slclj-fuzzy-saved-window-configuration)
      nil
    (set-window-configuration slclj-fuzzy-saved-window-configuration)
    (setq slclj-fuzzy-saved-window-configuration nil)
    t))

(defun slclj-fuzzy-window-configuration-change ()
  "Called on window-configuration-change-hook.  Since the window
configuration was changed, we nullify our saved configuration."
  (setq slclj-fuzzy-saved-window-configuration nil))

;;; Initialization 

(defun slclj-fuzzy-init ()
  (slclj-fuzzy-bind-keys)
  (slclj-require :swank-fuzzy))

(defun slclj-fuzzy-bind-keys ()
  (define-key slclj-mode-map "\C-c\M-i" 'slclj-fuzzy-complete-symbol)
  (when (featurep 'slclj-repl)
   (define-key slclj-repl-mode-map "\C-c\M-i" 'slclj-fuzzy-complete-symbol)))

(provide 'slclj-fuzzy)
