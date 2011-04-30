;;; swank-presentations.el --- imitat LispM' presentations
;;;
;;; Authors: Alan Ruttenberg  <alanr-l@mumble.net>
;;;          Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;;
;;; License: GNU GPL (same license as Emacs)
;;;
;;; Installation
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slclj-load-hook (lambda () (require 'slclj-presentations)))
;;

(unless (featurep 'slclj-repl)
  (error "slclj-presentations requires slclj-repl contrib"))

(defface slclj-repl-output-mouseover-face
  (if (featurep 'xemacs)
      '((t (:bold t)))
    (if (slclj-face-inheritance-possible-p)
        '((t
           (:box
            (:line-width 1 :color "black" :style released-button)
            :inherit
            slclj-repl-inputed-output-face)))
      '((t (:box (:line-width 1 :color "black"))))))
  "Face for Lisp output in the SLCLJ REPL, when the mouse hovers over it"
  :group 'slclj-repl)

(defface slclj-repl-inputed-output-face
  '((((class color) (background light)) (:foreground "Red"))
    (((class color) (background dark)) (:foreground "Red"))
    (t (:slant italic)))
  "Face for the result of an evaluation in the SLCLJ REPL."
  :group 'slclj-repl)

;; FIXME: This conditional is not right - just used because the code
;; here does not work in XEmacs.
(when (boundp 'text-property-default-nonsticky)
  (pushnew '(slclj-repl-presentation . t) text-property-default-nonsticky
	   :test 'equal)
  (pushnew '(slclj-repl-result-face . t) text-property-default-nonsticky
	   :test 'equal))

(make-variable-buffer-local
 (defvar slclj-presentation-start-to-point (make-hash-table)))

(defun slclj-mark-presentation-start (id &optional target)
  "Mark the beginning of a presentation with the given ID.
TARGET can be nil (regular process output) or :repl-result."
  (setf (gethash id slclj-presentation-start-to-point) 
        ;; We use markers because text can also be inserted before this presentation.
        ;; (Output arrives while we are writing presentations within REPL results.)
        (copy-marker (slclj-output-target-marker target) nil)))

(defun slclj-mark-presentation-start-handler (process string)
  (if (and string (string-match "<\\([-0-9]+\\)" string))
      (let* ((match (substring string (match-beginning 1) (match-end 1)))
             (id (car (read-from-string match))))
        (slclj-mark-presentation-start id))))

(defun slclj-mark-presentation-end (id &optional target)
  "Mark the end of a presentation with the given ID.
TARGET can be nil (regular process output) or :repl-result."
  (let ((start (gethash id slclj-presentation-start-to-point)))
    (remhash id slclj-presentation-start-to-point)
    (when start
      (let* ((marker (slclj-output-target-marker target))
             (buffer (and marker (marker-buffer marker))))
        (with-current-buffer buffer
          (let ((end (marker-position marker)))
            (slclj-add-presentation-properties start end
                                               id nil)))))))

(defun slclj-mark-presentation-end-handler (process string)
  (if (and string (string-match ">\\([-0-9]+\\)" string))
      (let* ((match (substring string (match-beginning 1) (match-end 1)))
             (id (car (read-from-string match))))
        (slclj-mark-presentation-end id))))

(defstruct slclj-presentation text id)

(defvar slclj-presentation-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; We give < and > parenthesis syntax, so that #< ... > is treated
    ;; as a balanced expression.  This allows to use C-M-k, C-M-SPC,
    ;; etc. to deal with a whole presentation.  (For Lisp mode, this
    ;; is not desirable, since we do not wish to get a mismatched
    ;; paren highlighted everytime we type < or >.)
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    table)
  "Syntax table for presentations.")

(defun slclj-add-presentation-properties (start end id result-p)
  "Make the text between START and END a presentation with ID.
RESULT-P decides whether a face for a return value or output text is used."
  (let* ((text (buffer-substring-no-properties start end))
         (presentation (make-slclj-presentation :text text :id id)))
    (let ((inhibit-modification-hooks t))
      (add-text-properties start end
                           `(modification-hooks (slclj-after-change-function)
                             insert-in-front-hooks (slclj-after-change-function)
                             insert-behind-hooks (slclj-after-change-function)
                             syntax-table ,slclj-presentation-syntax-table
                             rear-nonsticky t))
      ;; Use the presentation as the key of a text property
      (case (- end start)
        (0)
        (1
         (add-text-properties start end
                              `(slclj-repl-presentation ,presentation
                                ,presentation :start-and-end)))
        (t
         (add-text-properties start (1+ start) 
                              `(slclj-repl-presentation ,presentation
                                ,presentation :start))
         (when (> (- end start) 2)
           (add-text-properties (1+ start) (1- end)
                                `(,presentation :interior)))
         (add-text-properties (1- end) end
                              `(slclj-repl-presentation ,presentation
                                ,presentation :end))))
      ;; Also put an overlay for the face and the mouse-face.  This enables
      ;; highlighting of nested presentations.  However, overlays get lost
      ;; when we copy a presentation; their removal is also not undoable.
      ;; In these cases the mouse-face text properties need to take over ---
      ;; but they do not give nested highlighting.
      (slclj-ensure-presentation-overlay start end presentation))))

(defun slclj-ensure-presentation-overlay (start end presentation)
  (unless (find presentation (overlays-at start)
                :key (lambda (overlay) 
                       (overlay-get overlay 'slclj-repl-presentation)))
    (let ((overlay (make-overlay start end (current-buffer) t nil)))
      (overlay-put overlay 'slclj-repl-presentation presentation)
      (overlay-put overlay 'mouse-face 'slclj-repl-output-mouseover-face)
      (overlay-put overlay 'help-echo 
                   (if (eq major-mode 'slclj-repl-mode)
                       "mouse-2: copy to input; mouse-3: menu"
                     "mouse-2: inspect; mouse-3: menu"))
      (overlay-put overlay 'face 'slclj-repl-inputed-output-face)
      (overlay-put overlay 'keymap slclj-presentation-map))))
  
(defun slclj-remove-presentation-properties (from to presentation)
  (let ((inhibit-read-only t)) 
    (remove-text-properties from to
                            `(,presentation t syntax-table t rear-nonsticky t))
    (when (eq (get-text-property from 'slclj-repl-presentation) presentation)
      (remove-text-properties from (1+ from) `(slclj-repl-presentation t)))
    (when (eq (get-text-property (1- to) 'slclj-repl-presentation) presentation)
      (remove-text-properties (1- to) to `(slclj-repl-presentation t)))
    (dolist (overlay (overlays-at from))
      (when (eq (overlay-get overlay 'slclj-repl-presentation) presentation)
        (delete-overlay overlay)))))

(defun slclj-insert-presentation (string output-id &optional rectangle)
  "Insert STRING in current buffer and mark it as a presentation 
corresponding to OUTPUT-ID.  If RECTANGLE is true, indent multi-line
strings to line up below the current point."
  (flet ((insert-it ()
                    (if rectangle 
                        (slclj-insert-indented string)
                      (insert string))))
    (let ((start (point)))
      (insert-it)
      (slclj-add-presentation-properties start (point) output-id t))))

(defun slclj-presentation-whole-p (presentation start end &optional object)
  (let ((object (or object (current-buffer))))
    (string= (etypecase object
               (buffer (with-current-buffer object
                         (buffer-substring-no-properties start end)))
               (string (substring-no-properties object start end)))
             (slclj-presentation-text presentation))))

(defun slclj-presentations-around-point (point &optional object)
  (let ((object (or object (current-buffer))))
    (loop for (key value . rest) on (text-properties-at point object) by 'cddr
          when (slclj-presentation-p key)
          collect key)))

(defun slclj-presentation-start-p (tag)
  (memq tag '(:start :start-and-end)))

(defun slclj-presentation-stop-p (tag)
  (memq tag '(:end :start-and-end)))

(defun* slclj-presentation-start (point presentation
                                        &optional (object (current-buffer)))
  "Find start of `presentation' at `point' in `object'.
Return buffer index and whether a start-tag was found."
  (let* ((this-presentation (get-text-property point presentation object)))
    (while (not (slclj-presentation-start-p this-presentation))
      (let ((change-point (previous-single-property-change 
                           point presentation object)))
        (unless change-point
          (return-from slclj-presentation-start
            (values (etypecase object
                      (buffer (with-current-buffer object 1))
                      (string 0))
                    nil)))
        (setq this-presentation (get-text-property change-point 
                                                   presentation object))
        (unless this-presentation
          (return-from slclj-presentation-start 
            (values point nil)))
        (setq point change-point)))
    (values point t)))

(defun* slclj-presentation-end (point presentation
                                      &optional (object (current-buffer)))
  "Find end of presentation at `point' in `object'.  Return buffer
index (after last character of the presentation) and whether an
end-tag was found."
  (let* ((this-presentation (get-text-property point presentation object)))
    (while (not (slclj-presentation-stop-p this-presentation))
      (let ((change-point (next-single-property-change 
                           point presentation object)))
        (unless change-point
          (return-from slclj-presentation-end
            (values (etypecase object
                      (buffer (with-current-buffer object (point-max)))
                      (string (length object))) 
                    nil)))
        (setq point change-point)
        (setq this-presentation (get-text-property point 
                                                   presentation object))))
    (if this-presentation 
        (let ((after-end (next-single-property-change point
                                                      presentation object)))
          (if (not after-end)
              (values (etypecase object
                        (buffer (with-current-buffer object (point-max)))
                        (string (length object))) 
                      t)
              (values after-end t)))
        (values point nil))))

(defun* slclj-presentation-bounds (point presentation 
                                         &optional (object (current-buffer)))
  "Return start index and end index of `presentation' around `point'
in `object', and whether the presentation is complete."
  (multiple-value-bind (start good-start)
      (slclj-presentation-start point presentation object)
    (multiple-value-bind (end good-end)
        (slclj-presentation-end point presentation object)
      (values start end 
              (and good-start good-end
                   (slclj-presentation-whole-p presentation 
                                               start end object))))))

(defun slclj-presentation-around-point (point &optional object)
  "Return presentation, start index, end index, and whether the
presentation is complete."
  (let ((object (or object (current-buffer)))
        (innermost-presentation nil)
        (innermost-start 0)
        (innermost-end most-positive-fixnum))
    (dolist (presentation (slclj-presentations-around-point point object))
      (multiple-value-bind (start end whole-p)
          (slclj-presentation-bounds point presentation object)
        (when whole-p 
          (when (< (- end start) (- innermost-end innermost-start))
            (setq innermost-start start
                  innermost-end end
                  innermost-presentation presentation)))))
    (values innermost-presentation
            innermost-start innermost-end)))

(defun slclj-presentation-around-or-before-point (point &optional object)
  (let ((object (or object (current-buffer))))
    (multiple-value-bind (presentation start end whole-p)
        (slclj-presentation-around-point point object)
      (if (or presentation (= point (point-min)))
          (values presentation start end whole-p)
        (slclj-presentation-around-point (1- point) object)))))

(defun slclj-presentation-around-or-before-point-or-error (point)
  (multiple-value-bind (presentation start end whole-p)
      (slclj-presentation-around-or-before-point point)
    (unless presentation
      (error "No presentation at point"))
    (values presentation start end whole-p)))

(defun* slclj-for-each-presentation-in-region (from to function &optional (object (current-buffer)))
  "Call `function' with arguments `presentation', `start', `end',
`whole-p' for every presentation in the region `from'--`to' in the
string or buffer `object'."
  (flet ((handle-presentation (presentation point)
                              (multiple-value-bind (start end whole-p)
                                  (slclj-presentation-bounds point presentation object)
                                (funcall function presentation start end whole-p))))
    ;; Handle presentations active at `from'.
    (dolist (presentation (slclj-presentations-around-point from object))
      (handle-presentation presentation from))
    ;; Use the `slclj-repl-presentation' property to search for new presentations.
    (let ((point from))
      (while (< point to)
        (setq point (next-single-property-change point 'slclj-repl-presentation object to))
        (let* ((presentation (get-text-property point 'slclj-repl-presentation object))
               (status (get-text-property point presentation object)))
          (when (slclj-presentation-start-p status)
            (handle-presentation presentation point)))))))

;; XEmacs compatibility hack, from message by Stephen J. Turnbull on
;; xemacs-beta@xemacs.org of 18 Mar 2002
(unless (boundp 'undo-in-progress)
  (defvar undo-in-progress nil
   "Placeholder defvar for XEmacs compatibility from SLCLJ.")
  (defadvice undo-more (around slclj activate)
     (let ((undo-in-progress t)) ad-do-it)))

(defun slclj-after-change-function (start end &rest ignore)
  "Check all presentations within and adjacent to the change.
When a presentation has been altered, change it to plain text."
  (let ((inhibit-modification-hooks t))
    (let ((real-start (max 1 (1- start)))
          (real-end   (min (1+ (buffer-size)) (1+ end)))
          (any-change nil))
      ;; positions around the change
      (slclj-for-each-presentation-in-region 
       real-start real-end
       (lambda (presentation from to whole-p)
         (cond
          (whole-p
           (slclj-ensure-presentation-overlay from to presentation))
          ((not undo-in-progress)
           (slclj-remove-presentation-properties from to 
                                                 presentation)
           (setq any-change t)))))
      (when any-change
        (undo-boundary)))))

(defun slclj-presentation-around-click (event)
  "Return the presentation around the position of the mouse-click EVENT.
If there is no presentation, signal an error.
Also return the start position, end position, and buffer of the presentation."
  (when (and (featurep 'xemacs) (not (button-press-event-p event)))
    (error "Command must be bound to a button-press-event"))
  (let ((point (if (featurep 'xemacs) (event-point event) (posn-point (event-end event))))
        (window (if (featurep 'xemacs) (event-window event) (caadr event))))
    (with-current-buffer (window-buffer window)
      (multiple-value-bind (presentation start end)
          (slclj-presentation-around-point point)
        (unless presentation
          (error "No presentation at click"))
        (values presentation start end (current-buffer))))))
          
(defun slclj-copy-or-inspect-presentation-at-mouse (event)
  (interactive "e") ; no "@" -- we don't want to select the clicked-at window
  (multiple-value-bind (presentation start end buffer)
      (slclj-presentation-around-click event)
    (if (with-current-buffer buffer
          (eq major-mode 'slclj-repl-mode))
        (slclj-copy-presentation-at-mouse-to-repl event)
      (slclj-inspect-presentation-at-mouse event))))

(defun slclj-inspect-presentation (presentation start end buffer)
  (let ((reset-p 
	 (with-current-buffer buffer
	   (not (eq major-mode 'slclj-inspector-mode)))))
    (slclj-eval-async `(swank:inspect-presentation ',(slclj-presentation-id presentation) ,reset-p)
		      'slclj-open-inspector)))

(defun slclj-inspect-presentation-at-mouse (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer) 
      (slclj-presentation-around-click event)
    (slclj-inspect-presentation presentation start end buffer)))

(defun slclj-inspect-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation start end) 
      (slclj-presentation-around-or-before-point-or-error point)
    (slclj-inspect-presentation presentation start end (current-buffer))))


(defun slclj-M-.-presentation (presentation start end buffer &optional where)
  (let* ((id (slclj-presentation-id presentation))
	 (presentation-string (format "Presentation %s" id))
	 (location (slclj-eval `(swank:find-definition-for-thing
				 (swank::lookup-presented-object
				  ',(slclj-presentation-id presentation))))))
    (slclj-edit-definition-cont
     (and location (list (make-slclj-xref :dspec `(,presentation-string)
					  :location location)))
     presentation-string
     where)))

(defun slclj-M-.-presentation-at-mouse (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer) 
      (slclj-presentation-around-click event)
    (slclj-M-.-presentation presentation start end buffer)))

(defun slclj-M-.-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation start end) 
      (slclj-presentation-around-or-before-point-or-error point)
    (slclj-M-.-presentation presentation start end (current-buffer))))

(defun slclj-edit-presentation (name &optional where)
  (if (or current-prefix-arg (not (equal (slclj-symbol-at-point) name)))
      nil ; NAME came from user explicitly, so decline.
      (multiple-value-bind (presentation start end whole-p)
	  (slclj-presentation-around-or-before-point (point))
	(when presentation
	  (slclj-M-.-presentation presentation start end (current-buffer) where)))))


(defun slclj-copy-presentation-to-repl (presentation start end buffer)
  (let ((presentation-text 
	 (with-current-buffer buffer
	   (buffer-substring start end))))
    (unless (eql major-mode 'slclj-repl-mode)
      (slclj-switch-to-output-buffer))
    (flet ((do-insertion ()
	     (unless (looking-back "\\s-")
	       (insert " "))
	     (insert presentation-text)
	     (unless (or (eolp) (looking-at "\\s-"))
	       (insert " "))))
      (if (>= (point) slclj-repl-prompt-start-mark)
	  (do-insertion)
	(save-excursion
	  (goto-char (point-max))
	  (do-insertion))))))

(defun slclj-copy-presentation-at-mouse-to-repl (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer) 
      (slclj-presentation-around-click event)
    (slclj-copy-presentation-to-repl presentation start end buffer)))

(defun slclj-copy-presentation-at-point-to-repl (point)
  (interactive "d")
  (multiple-value-bind (presentation start end) 
      (slclj-presentation-around-or-before-point-or-error point)
    (slclj-copy-presentation-to-repl presentation start end (current-buffer))))

(defun slclj-copy-presentation-at-mouse-to-point (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer) 
      (slclj-presentation-around-click event)
    (let ((presentation-text 
           (with-current-buffer buffer
             (buffer-substring start end))))
      (when (not (string-match "\\s-"
                               (buffer-substring (1- (point)) (point))))
        (insert " "))
      (insert presentation-text)
      (slclj-after-change-function (point) (point))
      (when (and (not (eolp)) (not (looking-at "\\s-")))
        (insert " ")))))

(defun slclj-copy-presentation-to-kill-ring (presentation start end buffer)
  (let ((presentation-text 
           (with-current-buffer buffer
             (buffer-substring start end))))
    (kill-new presentation-text)
    (message "Saved presentation \"%s\" to kill ring" presentation-text)))

(defun slclj-copy-presentation-at-mouse-to-kill-ring (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer) 
      (slclj-presentation-around-click event)
    (slclj-copy-presentation-to-kill-ring presentation start end buffer)))

(defun slclj-copy-presentation-at-point-to-kill-ring (point)
  (interactive "d")
  (multiple-value-bind (presentation start end) 
      (slclj-presentation-around-or-before-point-or-error point)
    (slclj-copy-presentation-to-kill-ring presentation start end (current-buffer))))
  
(defun slclj-describe-presentation (presentation)
  (slclj-eval-describe 
     `(swank::describe-to-string
       (swank::lookup-presented-object ',(slclj-presentation-id presentation)))))

(defun slclj-describe-presentation-at-mouse (event)
  (interactive "@e")
  (multiple-value-bind (presentation) (slclj-presentation-around-click event)
    (slclj-describe-presentation presentation)))

(defun slclj-describe-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation) 
      (slclj-presentation-around-or-before-point-or-error point)
    (slclj-describe-presentation presentation)))

(defun slclj-pretty-print-presentation (presentation)
  (slclj-eval-describe 
     `(swank::swank-pprint
       (cl:list
        (swank::lookup-presented-object ',(slclj-presentation-id presentation))))))

(defun slclj-pretty-print-presentation-at-mouse (event)
  (interactive "@e")
  (multiple-value-bind (presentation) (slclj-presentation-around-click event)
    (slclj-pretty-print-presentation presentation)))

(defun slclj-pretty-print-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation) 
      (slclj-presentation-around-or-before-point-or-error point)
    (slclj-pretty-print-presentation presentation)))

(defun slclj-mark-presentation (point)
  (interactive "d")
  (multiple-value-bind (presentation start end) 
      (slclj-presentation-around-or-before-point-or-error point)
    (goto-char start)
    (push-mark end nil t)))

(defun slclj-previous-presentation (&optional arg)
  "Move point to the beginning of the first presentation before point.
With ARG, do this that many times.
A negative argument means move forward instead."
  (interactive "p")
  (unless arg (setq arg 1))
  (slclj-next-presentation (- arg)))

(defun slclj-next-presentation (&optional arg)
  "Move point to the beginning of the next presentation after point.
With ARG, do this that many times.
A negative argument means move backward instead."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((plusp arg)
    (dotimes (i arg)
      ;; First skip outside the current surrounding presentation (if any)
      (multiple-value-bind (presentation start end) 
	  (slclj-presentation-around-point (point))
	(when presentation
	  (goto-char end)))
      (let ((p (next-single-property-change (point) 'slclj-repl-presentation)))
	(unless p 
	  (error "No next presentation"))
	(multiple-value-bind (presentation start end) 
	    (slclj-presentation-around-or-before-point-or-error p)
	  (goto-char start)))))
   ((minusp arg)
    (dotimes (i (- arg))
      ;; First skip outside the current surrounding presentation (if any)
      (multiple-value-bind (presentation start end)
	  (slclj-presentation-around-point (point))
	(when presentation
	  (goto-char start)))
      (let ((p (previous-single-property-change (point) 'slclj-repl-presentation)))
	(unless p 
	  (error "No previous presentation"))
	(multiple-value-bind (presentation start end) 
	    (slclj-presentation-around-or-before-point-or-error p)
	  (goto-char start)))))))

(defvar slclj-presentation-map (make-sparse-keymap))

(define-key  slclj-presentation-map [mouse-2] 'slclj-copy-or-inspect-presentation-at-mouse)
(define-key  slclj-presentation-map [mouse-3] 'slclj-presentation-menu)

(when (featurep 'xemacs)
  (define-key  slclj-presentation-map [button2] 'slclj-copy-or-inspect-presentation-at-mouse)
  (define-key  slclj-presentation-map [button3] 'slclj-presentation-menu))

;; protocol for handling up a menu.
;; 1. Send lisp message asking for menu choices for this object. 
;;    Get back list of strings.
;; 2. Let used choose
;; 3. Call back to execute menu choice, passing nth and string of choice

(defun slclj-menu-choices-for-presentation (presentation buffer from to choice-to-lambda)
  "Return a menu for `presentation' at `from'--`to' in `buffer', suitable for `x-popup-menu'."
  (let* ((what (slclj-presentation-id presentation))
         (choices (with-current-buffer buffer
                    (slclj-eval 
                     `(swank::menu-choices-for-presentation-id ',what)))))
    (flet ((savel (f) ;; IMPORTANT - xemacs can't handle lambdas in x-popup-menu. So give them a name
            (let ((sym (gensym)))
              (setf (gethash sym choice-to-lambda) f)
              sym)))
    (etypecase choices
      (list
       `(,(format "Presentation %s" what)
         ("" 
	  ("Find Definition" . ,(savel 'slclj-M-.-presentation-at-mouse))
          ("Inspect" . ,(savel 'slclj-inspect-presentation-at-mouse))
          ("Describe" . ,(savel 'slclj-describe-presentation-at-mouse))
          ("Pretty-print" . ,(savel 'slclj-pretty-print-presentation-at-mouse))
          ("Copy to REPL" . ,(savel 'slclj-copy-presentation-at-mouse-to-repl))
          ("Copy to kill ring" . ,(savel 'slclj-copy-presentation-at-mouse-to-kill-ring))
          ,@(unless buffer-read-only 
              `(("Copy to point" . ,(savel 'slclj-copy-presentation-at-mouse-to-point))))
          ,@(let ((nchoice 0))
              (mapcar 
               (lambda (choice)
                 (incf nchoice)
                 (cons choice 
                       (savel `(lambda ()
                          (interactive)
                          (slclj-eval 
                           '(swank::execute-menu-choice-for-presentation-id
                             ',what ,nchoice ,(nth (1- nchoice) choices)))))))
               choices)))))
      (symbol                           ; not-present
       (with-current-buffer buffer
         (slclj-remove-presentation-properties from to presentation))
       (sit-for 0)                      ; allow redisplay
       `("Object no longer recorded" 
         ("sorry" . ,(if (featurep 'xemacs) nil '(nil)))))))))

(defun slclj-presentation-menu (event)
  (interactive "e")
  (let* ((point (if (featurep 'xemacs) (event-point event) 
                  (posn-point (event-end event))))
         (window (if (featurep 'xemacs) (event-window event) (caadr event)))
         (buffer (window-buffer window))
         (choice-to-lambda (make-hash-table)))
    (multiple-value-bind (presentation from to)
        (with-current-buffer buffer
          (slclj-presentation-around-point point))
      (unless presentation
        (error "No presentation at event position"))
      (let ((menu (slclj-menu-choices-for-presentation 
                   presentation buffer from to choice-to-lambda)))
        (let ((choice (x-popup-menu event menu)))
          (when choice
            (call-interactively (gethash choice choice-to-lambda))))))))

(defun slclj-presentation-expression (presentation)
  "Return a string that contains a CL s-expression accessing 
the presented object."
  (let ((id (slclj-presentation-id presentation)))
    (etypecase id
      (number
       ;; Make sure it works even if *read-base* is not 10.
       (format "(swank:lookup-presented-object-or-lose %d.)" id))
      (list
       ;; for frame variables and inspector parts
       (format "(swank:lookup-presented-object-or-lose '%s)" id)))))

(defun slclj-buffer-substring-with-reified-output (start end)
  (let ((str-props (buffer-substring start end))
        (str-no-props (buffer-substring-no-properties start end)))
    (slclj-reify-old-output str-props str-no-props)))

(defun slclj-reify-old-output (str-props str-no-props)
  (let ((pos (slclj-property-position 'slclj-repl-presentation str-props)))
    (if (null pos)
        str-no-props
        (multiple-value-bind (presentation start-pos end-pos whole-p)
            (slclj-presentation-around-point pos str-props)
          (if (not presentation)
              str-no-props
              (concat (substring str-no-props 0 pos)
                      ;; Eval in the reader so that we play nice with quote.
                      ;; -luke (19/May/2005)
                      "#." (slclj-presentation-expression presentation)
                      (slclj-reify-old-output (substring str-props end-pos)
                                              (substring str-no-props end-pos))))))))



(defun slclj-repl-grab-old-output (replace)
  "Resend the old REPL output at point.  
If replace it non-nil the current input is replaced with the old
output; otherwise the new input is appended."
  (multiple-value-bind (presentation beg end) 
      (slclj-presentation-around-or-before-point (point))
    (let ((old-output (buffer-substring beg end))) ;;keep properties
      ;; Append the old input or replace the current input
      (cond (replace (goto-char slclj-repl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (let ((inhibit-read-only t))
        (insert old-output)))))

;;; Presentation-related key bindings, non-context menu

(defvar slclj-presentation-command-map nil
  "Keymap for presentation-related commands. Bound to a prefix key.")

(defvar slclj-presentation-bindings
  '((?i slclj-inspect-presentation-at-point)
    (?d slclj-describe-presentation-at-point)
    (?w slclj-copy-presentation-at-point-to-kill-ring)
    (?r slclj-copy-presentation-at-point-to-repl)
    (?p slclj-previous-presentation)
    (?n slclj-next-presentation)
    (?\  slclj-mark-presentation)))

(defun slclj-presentation-init-keymaps ()
  (slclj-init-keymap 'slclj-presentation-command-map nil t 
		     slclj-presentation-bindings)
  (define-key slclj-presentation-command-map "\M-o" 'slclj-clear-presentations)
  ;; C-c C-v is the prefix for the presentation-command map.
  (define-key slclj-prefix-map "\C-v" slclj-presentation-command-map))

(defun slclj-presentation-around-or-before-point-p ()
  (multiple-value-bind (presentation beg end) 
      (slclj-presentation-around-or-before-point (point))
    presentation))

(defvar slclj-presentation-easy-menu
  (let ((P '(slclj-presentation-around-or-before-point-p)))
    `("Presentations"
      [ "Find Definition" slclj-M-.-presentation-at-point ,P ]
      [ "Inspect" slclj-inspect-presentation-at-point ,P ]
      [ "Describe" slclj-describe-presentation-at-point ,P ]
      [ "Pretty-print" slclj-pretty-print-presentation-at-point ,P ]
      [ "Copy to REPL" slclj-copy-presentation-at-point-to-repl ,P ]
      [ "Copy to kill ring" slclj-copy-presentation-at-point-to-kill-ring ,P ]
      [ "Mark" slclj-mark-presentation ,P ]
      "--"
      [ "Previous presentation" slclj-previous-presentation ]
      [ "Next presentation" slclj-next-presentation ]
      "--"
      [ "Clear all presentations" slclj-clear-presentations ])))

(defun slclj-presentation-add-easy-menu ()
  (easy-menu-define menubar-slclj-presentation slclj-mode-map "Presentations" slclj-presentation-easy-menu)
  (easy-menu-define menubar-slclj-presentation slclj-repl-mode-map "Presentations" slclj-presentation-easy-menu)
  (easy-menu-define menubar-slclj-presentation sldb-mode-map "Presentations" slclj-presentation-easy-menu)
  (easy-menu-define menubar-slclj-presentation slclj-inspector-mode-map "Presentations" slclj-presentation-easy-menu)
  (easy-menu-add slclj-presentation-easy-menu 'slclj-mode-map)
  (easy-menu-add slclj-presentation-easy-menu 'slclj-repl-mode-map)
  (easy-menu-add slclj-presentation-easy-menu 'sldb-mode-map)
  (easy-menu-add slclj-presentation-easy-menu 'slclj-inspector-mode-map))

;;; hook functions (hard to isolate stuff)

(defun slclj-dispatch-presentation-event (event)
  (destructure-case event
    ((:presentation-start id &optional target)
     (slclj-mark-presentation-start id target)
     t)
    ((:presentation-end id &optional target)
     (slclj-mark-presentation-end id target)
     t)
    (t nil)))

(defun slclj-presentation-write (string &optional target)
  (case target
    ((nil)                              ; Regular process output
     (with-current-buffer (slclj-output-buffer)
       (slclj-with-output-end-mark
	(slclj-propertize-region '(face slclj-repl-output-face
					rear-nonsticky (face))
	  (insert string))
        (set-marker slclj-output-end (point))
        (when (and (= (point) slclj-repl-prompt-start-mark)
                   (not (bolp)))
          (insert "\n")
          (set-marker slclj-output-end (1- (point))))
        (if (< slclj-repl-input-start-mark (point))
            (set-marker slclj-repl-input-start-mark
                        (point))))))
    (:repl-result                       
     (with-current-buffer (slclj-output-buffer)
       (let ((marker (slclj-output-target-marker target)))
         (goto-char marker)
         (slclj-propertize-region `(face slclj-repl-result-face
                                         rear-nonsticky (face))
           (insert string))
         ;; Move the input-start marker after the REPL result.
         (set-marker marker (point)))))
    (t
     (let* ((marker (slclj-output-target-marker target))
            (buffer (and marker (marker-buffer marker))))
       (when buffer
         (with-current-buffer buffer
           (save-excursion 
             ;; Insert STRING at MARKER, then move MARKER behind
             ;; the insertion.
             (goto-char marker)
             (insert-before-markers string)
             (set-marker marker (point)))))))))

(defun slclj-presentation-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer. Presentations of old results are expanded into code."
  (slclj-buffer-substring-with-reified-output  slclj-repl-input-start-mark
					       (point-max)))

(defun slclj-presentation-on-return-pressed ()
  (cond ((and (car (slclj-presentation-around-or-before-point (point)))
	      (< (point) slclj-repl-input-start-mark))
	 (slclj-repl-grab-old-output end-of-input)
	 (slclj-repl-recenter-if-needed)
	 t)
	(t nil)))

(defun slclj-presentation-on-stream-open (stream)
  (require 'bridge)
  (defun bridge-insert (process output)
    (slclj-output-filter process (or output "")))
  (install-bridge)
  (setq bridge-destination-insert nil)
  (setq bridge-source-insert nil)
  (setq bridge-handlers 
	(list* '("<" . slclj-mark-presentation-start-handler) 
	       '(">" . slclj-mark-presentation-end-handler)
	       bridge-handlers)))

(defun slclj-clear-presentations ()
  "Forget all objects associated to SLCLJ presentations.
This allows the garbage collector to remove these objects
even on Common Lisp implementations without weak hash tables."
  (interactive)
  (slclj-eval-async `(swank:clear-repl-results))
  (unless (eql major-mode 'slclj-repl-mode)
    (slclj-switch-to-output-buffer))
  (slclj-for-each-presentation-in-region 1 (1+ (buffer-size)) 
					 (lambda (presentation from to whole-p)
					   (slclj-remove-presentation-properties from to 
										 presentation))))

(defun slclj-presentation-inspector-insert-ispec (ispec)
  (if (stringp ispec)
      (insert ispec)
    (destructure-case ispec
      ((:value string id)
       (slclj-propertize-region 
           (list 'slclj-part-number id 
                 'mouse-face 'highlight
                 'face 'slclj-inspector-value-face)
         (slclj-insert-presentation string `(:inspected-part ,id) t)))
      ((:action string id)
       (slclj-insert-propertized (list 'slclj-action-number id
                                       'mouse-face 'highlight
                                       'face 'slclj-inspector-action-face)
                                 string)))))

(defun slclj-presentation-sldb-insert-frame-variable-value (value frame index)
  (slclj-insert-presentation
   (in-sldb-face local-value value)
   `(:frame-var ,slclj-current-thread ,(car frame) ,index) t))

;;; Initialization

(defun slclj-presentations-init ()
  (slclj-require :swank-presentations)
  (add-hook 'slclj-repl-mode-hook
	    (lambda ()
	      ;; Respect the syntax text properties of presentation.
	      (set (make-local-variable 'parse-sexp-lookup-properties) t)
	      (slclj-add-local-hook 'after-change-functions 
                                    'slclj-after-change-function)))
  (add-hook 'slclj-event-hooks 'slclj-dispatch-presentation-event)
  (setq slclj-write-string-function 'slclj-presentation-write)
  (add-hook 'slclj-repl-return-hooks 'slclj-presentation-on-return-pressed)
  (add-hook 'slclj-repl-current-input-hooks 'slclj-presentation-current-input)
  (add-hook 'slclj-open-stream-hooks 'slclj-presentation-on-stream-open)
  (add-hook 'slclj-repl-clear-buffer-hook 'slclj-clear-presentations)
  (add-hook 'slclj-edit-definition-hooks 'slclj-edit-presentation)
  (setq slclj-inspector-insert-ispec-function 'slclj-presentation-inspector-insert-ispec)
  (setq sldb-insert-frame-variable-value-function 
	'slclj-presentation-sldb-insert-frame-variable-value)
  (slclj-presentation-init-keymaps)
  (slclj-presentation-add-easy-menu))

(provide 'slclj-presentations)
