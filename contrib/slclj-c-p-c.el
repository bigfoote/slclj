;;; slclj-c-p-c.el --- ILISP style Compound Prefix Completion
;;
;; Authors: Luke Gorrie  <luke@synap.se>
;;          Edi Weitz  <edi@agharta.de>
;;          Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de> 
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others
;;
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (slclj-setup '(slclj-c-p-c ... possibly other packages ...))
;;



(require 'slclj)
(require 'slclj-parse)
(require 'slclj-editing-commands)
(require 'slclj-autodoc)

(defcustom slclj-c-p-c-unambiguous-prefix-p t
  "If true, set point after the unambigous prefix.
If false, move point to the end of the inserted text."
  :type 'boolean
  :group 'slclj-ui)

(defcustom slclj-complete-symbol*-fancy nil
  "Use information from argument lists for DWIM'ish symbol completion."
  :group 'slclj-mode
  :type 'boolean)

(defun slclj-complete-symbol* ()
  "Expand abbreviations and complete the symbol at point."
  ;; NB: It is only the name part of the symbol that we actually want
  ;; to complete -- the package prefix, if given, is just context.
  (or (slclj-maybe-complete-as-filename)
      (slclj-expand-abbreviations-and-complete)))

;; FIXME: factorize
(defun slclj-expand-abbreviations-and-complete ()
  (let* ((end (move-marker (make-marker) (slclj-symbol-end-pos)))
         (beg (move-marker (make-marker) (slclj-symbol-start-pos)))
         (prefix (buffer-substring-no-properties beg end))
         (completion-result (slclj-contextual-completions beg end))
         (completion-set (first completion-result))
         (completed-prefix (second completion-result)))
    (if (null completion-set)
        (progn (slclj-minibuffer-respecting-message
                "Can't find completion for \"%s\"" prefix)
               (ding)
               (slclj-complete-restore-window-configuration))
      ;; some XEmacs issue makes this distinction necessary
      (cond ((> (length completed-prefix) (- end beg))
	     (goto-char end)
	     (insert-and-inherit completed-prefix)
	     (delete-region beg end)
	     (goto-char (+ beg (length completed-prefix))))
	    (t nil))
      (cond ((and (member completed-prefix completion-set)
                  (slclj-length= completion-set 1))
             (slclj-minibuffer-respecting-message "Sole completion")
             (when slclj-complete-symbol*-fancy
               (slclj-complete-symbol*-fancy-bit))
             (slclj-complete-restore-window-configuration))
            ;; Incomplete
            (t
             (when (member completed-prefix completion-set)
               (slclj-minibuffer-respecting-message 
                "Complete but not unique"))
	     (when slclj-c-p-c-unambiguous-prefix-p
	       (let ((unambiguous-completion-length
		      (loop for c in completion-set
			    minimizing (or (mismatch completed-prefix c)
					   (length completed-prefix)))))
		 (goto-char (+ beg unambiguous-completion-length))))
             (slclj-display-or-scroll-completions completion-set 
                                                  completed-prefix))))))

(defun slclj-complete-symbol*-fancy-bit ()
  "Do fancy tricks after completing a symbol.
\(Insert a space or close-paren based on arglist information.)"
  (let ((arglist (slclj-retrieve-arglist (slclj-symbol-at-point))))
    (unless (eq arglist :not-available)
      (let ((args
             ;; Don't intern these symbols
             (let ((obarray (make-vector 10 0)))
               (cdr (read arglist))))
            (function-call-position-p
             (save-excursion
                (backward-sexp)
                (equal (char-before) ?\())))
        (when function-call-position-p
          (if (null args)
              (insert-and-inherit ")")
            (insert-and-inherit " ")
            (when (and slclj-space-information-p
                       (slclj-background-activities-enabled-p)
                       (not (minibuffer-window-active-p (minibuffer-window))))
              (slclj-echo-arglist))))))))

(defun* slclj-contextual-completions (beg end) 
  "Return a list of completions of the token from BEG to END in the
current buffer."
  (let ((token (buffer-substring-no-properties beg end)))
    (cond
     ((and (< beg (point-max))
           (string= (buffer-substring-no-properties beg (1+ beg)) ":"))
      ;; Contextual keyword completion
      (let ((completions 
             (slclj-completions-for-keyword token
                                            (save-excursion 
                                              (goto-char beg)
                                              (slclj-parse-form-upto-point)))))
        (when (first completions)
          (return-from slclj-contextual-completions completions))
        ;; If no matching keyword was found, do regular symbol
        ;; completion.
        ))
     ((and (>= (length token) 2)
           (string= (subseq token 0 2) "#\\"))
      ;; Character name completion
      (return-from slclj-contextual-completions
        (slclj-completions-for-character token))))
    ;; Regular symbol completion
    (slclj-completions token)))

(defun slclj-completions (prefix)
  (slclj-eval `(swank:completions ,prefix ',(slclj-current-package))))

(defun slclj-completions-for-keyword (prefix buffer-form)
  (slclj-eval `(swank:completions-for-keyword ,prefix ',buffer-form)))

(defun slclj-completions-for-character (prefix)
  (flet ((append-char-syntax (string) (concat "#\\" string)))
    (let ((result (slclj-eval `(swank:completions-for-character
                                ,(subseq prefix 2)))))
      (when (car result)
        (list (mapcar 'append-char-syntax (car result))
              (append-char-syntax (cadr result)))))))


;;; Complete form

(defun slclj-complete-form ()
  "Complete the form at point.  
This is a superset of the functionality of `slclj-insert-arglist'."
  (interactive)
  ;; Find the (possibly incomplete) form around point.
  (let ((buffer-form (slclj-parse-form-upto-point)))
    (let ((result (slclj-eval `(swank:complete-form ',buffer-form))))
      (if (eq result :not-available)
          (error "Could not generate completion for the form `%s'" buffer-form)
          (progn
            (just-one-space (if (looking-back "\\s(") 0 1))
            (save-excursion
              (insert result)
              (let ((slclj-close-parens-limit 1))
                (slclj-close-all-parens-in-sexp)))
            (save-excursion
              (backward-up-list 1)
              (indent-sexp)))))))

;;; Initialization

(defvar slclj-c-p-c-init-undo-stack nil)

(defun slclj-c-p-c-init ()
  (slclj-require :swank-c-p-c)
  ;; save current state for unload
  (push 
   `(progn
      (setq slclj-complete-symbol-function ',slclj-complete-symbol-function)
      (remove-hook 'slclj-connected-hook 'slclj-c-p-c-on-connect)
      ,@(when (featurep 'slclj-repl)
              `((define-key slclj-mode-map "\C-c\C-s"
                  ',(lookup-key slclj-mode-map "\C-c\C-s"))
                (define-key slclj-repl-mode-map "\C-c\C-s"
                  ',(lookup-key slclj-repl-mode-map "\C-c\C-s")))))
   slclj-c-p-c-init-undo-stack)
  (setq slclj-complete-symbol-function 'slclj-complete-symbol*)
  (define-key slclj-mode-map "\C-c\C-s" 'slclj-complete-form)
  (when (featurep 'slclj-repl)
    (define-key slclj-repl-mode-map "\C-c\C-s" 'slclj-complete-form)))

(defun slclj-c-p-c-unload ()
  (while slclj-c-p-c-init-undo-stack
    (eval (pop slclj-c-p-c-init-undo-stack))))

(def-slclj-test complete-symbol*
    (prefix expected-completions)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" (("cl:compile" "cl:compile-file" "cl:compile-file-pathname"
                      "cl:compiled-function" "cl:compiled-function-p" 
                      "cl:compiler-macro" "cl:compiler-macro-function")
                     "cl:compile"))
      ("cl:foobar" nil)
      ("swank::compile-file" (("swank::compile-file" 
                               "swank::compile-file-for-emacs"
                               "swank::compile-file-if-needed"
                               "swank::compile-file-output"
                               "swank::compile-file-pathname")
                              "swank::compile-file"))
      ("cl:m-v-l" (("cl:multiple-value-list" "cl:multiple-values-limit") "cl:multiple-value"))
      ("common-lisp" (("common-lisp-user:" "common-lisp:") "common-lisp")))
  (let ((completions (slclj-completions prefix)))
    (slclj-test-expect "Completion set" expected-completions completions)))

(def-slclj-test complete-form
    (buffer-sexpr wished-completion &optional skip-trailing-test-p)
    ""
    '(("(defmethod arglist-dispatch *HERE*"
       "(defmethod arglist-dispatch (operator arguments) body...)")
      ("(with-struct *HERE*"
       "(with-struct (conc-name names...) obj body...)")
      ("(with-struct *HERE*"
       "(with-struct (conc-name names...) obj body...)")
      ("(with-struct (*HERE*"
       "(with-struct (conc-name names...)" t)
      ("(with-struct (foo. bar baz *HERE*"
       "(with-struct (foo. bar baz names...)" t))
  (slclj-check-top-level)
  (with-temp-buffer
    (lisp-mode)
    (setq slclj-buffer-package "SWANK")
    (insert buffer-sexpr)
    (search-backward "*HERE*")
    (delete-region (match-beginning 0) (match-end 0))
    (slclj-complete-form)
    (slclj-check-completed-form buffer-sexpr wished-completion)

    ;; Now the same but with trailing `)' for paredit users...
    (unless skip-trailing-test-p
      (erase-buffer)
      (insert buffer-sexpr)
      (search-backward "*HERE*")
      (delete-region (match-beginning 0) (match-end 0))
      (insert ")") (backward-char)
      (slclj-complete-form)
      (slclj-check-completed-form (concat buffer-sexpr ")") wished-completion))
    ))

(defun slclj-check-completed-form (buffer-sexpr wished-completion)
  (slclj-test-expect (format "Completed form for `%s' is as expected"
                              buffer-sexpr)
                     wished-completion
                     (buffer-string)
                     'equal))

(provide 'slclj-c-p-c)
