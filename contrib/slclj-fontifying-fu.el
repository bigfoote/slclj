;;; slclj-fontifying-fu.el --- Additional fontification tweaks.
;;
;; Author:  Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;


;;; Fontify WITH-FOO, DO-FOO, and DEFINE-FOO like standard macros.
;;; Fontify CHECK-FOO like CHECK-TYPE.
(defvar slclj-additional-font-lock-keywords
 '(("(\\(\\(\\s_\\|\\w\\)*:\\(define-\\|do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face) 
   ("(\\(\\(define-\\|do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)
   ("(\\(check-\\(\\s_\\|\\w\\)*\\)" 1 font-lock-warning-face)))


;;;; Specially fontify forms suppressed by a reader conditional.

(defcustom slclj-highlight-suppressed-forms t
  "Display forms disabled by reader conditionals as comments."
  :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
  :group 'slclj-mode)

(defface slclj-reader-conditional-face
  (if (slclj-face-inheritance-possible-p)
    '((t (:inherit font-lock-comment-face)))
  '((((background light)) (:foreground "DimGray" :bold t))
    (((background dark)) (:foreground "LightGray" :bold t))))
  "Face for compiler notes while selected."
  :group 'slclj-mode-faces)

(defvar slclj-search-suppressed-forms-match-data (list nil nil))

(defun slclj-search-suppressed-forms-internal (limit)
  (when (search-forward-regexp slclj-reader-conditionals-regexp limit t)
    (let ((start (match-beginning 0))   ; save match data
          (state (slclj-current-parser-state)))
      (if (or (nth 3 state) (nth 4 state)) ; inside string or comment?
          (slclj-search-suppressed-forms-internal limit)
        (let* ((char (char-before))
               (expr (read (current-buffer)))
               (val  (slclj-eval-feature-expression expr)))
          (when (<= (point) limit)
            (if (or (and (eq char ?+) (not val))
                    (and (eq char ?-) val))
                ;; If `slclj-extend-region-for-font-lock' did not
                ;; fully extend the region, the assertion below may
                ;; fail. This should only happen on XEmacs and older
                ;; versions of GNU Emacs.
                (ignore-errors
                  (forward-sexp) (backward-sexp)
                  ;; Try to suppress as far as possible.
                  (slclj-forward-sexp)
                  (assert (<= (point) limit))
                  (let ((md (match-data nil slclj-search-suppressed-forms-match-data)))
                    (setf (first md) start)
                    (setf (second md) (point))
                    (set-match-data md)
                    t))
                (slclj-search-suppressed-forms-internal limit))))))))

(defun slclj-search-suppressed-forms (limit)
  "Find reader conditionalized forms where the test is false."
  (when (and slclj-highlight-suppressed-forms
             (slclj-connected-p))
    (let ((result 'retry))
      (while (and (eq result 'retry) (<= (point) limit))
        (condition-case condition
            (setq result (slclj-search-suppressed-forms-internal limit))
          (end-of-file                        ; e.g. #+(
           (setq result nil)) 
          ;; We found a reader conditional we couldn't process for
          ;; some reason; however, there may still be other reader
          ;; conditionals before `limit'.
          (invalid-read-syntax                ; e.g. #+#.foo
           (setq result 'retry))
          (scan-error                         ; e.g. #+nil (foo ...
           (setq result 'retry)) 
          (slclj-incorrect-feature-expression ; e.g. #+(not foo bar)
           (setq result 'retry))
          (slclj-unknown-feature-expression   ; e.g. #+(foo)
           (setq result 'retry)) 
          (error
           (setq result nil)
           (slclj-display-warning
            (concat "Caught error during fontification while searching for forms\n"
                    "that are suppressed by reader-conditionals. The error was: %S.")
            condition))))
      result)))


(defun slclj-search-directly-preceding-reader-conditional ()
  "Search for a directly preceding reader conditional. Return its
position, or nil."
  ;;; We search for a preceding reader conditional. Then we check that
  ;;; between the reader conditional and the point where we started is
  ;;; no other intervening sexp, and we check that the reader
  ;;; conditional is at the same nesting level.
  (condition-case nil
      (let* ((orig-pt (point)))
        (when-let (reader-conditional-pt 
                   (search-backward-regexp slclj-reader-conditionals-regexp
                                           ;; We restrict the search to the
                                           ;; beginning of the /previous/ defun.
                                           (save-excursion (beginning-of-defun) (point))
                                           t))
          (let* ((parser-state 
                  (parse-partial-sexp (progn (goto-char (+ reader-conditional-pt 2))
                                             (forward-sexp) ; skip feature expr.
                                             (point))
                                      orig-pt))
                 (paren-depth  (car  parser-state))
                 (last-sexp-pt (caddr  parser-state)))
            (if (and paren-depth (not (plusp paren-depth)) ; no opening parenthesis in between?
                     (not last-sexp-pt))                   ; no complete sexp in between?
                reader-conditional-pt
                nil))))
    (scan-error nil)))                                     ; improper feature expression


;;; We'll push this onto `font-lock-extend-region-functions'. In past,
;;; we didn't do so which made our reader-conditional font-lock magic
;;; pretty unreliable (it wouldn't highlight all suppressed forms, and
;;; worked quite non-deterministic in general.)
;;;
;;; Cf. _Elisp Manual_, 23.6.10 Multiline Font Lock Constructs.
;;;
;;; We make sure that `font-lock-beg' and `font-lock-end' always point
;;; to the beginning or end of a toplevel form. So we never miss a
;;; reader-conditional, or point in mid of one.
(defun slclj-extend-region-for-font-lock ()
  (when slclj-highlight-suppressed-forms
    (condition-case c
        (let (changedp)
          (multiple-value-setq (changedp font-lock-beg font-lock-end)
            (slclj-compute-region-for-font-lock font-lock-beg font-lock-end))
          changedp)
      (error
       (slclj-display-warning
        (concat "Caught error when trying to extend the region for fontification.\n"
                "The error was: %S\n"
                "Further: font-lock-beg=%d, font-lock-end=%d.")
        c font-lock-beg font-lock-end)))))

(when (fboundp 'syntax-ppss-toplevel-pos)
  (defun slclj-beginning-of-tlf ()
    (when-let (pos (syntax-ppss-toplevel-pos (slclj-current-parser-state)))
      (goto-char pos))))

(unless (fboundp 'syntax-ppss-toplevel-pos)
  (defun slclj-beginning-of-tlf ()
    (let* ((state (slclj-current-parser-state))
           (comment-start (nth 8 state)))
      (when comment-start               ; or string
        (goto-char comment-start)
        (setq state (slclj-current-parser-state)))
      (let ((depth (nth 0 state)))
        (when (plusp depth)
          (ignore-errors (up-list (- depth)))) ; ignore unbalanced parentheses
        (when-let (upper-pt (nth 1 state)) 
          (goto-char upper-pt)
          (while (when-let (upper-pt (nth 1 (slclj-current-parser-state)))
                   (goto-char upper-pt))))))))

(defun slclj-compute-region-for-font-lock (orig-beg orig-end)
  (let ((beg orig-beg)
        (end orig-end))
    (goto-char beg)
    (inline (slclj-beginning-of-tlf))
    (assert (not (plusp (nth 0 (slclj-current-parser-state)))))
    (setq beg (let ((pt (point)))
                (cond ((> (- beg pt) 20000) beg)
                      ((slclj-search-directly-preceding-reader-conditional))
                      (t pt))))
    (goto-char end)
    (while (search-backward-regexp slclj-reader-conditionals-regexp beg t)
      (setq end (max end (save-excursion 
                           (ignore-errors (slclj-forward-reader-conditional))
                           (point)))))
    (values (or (/= beg orig-beg) (/= end orig-end)) beg end)))



(defun slclj-activate-font-lock-magic ()
  (if (featurep 'xemacs)
      (let ((pattern `((slclj-search-suppressed-forms
                        (0 slclj-reader-conditional-face t)))))
        (dolist (sym '(lisp-font-lock-keywords
                       lisp-font-lock-keywords-1
                       lisp-font-lock-keywords-2))
          (set sym (append (symbol-value sym) pattern))))
      (font-lock-add-keywords
       'lisp-mode
       `((slclj-search-suppressed-forms 0 ,''slclj-reader-conditional-face t)))

      (add-hook 'lisp-mode-hook 
                #'(lambda () 
                    (add-hook 'font-lock-extend-region-functions
                              'slclj-extend-region-for-font-lock t t)))
      ))


(defun slclj-fontifying-fu-init ()
  (font-lock-add-keywords
   'lisp-mode slclj-additional-font-lock-keywords)
  (when slclj-highlight-suppressed-forms
    (slclj-activate-font-lock-magic)))

(defun slclj-fontifying-fu-unload ()
  (font-lock-remove-keywords 
   'lisp-mode slclj-additional-font-lock-keywords)
  ;;; FIXME: remove `slclj-search-suppressed-forms', and remove the
  ;;; extend-region hook.
  )


(def-slclj-test font-lock-magic (buffer-content)
    "Some testing for the font-lock-magic. *YES* should be
    highlighted as a suppressed form, *NO* should not."

    '(("(defun *NO* (x y) (+ x y))")
      ("(defun *NO*")
      ("*NO*) #-(and) (*YES*) (*NO* *NO*")
      ("\(
\(defun *NO*")
      ("\)
\(defun *NO*
    \(
\)")
      ("#+#.foo
\(defun *NO* (x y) (+ x y))")
      ("#+#.foo
\(defun *NO* (x ")
      ("#+(
\(defun *NO* (x ")
      ("#+(test)
\(defun *NO* (x ")

      ("(eval-when (...)
\(defun *NO* (x ")

      ("(eval-when (...)
#+(and)
\(defun *NO* (x ")

      ("#-(and) (defun *YES* (x y) (+ x y))")
      ("
#-(and) (defun *YES* (x y) (+ x y))
#+(and) (defun *NO* (x y) (+ x y))")

      ("#+(and) (defun *NO* (x y) #-(and) (+ *YES* y))")
      ("#| #+(or) |# *NO*")
      ("#| #+(or) x |# *NO*")
      ("*NO* \"#| *NO* #+(or) x |# *NO*\" *NO*")
      ("#+#.foo (defun foo (bar))
#-(and) *YES* *NO* bar
")
      ("#+(foo) (defun foo (bar))
#-(and) *YES* *NO* bar")
      ("#| #+(or) |# *NO* foo
#-(and) *YES* *NO*")
      ("#- (and)
\(*YES*)
\(*NO*)
#-(and)
\(*YES*)
\(*NO*)")
      ("#+nil (foo)

#-(and)
#+nil (
       asdf *YES* a
            fsdfad)

\( asdf *YES*

       )
\(*NO*)

")
      ("*NO*

#-(and) \(progn
   #-(and)
   (defun *YES* ...)

   #+(and)
   (defun *YES* ...)

   (defun *YES* ...)

   *YES*

   *YES*

   *YES*

   *YES*
\)

*NO*")
      ("#-(not) *YES* *NO*

*NO*

#+(not) *NO* *NO*

*NO*

#+(not a b c) *NO* *NO*

*NO*"))
  (slclj-check-top-level)
  (with-temp-buffer
    (insert buffer-content)
    (slclj-initialize-lisp-buffer-for-test-suite
     :autodoc t :font-lock-magic t)
    ;; Can't use `font-lock-fontify-buffer' because for the case when
    ;; `jit-lock-mode' is enabled. Jit-lock-mode fontifies only on
    ;; actual display.
    (font-lock-default-fontify-buffer)
    (when (search-backward "*NO*" nil t)
      (slclj-test-expect "Not suppressed by reader conditional?"
                         'slclj-reader-conditional-face
                         (get-text-property (point) 'face)
                         #'(lambda (x y) (not (eq x y)))))
    (goto-char (point-max))
    (when (search-backward "*YES*" nil t)
      (slclj-test-expect "Suppressed by reader conditional?"
                         'slclj-reader-conditional-face
                         (get-text-property (point) 'face)))))

(defun* slclj-initialize-lisp-buffer-for-test-suite 
    (&key (font-lock-magic t) (autodoc t))
  (let ((hook lisp-mode-hook))
    (unwind-protect
         (progn 
           (set (make-local-variable 'slclj-highlight-suppressed-forms)
                font-lock-magic)
           (setq lisp-mode-hook nil)
           (lisp-mode)
           (slclj-mode 1)
           (when (boundp 'slclj-autodoc-mode)
             (if autodoc
                 (slclj-autodoc-mode 1)
                 (slclj-autodoc-mode -1))))
      (setq lisp-mode-hook hook))))

(provide 'slclj-fontifying-fu)

(let ((byte-compile-warnings '())) 
  (mapc #'byte-compile
        '(slclj-extend-region-for-font-lock
          slclj-compute-region-for-font-lock
          slclj-search-directly-preceding-reader-conditional
          slclj-search-suppressed-forms
          slclj-beginning-of-tlf)))
