;;; slclj-editing-commands.el -- editing commands whithout server interaction
;;
;; Authors: Thomas F. Burdick  <tfb@OCF.Berkeley.EDU>
;;          Luke Gorrie  <luke@synap.se>
;;          Bill Clementson <billclem@gmail.com>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others
;; 
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slclj-load-hook (lambda () (require 'slclj-editing-commands)))
;;

(defun slclj-beginning-of-defun ()
  (interactive)
  (if (and (boundp 'slclj-repl-input-start-mark)
           slclj-repl-input-start-mark)
      (slclj-repl-beginning-of-defun)
      (beginning-of-defun)))

(defun slclj-end-of-defun ()
  (interactive)
  (if (eq major-mode 'slclj-repl-mode)
      (slclj-repl-end-of-defun)
      (end-of-defun)))

(defvar slclj-comment-start-regexp
  "\\(\\(^\\|[^\n\\\\]\\)\\([\\\\][\\\\]\\)*\\);+[ \t]*"
  "Regexp to match the start of a comment.")

(defun slclj-beginning-of-comment ()
  "Move point to beginning of comment.
If point is inside a comment move to beginning of comment and return point.
Otherwise leave point unchanged and return NIL."
  (let ((boundary (point)))
    (beginning-of-line)
    (cond ((re-search-forward slclj-comment-start-regexp boundary t)
           (point))
          (t (goto-char boundary) 
             nil))))

(defun slclj-close-all-parens-in-sexp (&optional region)
  "Balance parentheses of open s-expressions at point.
Insert enough right parentheses to balance unmatched left parentheses.
Delete extra left parentheses.  Reformat trailing parentheses 
Lisp-stylishly.

If REGION is true, operate on the region. Otherwise operate on
the top-level sexp before point."
  (interactive "P")
  (let ((sexp-level 0)
        point)
    (save-excursion
      (save-restriction
        (when region
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-max)))
        ;; skip over closing parens, but not into comment
        (skip-chars-backward ") \t\n")
        (when (slclj-beginning-of-comment)
          (forward-line)
          (skip-chars-forward " \t"))
        (setq point (point))
        ;; count sexps until either '(' or comment is found at first column
        (while (and (not (looking-at "^[(;]"))
		    (ignore-errors (backward-up-list 1) t))
          (incf sexp-level))))
    (when (> sexp-level 0)
      ;; insert correct number of right parens
      (goto-char point)
      (dotimes (i sexp-level) (insert ")"))
      ;; delete extra right parens
      (setq point (point))
      (skip-chars-forward " \t\n)")
      (skip-chars-backward " \t\n")
      (let* ((deleted-region     (slclj-delete-and-extract-region point (point)))
             (deleted-text       (substring-no-properties deleted-region))
             (prior-parens-count (count ?\) deleted-text)))
        ;; Remember: we always insert as many parentheses as necessary
        ;; and only afterwards delete the superfluously-added parens.
        (when slclj-close-parens-limit
          (let ((missing-parens (- sexp-level prior-parens-count
                                   slclj-close-parens-limit)))
            (dotimes (i (max 0 missing-parens))
              (delete-char -1))))))))

(defvar slclj-close-parens-limit nil
  "Maxmimum parens for `slclj-close-all-sexp' to insert. NIL
means to insert as many parentheses as necessary to correctly
close the form.")

(defun slclj-insert-balanced-comments (arg)
  "Insert a set of balanced comments around the s-expression
containing the point.  If this command is invoked repeatedly
\(without any other command occurring between invocations), the
comment progressively moves outward over enclosing expressions.
If invoked with a positive prefix argument, the s-expression arg
expressions out is enclosed in a set of balanced comments."
  (interactive "*p")
  (save-excursion
    (when (eq last-command this-command)
      (when (search-backward "#|" nil t)
        (save-excursion
          (delete-char 2)
          (while (and (< (point) (point-max)) (not (looking-at " *|#")))
            (forward-sexp))
          (replace-match ""))))
    (while (> arg 0)
      (backward-char 1)
      (cond ((looking-at ")") (incf arg))
            ((looking-at "(") (decf arg))))
    (insert "#|")
    (forward-sexp)
    (insert "|#")))

(defun slclj-remove-balanced-comments ()
  "Remove a set of balanced comments enclosing point."
  (interactive "*")
  (save-excursion
    (when (search-backward "#|" nil t)
      (delete-char 2)
      (while (and (< (point) (point-max)) (not (looking-at " *|#")))
      (forward-sexp))
      (replace-match ""))))


;; SLCLJ-CLOSE-PARENS-AT-POINT is obsolete:

;; It doesn't work correctly on the REPL, because there
;; BEGINNING-OF-DEFUN-FUNCTION and END-OF-DEFUN-FUNCTION is bound to
;; SLCLJ-REPL-MODE-BEGINNING-OF-DEFUN (and
;; SLCLJ-REPL-MODE-END-OF-DEFUN respectively) which compromises the
;; way how they're expect to work (i.e. END-OF-DEFUN does not signal
;; an UNBOUND-PARENTHESES error.)

;; Use SLCLJ-CLOSE-ALL-PARENS-IN-SEXP instead.

;; (defun slclj-close-parens-at-point ()
;;   "Close parenthesis at point to complete the top-level-form.  Simply
;; inserts ')' characters at point until `beginning-of-defun' and
;; `end-of-defun' execute without errors, or `slclj-close-parens-limit'
;; is exceeded."
;;   (interactive)
;;   (loop for i from 1 to slclj-close-parens-limit
;;         until (save-excursion
;;                 (slclj-beginning-of-defun)
;;                 (ignore-errors (slclj-end-of-defun) t))
;;         do (insert ")")))

(defun slclj-reindent-defun (&optional force-text-fill)
  "Reindent the current defun, or refill the current paragraph.
If point is inside a comment block, the text around point will be
treated as a paragraph and will be filled with `fill-paragraph'.
Otherwise, it will be treated as Lisp code, and the current defun
will be reindented.  If the current defun has unbalanced parens,
an attempt will be made to fix it before reindenting.

When given a prefix argument, the text around point will always
be treated as a paragraph.  This is useful for filling docstrings."
  (interactive "P")
  (save-excursion
    (if (or force-text-fill (slclj-beginning-of-comment))
        (fill-paragraph nil)
      (let ((start (progn (unless (or (and (zerop (current-column))
                                           (eq ?\( (char-after)))
                                      (and slclj-repl-input-start-mark
                                           (slclj-repl-at-prompt-start-p)))
                            (slclj-beginning-of-defun))
                          (point)))
            (end (ignore-errors (slclj-end-of-defun) (point))))
        (unless end
          (forward-paragraph)
          (slclj-close-all-parens-in-sexp)
          (slclj-end-of-defun)
          (setf end (point)))
        (indent-region start end nil)))))

(defun slclj-editing-commands-init ()
  (define-key slclj-mode-map "\M-\C-a"  'slclj-beginning-of-defun)
  (define-key slclj-mode-map "\M-\C-e"  'slclj-end-of-defun)
  (define-key slclj-mode-map "\C-c\M-q" 'slclj-reindent-defun)
  (define-key slclj-mode-map "\C-c\C-]" 'slclj-close-all-parens-in-sexp))

(provide 'slclj-editing-commands)
