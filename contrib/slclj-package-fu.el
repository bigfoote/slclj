;;; slclj-package-fu.el --- Exporting/Unexporting symbols at point.
;;
;; Author:  Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;

(defvar slclj-package-file-candidates
  (mapcar #'file-name-nondirectory
	  '("package.lisp" "packages.lisp" "pkgdcl.lisp" "defpackage.lisp")))

(defvar slclj-export-symbol-representation-function
  #'(lambda (n) (format "#:%s" n)))

(defvar slclj-defpackage-regexp
  "^(\\(cl:\\|common-lisp:\\)?defpackage\\>[ \t']*")


(defun slclj-find-package-definition-rpc (package)
  (slclj-eval `(swank:find-definition-for-thing (swank::guess-package ,package))))

(defun slclj-find-package-definition-regexp (package)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (block nil
	(while (re-search-forward slclj-defpackage-regexp nil t)
	  (when (slclj-package-equal package (slclj-sexp-at-point))
            (backward-sexp)
	    (return (make-slclj-file-location (buffer-file-name)
                                              (1- (point))))))))))

(defun slclj-package-equal (designator1 designator2)
  ;; First try to be lucky and compare the strings themselves (for the
  ;; case when one of the designated packages isn't loaded in the
  ;; image.) Then try to do it properly using the inferior Lisp which
  ;; will also resolve nicknames for us &c.
  (or (equalp (slclj-cl-symbol-name designator1)
	      (slclj-cl-symbol-name designator2))
      (slclj-eval `(swank:package= ,designator1 ,designator2))))

(defun slclj-export-symbol (symbol package)
  "Unexport `symbol' from `package' in the Lisp image."
  (slclj-eval `(swank:export-symbol-for-emacs ,symbol ,package)))

(defun slclj-unexport-symbol (symbol package)
  "Export `symbol' from `package' in the Lisp image."
  (slclj-eval `(swank:unexport-symbol-for-emacs ,symbol ,package)))


(defun slclj-find-possible-package-file (buffer-file-name)
  (flet ((file-name-subdirectory (dirname)
	   (expand-file-name
	    (concat (file-name-as-directory (slclj-to-lisp-filename dirname))
		    (file-name-as-directory ".."))))
	 (try (dirname)
	   (dolist (package-file-name slclj-package-file-candidates)
	     (let ((f (slclj-to-lisp-filename (concat dirname package-file-name))))
	       (when (file-readable-p f)
		 (return f))))))
    (when buffer-file-name
      (let ((buffer-cwd (file-name-directory buffer-file-name)))
	(or (try buffer-cwd)
	    (try (file-name-subdirectory buffer-cwd))
	    (try (file-name-subdirectory (file-name-subdirectory buffer-cwd))))))))

(defun slclj-goto-package-source-definition (package)
  "Tries to find the DEFPACKAGE form of `package'. If found,
places the cursor at the start of the DEFPACKAGE form."
  (flet ((try (location)
	   (when (slclj-location-p location)
	     (slclj-goto-source-location location)
	     t)))
    (or (try (slclj-find-package-definition-rpc package))
	(try (slclj-find-package-definition-regexp package))
	(try (when-let (package-file (slclj-find-possible-package-file (buffer-file-name)))
	       (with-current-buffer (find-file-noselect package-file t)
		 (slclj-find-package-definition-regexp package))))
	(error "Couldn't find source definition of package: %s" package))))

(defun slclj-at-expression-p (pattern)
  (when (ignore-errors
          ;; at a list?
          (= (point) (progn (down-list 1)
                            (backward-up-list 1)
                            (point))))
    (save-excursion
      (down-list 1)
      (slclj-in-expression-p pattern))))

(defun slclj-goto-next-export-clause ()
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (let ((point))
    (save-excursion
      (block nil
	(while (ignore-errors (slclj-forward-sexp) t)
          (skip-chars-forward " \n\t")
	  (when (slclj-at-expression-p '(:export *))
	    (setq point (point))
	    (return)))))
    (if point
	(goto-char point)
	(error "No next (:export ...) clause found"))))

(defun slclj-search-exports-in-defpackage (symbol-name)
  "Look if `symbol-name' is mentioned in one of the :EXPORT clauses."
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (flet ((target-symbol-p (symbol)
           (string-match-p (format "^\\(\\(#:\\)\\|:\\)?%s$"
                                   (regexp-quote symbol-name))
                           symbol)))
    (save-excursion
      (block nil
        (while (ignore-errors (slclj-goto-next-export-clause) t)
          (let ((clause-end (save-excursion (forward-sexp) (point))))
            (when (and (search-forward symbol-name clause-end t)
                       (target-symbol-p (slclj-symbol-at-point)))
              (return (point)))))))))

(defun slclj-frob-defpackage-form (current-package do-what symbol)
  "Adds/removes `symbol' from the DEFPACKAGE form of `current-package'
depending on the value of `do-what' which can either be `:export',
or `:unexport'.

Returns t if the symbol was added/removed. Nil if the symbol was
already exported/unexported."
  (let ((symbol-name (slclj-cl-symbol-name symbol)))
    (save-excursion
      (slclj-goto-package-source-definition current-package)
      (down-list 1)			; enter DEFPACKAGE form
      (forward-sexp)			; skip DEFPACKAGE symbol
      (forward-sexp)			; skip package name
      (let ((already-exported-p (slclj-search-exports-in-defpackage symbol-name)))
	(ecase do-what
	  (:export
	   (if already-exported-p
	       nil
	       (prog1 t (slclj-insert-export symbol-name))))
	  (:unexport
	   (if already-exported-p
	       (prog1 t (slclj-remove-export symbol-name))
	       nil)))))))


(defun slclj-insert-export (symbol-name)
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (flet ((goto-last-export-clause ()
	   (let (point)
	     (save-excursion
	       (while (ignore-errors (slclj-goto-next-export-clause) t)
		 (setq point (point))))
	     (when point (goto-char point))
	     point)))
    (let ((defpackage-point (point))
	  (symbol-name (funcall slclj-export-symbol-representation-function
				symbol-name)))
      (cond ((goto-last-export-clause)
	     (down-list) (slclj-end-of-list)
	     (unless (looking-back "^\\s-*")
	       (newline-and-indent))
	     (insert symbol-name))
	    (t
	     (slclj-end-of-list)
	     (newline-and-indent)
	     (insert (format "(:export %s)" symbol-name)))))))

(defun slclj-remove-export (symbol-name)
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (let ((point))
    (while (setq point (slclj-search-exports-in-defpackage symbol-name))
      (save-excursion
	(goto-char point)
	(backward-sexp)
	(delete-region (point) point)
	(beginning-of-line)
	(when (looking-at "^\\s-*$")
	  (join-line))))))


(defun slclj-export-symbol-at-point ()
  "Add the symbol at point to the defpackage source definition
belonging to the current buffer-package. With prefix-arg, remove
the symbol again. Additionally performs an EXPORT/UNEXPORT of the
symbol in the Lisp image if possible."
  (interactive)
  (let ((package (slclj-current-package))
	(symbol (slclj-symbol-at-point)))
    (unless symbol (error "No symbol at point."))
    (cond (current-prefix-arg
	   (if (slclj-frob-defpackage-form package :unexport symbol)
	       (message "Symbol `%s' no longer exported form `%s'" symbol package)
	       (message "Symbol `%s' is not exported from `%s'" symbol package))
	   (slclj-unexport-symbol symbol package))
	  (t
	   (if (slclj-frob-defpackage-form package :export symbol)
	       (message "Symbol `%s' now exported from `%s'" symbol package)
	       (message "Symbol `%s' already exported from `%s'" symbol package))
	   (slclj-export-symbol symbol package)))))


(defvar slclj-package-fu-init-undo-stack nil)

(defun slclj-package-fu-init ()
  (slclj-require :swank-package-fu)
  (push `(progn (define-key slclj-mode-map "\C-cx"
		  ',(lookup-key slclj-mode-map "\C-cx")))
	slclj-package-fu-init-undo-stack)
  (define-key slclj-mode-map "\C-cx"  'slclj-export-symbol-at-point))

(defun slclj-package-fu-unload ()
  (while slclj-c-p-c-init-undo-stack
    (eval (pop slclj-c-p-c-init-undo-stack))))

(provide 'slclj-package-fu)
