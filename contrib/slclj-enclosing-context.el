;;; slclj-enclosing-context.el --- Utilities on top of slclj-parse.
;;
;; Author:  Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;

;;; TODO: with the removal of `slclj-enclosing-form-specs' this
;;; contrib won't work anymore.

(require 'slclj-parse)

(defvar slclj-variable-binding-ops-alist
  '((let &bindings &body)))

(defvar slclj-function-binding-ops-alist
  '((flet &bindings &body) 
    (labels &bindings &body)
    (macrolet &bindings &body)))

(defun slclj-lookup-binding-op (op &optional binding-type)
  (flet ((lookup-in (list) (assoc* op list :test 'equalp :key 'symbol-name)))
    (cond ((eq binding-type :variable) (lookup-in slclj-variable-binding-ops-alist))
	  ((eq binding-type :function) (lookup-in slclj-function-binding-ops-alist))
	  (t (or (lookup-in slclj-variable-binding-ops-alist)
		 (lookup-in slclj-function-binding-ops-alist))))))

(defun slclj-binding-op-p (op &optional binding-type)
  (and (slclj-lookup-binding-op op binding-type) t))

(defun slclj-binding-op-body-pos (op)
  (when-let (special-lambda-list (slclj-lookup-binding-op op))
    (position '&body special-lambda-list)))

(defun slclj-binding-op-bindings-pos (op)
  (when-let (special-lambda-list (slclj-lookup-binding-op op))
    (position '&bindings special-lambda-list)))


(defun slclj-enclosing-bound-names ()
  "Returns all bound function names as first value, and the
points where their bindings are established as second value."
  (multiple-value-call #'slclj-find-bound-names (slclj-enclosing-form-specs)))

(defun slclj-find-bound-names (ops indices points)
  (let ((binding-names) (binding-start-points))
    (save-excursion
      (loop for (op . nil) in ops
	    for index in indices
	    for point in points
	    do (when (and (slclj-binding-op-p op) 
			  ;; Are the bindings of OP in scope?
			  (>= index (slclj-binding-op-body-pos op)))
		 (goto-char point) 
		 (forward-sexp (slclj-binding-op-bindings-pos op))
		 (down-list)
		 (ignore-errors
		   (loop 
		    (down-list) 
		    (push (slclj-symbol-at-point) binding-names)
		    (push (save-excursion (backward-up-list) (point)) 
			  binding-start-points)
		    (up-list)))))
      (values (nreverse binding-names) (nreverse binding-start-points)))))


(defun slclj-enclosing-bound-functions ()
  (multiple-value-call #'slclj-find-bound-functions (slclj-enclosing-form-specs)))

(defun slclj-find-bound-functions (ops indices points)
  (let ((names) (arglists) (start-points))
    (save-excursion
      (loop for (op . nil) in ops
	    for index in indices
	    for point in points
	    do (when (and (slclj-binding-op-p op :function) 
			  ;; Are the bindings of OP in scope?
			  (>= index (slclj-binding-op-body-pos op)))
		 (goto-char point)
		 (forward-sexp (slclj-binding-op-bindings-pos op))
		 (down-list)
                 ;; If we're at the end of the bindings, an error will
                 ;; be signalled by the `down-list' below.
		 (ignore-errors 
		   (loop
		    (down-list) 
		    (destructuring-bind (name arglist) 
                        (slclj-parse-sexp-at-point 2)
		      (assert (slclj-has-symbol-syntax-p name)) (assert arglist)
		      (push name names)
		      (push arglist arglists)
		      (push (save-excursion (backward-up-list) (point)) 
			    start-points))
		    (up-list)))))
      (values (nreverse names)
	      (nreverse arglists) 
	      (nreverse start-points)))))


(defun slclj-enclosing-bound-macros ()
  (multiple-value-call #'slclj-find-bound-macros (slclj-enclosing-form-specs)))

(defun slclj-find-bound-macros (ops indices points)
  ;; Kludgy!
  (let ((slclj-function-binding-ops-alist '((macrolet &bindings &body))))
    (slclj-find-bound-functions ops indices points)))


(def-slclj-test enclosing-context.1
    (buffer-sexpr wished-bound-names wished-bound-functions)
    "Check that finding local definitions work."
    '(("(flet ((,nil ()))
	 (let ((bar 13)
	       (,foo 42))
	   *HERE*))"
       ;; We used to return ,foo here, but we do not anymore.  We
       ;; still return ,nil for the `slclj-enclosing-bound-functions',
       ;; though. The first one is used for local M-., whereas the
       ;; latter is used for local autodoc. It does not seem too
       ;; important for local M-. to work on such names. \(The reason
       ;; that it does not work anymore, is that
       ;; `slclj-symbol-at-point' now does TRT and does not return a
       ;; leading comma anymore.\)
       ("bar" nil nil)
       ((",nil" "()")))
      ("(flet ((foo ()))
         (quux)
         (bar *HERE*))"
       ("foo")
       (("foo" "()"))))
  (slclj-check-top-level)
  (with-temp-buffer
    (let ((tmpbuf (current-buffer)))
      (lisp-mode)
      (insert buffer-sexpr)
      (search-backward "*HERE*")
      (multiple-value-bind (bound-names points) 
	  (slclj-enclosing-bound-names)
	(slclj-check "Check enclosing bound names"
	  (loop for name in wished-bound-names
		always (member name bound-names))))
      (multiple-value-bind (fn-names fn-arglists points) 
	  (slclj-enclosing-bound-functions)
	(slclj-check "Check enclosing bound functions"
	  (loop for (name arglist) in wished-bound-functions
		always (and (member name fn-names)
			    (member arglist fn-arglists)))))
      )))



(provide 'slclj-enclosing-context)