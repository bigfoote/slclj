;;; slclj-mdot-fu.el --- Making M-. work on local functions.
;;
;; Author:  Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;

(require 'slclj-enclosing-context)

(defun slclj-edit-local-definition (name &optional where)
  "Like `slclj-edit-definition', but tries to find the definition
in a local function binding near point."
  (interactive (list (slclj-read-symbol-name "Name: ")))
  (multiple-value-bind (binding-name point)
      (multiple-value-call #'some #'(lambda (binding-name point)
				      (when (equalp binding-name name)
					(values binding-name point)))
			   (slclj-enclosing-bound-names))
    (when (and binding-name point)
      (slclj-edit-definition-cont 
       `((,binding-name
	  ,(make-slclj-buffer-location (buffer-name (current-buffer)) point)))
       name
       where))))

(defun slclj-mdot-fu-init ()
  (add-hook 'slclj-edit-definition-hooks 
	    'slclj-edit-local-definition))

(defun slclj-mdot-fu-unload ()
  (remove-hook 'slclj-edit-definition-hooks 
	       'slclj-edit-local-definition))



(def-slclj-test find-local-definitions.1
    (buffer-sexpr definition target-regexp)
    "Check that finding local definitions work."
    '(((defun foo (x)
	  (let ((y (+ x 1)))
	    (- x y *HERE*)))
       y
       "(y (+ x 1))")

      ((defun bar (x)
	 (flet ((foo (z) (+ x z)))
	   (* x (foo *HERE*))))
       foo
       "(foo (z) (+ x z))")

      ((defun quux (x)
	 (flet ((foo (z) (+ x z)))
	   (let ((foo (- 1 x)))
	     (+ x foo *HERE*))))
       foo
       "(foo (- 1 x)")
      
      ((defun zurp (x)
	 (macrolet ((frob (x y) `(quux ,x ,y)))
	   (frob x *HERE*)))
       frob
       "(frob (x y)"))
  (slclj-check-top-level)
  (with-temp-buffer
    (let ((tmpbuf (current-buffer)))
      (insert (prin1-to-string buffer-sexpr))
      (search-backward "*HERE*")
      (slclj-edit-local-definition (prin1-to-string definition))
      (slclj-sync)
      (slclj-check "Check that we didnt leave the temp buffer." 
	(eq (current-buffer) tmpbuf))
      (slclj-check "Check that we are at the local definition."
	(looking-at (regexp-quote target-regexp))))))


(provide 'slclj-mdot-fu)


