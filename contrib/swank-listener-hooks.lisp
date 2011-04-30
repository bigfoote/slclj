;;; swank-listener-hooks.lisp --- listener with special hooks
;;
;; Author: Alan Ruttenberg  <alanr-l@mumble.net>

;; I guess that only Alan Ruttenberg knows how to use this code.  It
;; was in swank.lisp for a long time, so here it is. -- Helmut Eller

(in-package :swank)

(defvar *slclj-repl-advance-history* nil 
  "In the dynamic scope of a single form typed at the repl, is set to nil to 
   prevent the repl from advancing the history - * ** *** etc.")

(defvar *slclj-repl-suppress-output* nil
  "In the dynamic scope of a single form typed at the repl, is set to nil to
   prevent the repl from printing the result of the evalation.")
  
(defvar *slclj-repl-eval-hook-pass* (gensym "PASS")
  "Token to indicate that a repl hook declines to evaluate the form")

(defvar *slclj-repl-eval-hooks* nil
  "A list of functions. When the repl is about to eval a form, first try running each of
   these hooks. The first hook which returns a value which is not *slclj-repl-eval-hook-pass*
   is considered a replacement for calling eval. If there are no hooks, or all
   pass, then eval is used.")

(defslcljfun repl-eval-hook-pass ()
  "call when repl hook declines to evaluate the form"
  (throw *slclj-repl-eval-hook-pass* *slclj-repl-eval-hook-pass*))

(defslcljfun repl-suppress-output ()
  "In the dynamic scope of a single form typed at the repl, call to
   prevent the repl from printing the result of the evalation."
  (setq *slclj-repl-suppress-output* t))

(defslcljfun repl-suppress-advance-history ()
  "In the dynamic scope of a single form typed at the repl, call to 
   prevent the repl from advancing the history - * ** *** etc."
  (setq *slclj-repl-advance-history* nil))

(defun %eval-region (string)
  (with-input-from-string (stream string)
    (let (- values)
      (loop
       (let ((form (read stream nil stream)))
	 (when (eq form stream)
	   (fresh-line)
	   (finish-output)
	   (return (values values -)))
	 (setq - form)
	 (if *slclj-repl-eval-hooks* 
	     (setq values (run-repl-eval-hooks form))
	     (setq values (multiple-value-list (eval form))))
	 (finish-output))))))

(defun run-repl-eval-hooks (form)
  (loop for hook in *slclj-repl-eval-hooks* 
	for res =  (catch *slclj-repl-eval-hook-pass* 
		     (multiple-value-list (funcall hook form)))
	until (not (eq res *slclj-repl-eval-hook-pass*))
	finally (return 
		  (if (eq res *slclj-repl-eval-hook-pass*)
		      (multiple-value-list (eval form))
		      res))))

(defun %listener-eval (string)
  (clear-user-input)
  (with-buffer-syntax ()
    (track-package 
     (lambda ()
       (let ((*slclj-repl-suppress-output* :unset)
	     (*slclj-repl-advance-history* :unset))
	 (multiple-value-bind (values last-form) (%eval-region string)
	   (unless (or (and (eq values nil) (eq last-form nil))
		       (eq *slclj-repl-advance-history* nil))
	     (setq *** **  ** *  * (car values)
		   /// //  // /  / values))
	   (setq +++ ++  ++ +  + last-form)
	   (unless (eq *slclj-repl-suppress-output* t)
	     (funcall *send-repl-results-function* values)))))))
  nil)

(setq *listener-eval-function* '%listener-eval)

(provide :swank-listener-hooks)
