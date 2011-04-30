;;; slclj-tramp.el ---  Filename translations for tramp
;;
;; Authors: Marco Baringer <mb@bese.it>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path ".../slclj/contrib")
;;   (add-hook 'slclj-load-hook (lambda () (require 'slclj-tramp)))
;;

(require 'tramp)

(defcustom slclj-filename-translations nil
  "Assoc list of hostnames and filename translation functions.  
Each element is of the form (HOSTNAME-REGEXP TO-LISP FROM-LISP).

HOSTNAME-REGEXP is a regexp which is applied to the connection's
slclj-machine-instance. If HOSTNAME-REGEXP maches then the
corresponding TO-LISP and FROM-LISP functions will be used to
translate emacs filenames and lisp filenames.

TO-LISP will be passed the filename of an emacs buffer and must
return a string which the underlying lisp understandas as a
pathname. FROM-LISP will be passed a pathname as returned by the
underlying lisp and must return something that emacs will
understand as a filename (this string will be passed to
find-file).

This list will be traversed in order, so multiple matching
regexps are possible.

Example:

Assuming you run emacs locally and connect to slclj running on
the machine 'soren' and you can connect with the username
'animaliter':

  (push (list \"^soren$\"
              (lambda (emacs-filename)
                (subseq emacs-filename (length \"/ssh:animaliter@soren:\")))
              (lambda (lisp-filename)
                (concat \"/ssh:animaliter@soren:\" lisp-filename)))
        slclj-filename-translations)

See also `slclj-create-filename-translator'."
  :type '(repeat (list :tag "Host description"
                       (regexp :tag "Hostname regexp")
                       (function :tag "To   lisp function")
                       (function :tag "From lisp function")))
  :group 'slclj-lisp)

(defun slclj-find-filename-translators (hostname)
  (cond ((and hostname slclj-filename-translations)
         (or (cdr (assoc-if (lambda (regexp) (string-match regexp hostname))
                            slclj-filename-translations))
             (error "No filename-translations for hostname: %s" hostname)))
        (t (list #'identity #'identity))))

(defun slclj-make-tramp-file-name (username remote-host lisp-filename)
  "Old (with multi-hops) tramp compatability function"
  (if (boundp 'tramp-multi-methods)
      (tramp-make-tramp-file-name nil nil
                                  username
                                  remote-host
                                  lisp-filename)
      (tramp-make-tramp-file-name nil
                                  username
                                  remote-host
                                  lisp-filename)))

(defun* slclj-create-filename-translator (&key machine-instance
                                         remote-host
                                         username)
  "Creates a three element list suitable for push'ing onto
slclj-filename-translations which uses Tramp to load files on
hostname using username. MACHINE-INSTANCE is a required
parameter, REMOTE-HOST defaults to MACHINE-INSTANCE and USERNAME
defaults to (user-login-name).

MACHINE-INSTANCE is the value returned by slclj-machine-instance,
which is just the value returned by cl:machine-instance on the
remote lisp. REMOTE-HOST is the fully qualified domain name (or
just the IP) of the remote machine. USERNAME is the username we
should login with.
The functions created here expect your tramp-default-method or
 tramp-default-method-alist to be setup correctly."
  (lexical-let ((remote-host (or remote-host machine-instance))
                (username (or username (user-login-name))))
    (list (concat "^" machine-instance "$")
          (lambda (emacs-filename)
            (tramp-file-name-localname
             (tramp-dissect-file-name emacs-filename)))
          `(lambda (lisp-filename)
            (slclj-make-tramp-file-name
             ,username
             ,remote-host
             lisp-filename)))))

(defun slclj-tramp-to-lisp-filename (filename)
  (funcall (first (slclj-find-filename-translators (slclj-machine-instance)))
           (expand-file-name filename)))

(defun slclj-tramp-from-lisp-filename (filename)
  (funcall (second (slclj-find-filename-translators (slclj-machine-instance)))
           filename))

(setq slclj-to-lisp-filename-function #'slclj-tramp-to-lisp-filename)
(setq slclj-from-lisp-filename-function #'slclj-tramp-from-lisp-filename)

(provide 'slclj-tramp)