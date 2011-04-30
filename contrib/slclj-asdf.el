;;; slclj-asdf.el -- ASDF support
;;
;; Authors: Daniel Barlow       <dan@telent.net>
;;          Marco Baringer      <mb@bese.it>
;;          Edi Weitz           <edi@agharta.de>
;;          Stas Boukarev       <stassats@gmail.com>
;;          Tobias C Rittweiler <tcr@freebits.de>
;;          and others 
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (slclj-setup '(slclj-asdf ... possibly other packages ...))
;;

;; NOTE: `system-name' is a predefined variable in Emacs.  Try to
;; avoid it as local variable name.

(require 'slclj-repl)
(slclj-require :swank-asdf)

;;; Utilities

(defvar slclj-system-history nil
  "History list for ASDF system names.")

(defun slclj-read-system-name (&optional prompt 
                                         default-value
                                         determine-default-accurately)
  "Read a system name from the minibuffer, prompting with PROMPT.
If no `default-value' is given, one is tried to be determined: if
`determine-default-accurately' is true, by an RPC request which
grovels through all defined systems; if it's not true, by looking
in the directory of the current buffer."
  (let* ((completion-ignore-case nil)
         (prompt (or prompt "System"))
         (system-names (slclj-eval `(swank:list-asdf-systems)))
         (default-value (or default-value 
                            (if determine-default-accurately
                                (slclj-determine-asdf-system (buffer-file-name)
                                                             (slclj-current-package))
                                (slclj-find-asd-file (or default-directory
                                                         (buffer-file-name))
                                                     system-names))))
         (prompt (concat prompt (if default-value
                                    (format " (default `%s'): " default-value)
                                    ": "))))
    (completing-read prompt (slclj-bogus-completion-alist system-names)
                     nil nil nil
                     'slclj-system-history default-value)))



(defun slclj-find-asd-file (directory system-names)
  "Tries to find an ASDF system definition file in the
`directory' and returns it if it's in `system-names'."
  (let ((asd-files
         (directory-files (file-name-directory directory) nil "\.asd$")))
    (loop for system in asd-files
          for candidate = (file-name-sans-extension system)
          when (find candidate system-names :test #'string-equal)
            do (return candidate))))

(defun slclj-determine-asdf-system (filename buffer-package)
  "Try to determine the asdf system that `filename' belongs to."
  (slclj-eval `(swank:asdf-determine-system ,(slclj-to-lisp-filename filename)
                                            ,buffer-package)))

(defun slclj-who-depends-on-rpc (system)
  (slclj-eval `(swank:who-depends-on ,system)))

(defun slclj-oos (system operation &rest keyword-args)
  "Operate On System."
  (slclj-save-some-lisp-buffers)
  (slclj-display-output-buffer)
  (message "Performing ASDF %S%s on system %S"
           operation (if keyword-args (format " %S" keyword-args) "")
           system)
  (slclj-repl-shortcut-eval-async
   `(swank:operate-on-system-for-emacs ,system ',operation ,@keyword-args)
   #'slclj-compilation-finished))


;;; Interactive functions

(defun slclj-load-system (&optional system)
  "Compile and load an ASDF system.  

Default system name is taken from first file matching *.asd in current
buffer's working directory"
  (interactive (list (slclj-read-system-name)))
  (slclj-oos system 'load-op))

(defun slclj-open-system (name &optional load)
  "Open all files in an ASDF system."
  (interactive (list (slclj-read-system-name)))
  (when (or load
            (and (called-interactively-p)
                 (not (slclj-eval `(swank:asdf-system-loaded-p ,name)))
                 (y-or-n-p "Load it? ")))
    (slclj-load-system name))
  (slclj-eval-async
   `(swank:asdf-system-files ,name)
   (lambda (files)
     (when files
       (let ((files (mapcar 'slclj-from-lisp-filename
                            (nreverse files))))
         (find-file-other-window (car files))
         (mapc 'find-file (cdr files)))))))

(defun slclj-browse-system (name)
  "Browse files in an ASDF system using Dired."
  (interactive (list (slclj-read-system-name)))
  (slclj-eval-async `(swank:asdf-system-directory ,name)
   (lambda (directory)
     (when directory
       (dired (slclj-from-lisp-filename directory))))))

(if (fboundp 'rgrep)
    (defun slclj-rgrep-system (sys-name regexp)
      "Run `rgrep' on the base directory of an ASDF system."
      (interactive (progn (grep-compute-defaults)
                          (list (slclj-read-system-name nil nil t)
                                (grep-read-regexp))))
      (rgrep regexp "*.lisp"
             (slclj-from-lisp-filename
              (slclj-eval `(swank:asdf-system-directory ,sys-name)))))
    (defun slclj-rgrep-system ()
      (interactive)
      (error "This command is only supported on GNU Emacs >21.x.")))

(if (boundp 'multi-isearch-next-buffer-function)
    (defun slclj-isearch-system (sys-name)
      "Run `isearch-forward' on the files of an ASDF system."
      (interactive (list (slclj-read-system-name nil nil t)))
      (let* ((files (mapcar 'slclj-from-lisp-filename
                            (slclj-eval `(swank:asdf-system-files ,sys-name))))
             (multi-isearch-next-buffer-function
              (lexical-let* 
                  ((buffers-forward  (mapcar #'find-file-noselect files))
                   (buffers-backward (reverse buffers-forward)))
                #'(lambda (current-buffer wrap)
                    ;; Contrarily to the the docstring of
                    ;; `multi-isearch-next-buffer-function', the first
                    ;; arg is not necessarily a buffer. Report sent
                    ;; upstream. (2009-11-17)
                    (setq current-buffer (or current-buffer (current-buffer)))
                    (let* ((buffers (if isearch-forward
                                        buffers-forward
                                        buffers-backward)))
                      (if wrap
                          (car buffers)
                          (second (memq current-buffer buffers))))))))
        (isearch-forward)))
    (defun slclj-isearch-system ()
      (interactive)
      (error "This command is only supported on GNU Emacs >23.1.x.")))

(defun slclj-read-query-replace-args (format-string &rest format-args)
  (let* ((minibuffer-setup-hook (slclj-minibuffer-setup-hook))
         (minibuffer-local-map slclj-minibuffer-map)
         (common (query-replace-read-args (apply #'format format-string
                                                 format-args)
                                          t t)))
    (list (nth 0 common) (nth 1 common) (nth 2 common))))

(defun slclj-query-replace-system (name from to &optional delimited)
  "Run `query-replace' on an ASDF system."
  (interactive (let ((system (slclj-read-system-name nil nil t)))
                 (cons system (slclj-read-query-replace-args
                               "Query replace throughout `%s'" system))))
  (condition-case c
      ;; `tags-query-replace' actually uses `query-replace-regexp'
      ;; internally.
      (tags-query-replace (regexp-quote from) to delimited
                          '(mapcar 'slclj-from-lisp-filename
                            (slclj-eval `(swank:asdf-system-files ,name))))
    (error
     ;; Kludge: `tags-query-replace' does not actually return but
     ;; signals an unnamed error with the below error
     ;; message. (<=23.1.2, at least.)
     (unless (string-equal (error-message-string c) "All files processed")
       (signal (car c) (cdr c)))        ; resignal
     t)))

(defun slclj-query-replace-system-and-dependents
    (name from to &optional delimited)
  "Run `query-replace' on an ASDF system and all the systems
depending on it."
  (interactive (let ((system (slclj-read-system-name nil nil t)))
                 (cons system (slclj-read-query-replace-args
                               "Query replace throughout `%s'+dependencies"
                               system))))
  (slclj-query-replace-system name from to delimited)
  (dolist (dep (slclj-who-depends-on-rpc name))
    (when (y-or-n-p (format "Descend into system `%s'? " dep))
      (slclj-query-replace-system dep from to delimited))))

(defun slclj-delete-system-fasls (name)
  "Delete FASLs produced by compiling a system."
  (interactive (list (slclj-read-system-name)))
  (slclj-repl-shortcut-eval-async
   `(swank:delete-system-fasls ,name)
   'message))

(defun slclj-reload-system (system)
  "Reload an ASDF system without reloading its dependencies."
  (interactive (list (slclj-read-system-name)))
  (slclj-save-some-lisp-buffers)
  (slclj-display-output-buffer)
  (message "Performing ASDF LOAD-OP on system %S" system)
  (slclj-repl-shortcut-eval-async
   `(swank:reload-system ,system)
   #'slclj-compilation-finished))

(defun slclj-who-depends-on (system-name)
  (interactive (list (slclj-read-system-name)))
  (slclj-xref :depends-on system-name))

(defun slclj-save-system (system)
  "Save files belonging to an ASDF system."
  (interactive (list (slclj-read-system-name)))
  (slclj-eval-async
      `(swank:asdf-system-files ,system)
    (lambda (files)
      (dolist (file files)
        (let ((buffer (get-file-buffer (slclj-from-lisp-filename file))))
          (when buffer
            (with-current-buffer buffer
              (save-buffer buffer)))))
      (message "Done."))))


;;; REPL shortcuts

(defslclj-repl-shortcut slclj-repl-load/force-system ("force-load-system")
  (:handler (lambda ()
              (interactive)
              (slclj-oos (slclj-read-system-name) 'load-op :force t)))
  (:one-liner "Recompile and load an ASDF system."))

(defslclj-repl-shortcut slclj-repl-load-system ("load-system")
  (:handler (lambda ()
              (interactive)
              (slclj-oos (slclj-read-system-name) 'load-op)))
  (:one-liner "Compile (as needed) and load an ASDF system."))

(defslclj-repl-shortcut slclj-repl-test/force-system ("force-test-system")
  (:handler (lambda ()
              (interactive)
              (slclj-oos (slclj-read-system-name) 'test-op :force t)))
  (:one-liner "Compile (as needed) and force test an ASDF system."))

(defslclj-repl-shortcut slclj-repl-test-system ("test-system")
  (:handler (lambda ()
              (interactive)
              (slclj-oos (slclj-read-system-name) 'test-op)))
  (:one-liner "Compile (as needed) and test an ASDF system."))

(defslclj-repl-shortcut slclj-repl-compile-system ("compile-system")
  (:handler (lambda ()
              (interactive)
              (slclj-oos (slclj-read-system-name) 'compile-op)))
  (:one-liner "Compile (but not load) an ASDF system."))

(defslclj-repl-shortcut slclj-repl-compile/force-system 
  ("force-compile-system")  
  (:handler (lambda ()
              (interactive)
              (slclj-oos (slclj-read-system-name) 'compile-op :force t)))
  (:one-liner "Recompile (but not load) an ASDF system."))

(defslclj-repl-shortcut slclj-repl-open-system ("open-system")
  (:handler 'slclj-open-system)
  (:one-liner "Open all files in an ASDF system."))

(defslclj-repl-shortcut slclj-repl-browse-system ("browse-system")
  (:handler 'slclj-browse-system)
  (:one-liner "Browse files in an ASDF system using Dired."))

(defslclj-repl-shortcut slclj-repl-delete-system-fasls ("delete-system-fasls")
  (:handler 'slclj-delete-system-fasls)
  (:one-liner "Delete FASLs of an ASDF system."))

(defslclj-repl-shortcut slclj-repl-reload-system ("reload-system")
  (:handler 'slclj-reload-system)
  (:one-liner "Recompile and load an ASDF system."))


;;; Initialization

(defun slclj-asdf-init ()
  (slclj-require :swank-asdf)
  (add-to-list 'slclj-edit-uses-xrefs :depends-on t)
  (define-key slclj-who-map [?d] 'slclj-who-depends-on))

(defun slclj-asdf-unload ()
  (remove-hook 'slclj-connected-hook 'slclj-asdf-on-connect))

(provide 'slclj-asdf)
