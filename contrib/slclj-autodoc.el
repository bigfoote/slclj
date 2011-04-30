;;; slclj-autodoc.el --- show fancy arglist in echo area
;;
;; Authors: Luke Gorrie  <luke@bluetail.com>
;;          Lawrence Mitchell  <wence@gmx.li>
;;          Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others
;; 
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slclj-load-hook (lambda () (require 'slclj-autodoc)))
;;

(eval-and-compile
  (assert (not (featurep 'xemacs)) ()
	  "slclj-autodoc doesn't work with XEmacs"))

(require 'slclj-parse)

(defcustom slclj-use-autodoc-mode t
  "When non-nil always enable slclj-autodoc-mode in slclj-mode.")

(defcustom slclj-autodoc-use-multiline-p nil
  "If non-nil, allow long autodoc messages to resize echo area display."
  :type 'boolean
  :group 'slclj-ui)

(defcustom slclj-autodoc-delay 0.3
  "*Delay before autodoc messages are fetched and displayed, in seconds."
  :type 'number
  :group 'slclj-ui)

(defcustom slclj-autodoc-accuracy-depth 10
  "Number of paren levels that autodoc takes into account for
  context-sensitive arglist display (local functions. etc)")



(defun slclj-arglist (name)
  "Show the argument list for NAME."
  (interactive (list (slclj-read-symbol-name "Arglist of: " t)))
  (let ((arglist (slclj-retrieve-arglist name)))
    (if (eq arglist :not-available)
        (error "Arglist not available")
        (message "%s" (slclj-fontify-string arglist)))))

(defun slclj-retrieve-arglist (name)
  (let ((name (etypecase name
                 (string name)
                 (symbol (symbol-name name)))))
    (slclj-eval `(swank:autodoc '(,name ,slclj-cursor-marker)))))


;;;; Autodocs (automatic context-sensitive help)

(defun slclj-make-autodoc-rpc-form ()
  "Return a cache key and a swank form."
  (let* ((levels slclj-autodoc-accuracy-depth)
         (buffer-form (slclj-parse-form-upto-point levels)))
    (when buffer-form
      (values buffer-form
              (multiple-value-bind (width height)
                  (slclj-autodoc-message-dimensions)
                `(swank:autodoc ',buffer-form
                                :print-right-margin ,width
                                :print-lines ,height))))))

(defun slclj-autodoc-global-at-point ()
  "Return the global variable name at point, if any."
  (when-let (name (slclj-symbol-at-point))
    (and (slclj-global-variable-name-p name) name)))

(defcustom slclj-global-variable-name-regexp "^\\(.*:\\)?\\([*+]\\).+\\2$"
  "Regexp used to check if a symbol name is a global variable.

Default value assumes +this+ or *that* naming conventions."
  :type 'regexp
  :group 'slclj)

(defun slclj-global-variable-name-p (name)
  "Is NAME a global variable?
Globals are recognised purely by *this-naming-convention*."
  (and (< (length name) 80) ; avoid overflows in regexp matcher
       (string-match slclj-global-variable-name-regexp name)))

(defvar slclj-autodoc-dimensions-function nil)

(defun slclj-autodoc-message-dimensions ()
  "Return the available width and height for pretty printing autodoc
messages."
  (cond
   (slclj-autodoc-dimensions-function
    (funcall slclj-autodoc-dimensions-function))
   (slclj-autodoc-use-multiline-p 
    ;; Use the full width of the minibuffer;
    ;; minibuffer will grow vertically if necessary
    (values (window-width (minibuffer-window))
            nil))
   (t
    ;; Try to fit everything in one line; we cut off when displaying
    (values 1000 1))))


;;;; Autodoc cache

(defvar slclj-autodoc-last-buffer-form nil)
(defvar slclj-autodoc-last-autodoc nil)

(defun slclj-get-cached-autodoc (buffer-form)
  "Return the cached autodoc documentation for `buffer-form', or nil."
  (when (equal buffer-form slclj-autodoc-last-buffer-form)
    slclj-autodoc-last-autodoc))

(defun slclj-store-into-autodoc-cache (buffer-form autodoc)
  "Update the autodoc cache for SYMBOL with DOCUMENTATION.
Return DOCUMENTATION."
  (setq slclj-autodoc-last-buffer-form buffer-form)
  (setq slclj-autodoc-last-autodoc autodoc))


;;;; Formatting autodoc

(defun slclj-format-autodoc (doc)
  (setq doc (slclj-fontify-string doc))
  (unless slclj-autodoc-use-multiline-p
    (setq doc (slclj-oneliner doc)))
  doc)

(defun slclj-fontify-string (string)
  "Fontify STRING as `font-lock-mode' does in Lisp mode."
  (with-current-buffer (get-buffer-create " *slclj-fontify*")
    (erase-buffer)
    (unless (eq major-mode 'lisp-mode)
      ;; Just calling (lisp-mode) will turn slclj-mode on in that buffer,
      ;; which may interfere with this function
      (setq major-mode 'lisp-mode)
      (lisp-mode-variables t))
    (insert string)
    (let ((font-lock-verbose nil))
      (font-lock-fontify-buffer))
    (goto-char (point-min))
    (when (re-search-forward "===> \\(\\(.\\|\n\\)*\\) <===" nil t)
      (let ((highlight (match-string 1)))
        ;; Can't use (replace-match highlight) here -- broken in Emacs 21
        (delete-region (match-beginning 0) (match-end 0))
	(slclj-insert-propertized '(face highlight) highlight)))
    (buffer-substring (point-min) (point-max))))


;;;; slclj-autodoc-mode


(defun slclj-autodoc ()
  "Returns the cached arglist information as string, or nil.
If it's not in the cache, the cache will be updated asynchronously."
  (interactive)
  (save-excursion
    ;; Save match data just in case. This is automatically run in
    ;; background, so it'd be rather disastrous if it touched match
    ;; data.
    (save-match-data
      (unless (slclj-inside-string-or-comment-p)
        (multiple-value-bind (cache-key retrieve-form) 
            (slclj-make-autodoc-rpc-form)
          (let ((cached))
            (cond 
              ((not cache-key) nil)
              ((setq cached (slclj-get-cached-autodoc cache-key)) cached)
              (t
               ;; If nothing is in the cache, we first decline (by
               ;; returning nil), and fetch the arglist information
               ;; asynchronously.
               (slclj-eval-async retrieve-form
                 (lexical-let ((cache-key cache-key))
                   (lambda (doc)
                     (unless (eq doc :not-available) 
                       (setq doc (slclj-format-autodoc doc))
                       ;; Now that we've got our information,
                       ;; get it to the user ASAP.
                       (eldoc-message doc)
                       (slclj-store-into-autodoc-cache cache-key doc)))))
               nil))))))))

(make-variable-buffer-local (defvar slclj-autodoc-mode nil))

(defun slclj-autodoc-mode (&optional arg)
  (interactive (list (or current-prefix-arg 'toggle)))
  (make-local-variable 'eldoc-documentation-function)
  (make-local-variable 'eldoc-idle-delay)
  (make-local-variable 'eldoc-minor-mode-string)
  (setq eldoc-documentation-function 'slclj-autodoc)
  (setq eldoc-idle-delay slclj-autodoc-delay)
  (setq eldoc-minor-mode-string " Autodoc")
  (setq slclj-autodoc-mode (eldoc-mode arg))
  (when (interactive-p)
    (message (format "Slclj autodoc mode %s."
                     (if slclj-autodoc-mode "enabled" "disabled")))))

(defadvice eldoc-display-message-no-interference-p 
    (after slclj-autodoc-message-ok-p)
  (when slclj-autodoc-mode
    (setq ad-return-value
          (and ad-return-value
               ;; Display arglist only when the minibuffer is
               ;; inactive, e.g. not on `C-x C-f'.
               (not (active-minibuffer-window))
               ;; Display arglist only when inferior Lisp will be able
               ;; to cope with the request.
               (slclj-background-activities-enabled-p))))
  ad-return-value)


;;;; Initialization

(defun slclj-autodoc-init ()
  (slclj-require :swank-arglists)
  (dolist (h '(slclj-mode-hook slclj-repl-mode-hook sldb-mode-hook))
    (add-hook h 'slclj-autodoc-maybe-enable)))

(defun slclj-autodoc-maybe-enable ()
  (when slclj-use-autodoc-mode
    (slclj-autodoc-mode 1)
    (setq slclj-echo-arglist-function
          (lambda () 
            (if slclj-autodoc-mode
                (eldoc-message (slclj-autodoc))
                (slclj-show-arglist))))))

;;; FIXME: This doesn't disable eldoc-mode in existing buffers.
(defun slclj-autodoc-unload ()
  (setq slclj-echo-arglist-function 'slclj-show-arglist)
  (dolist (h '(slclj-mode-hook slclj-repl-mode-hook sldb-mode-hook))
    (remove-hook h 'slclj-autodoc-maybe-enable)))

;;;; Test cases

(defun slclj-check-autodoc-at-point (arglist)
  (let ((slclj-autodoc-use-multiline-p nil))
    (slclj-test-expect (format "Autodoc in `%s' (at %d) is as expected" 
                               (buffer-string) (point)) 
                       arglist
                       (slclj-eval (second (slclj-make-autodoc-rpc-form)))
                       'equal)))

(def-slclj-test autodoc.1
    (buffer-sexpr wished-arglist &optional skip-trailing-test-p)
    ""
    '(
      ;; Test basics
      ("(swank::emacs-connected*HERE*"    "(emacs-connected)")
      ("(swank::emacs-connected *HERE*"   "(emacs-connected)")
      ("(swank::create-socket*HERE*"      "(create-socket host port)")
      ("(swank::create-socket *HERE*"     "(create-socket ===> host <=== port)")
      ("(swank::create-socket foo *HERE*" "(create-socket host ===> port <===)")

      ;; Test that autodoc differentiates between exported and unexported symbols.
      ("(swank:create-socket*HERE*" :not-available)

      ;; Test if cursor is on non-existing required parameter
      ("(swank::create-socket foo bar *HERE*" "(create-socket host port)")

      ;; Test cursor in front of opening parenthesis
      ("(swank::with-struct *HERE*(foo. x y) *struct* body1)"
       "(with-struct (conc-name &rest names) obj &body body)"
       t)

      ;; Test variable content display
      ("(progn swank::default-server-port*HERE*" "DEFAULT-SERVER-PORT => 4005")

      ;; Test that "variable content display" is not triggered for trivial constants.
      ("(swank::create-socket t*HERE*"     "(create-socket ===> host <=== port)")
      ("(swank::create-socket :foo*HERE*"  "(create-socket ===> host <=== port)")

      ;; Test with syntactic sugar
      ("#'(lambda () (swank::create-socket*HERE*" "(create-socket host port)")
      ("`(lambda () ,(swank::create-socket*HERE*" "(create-socket host port)")
      ("(remove-if #'(lambda () (swank::create-socket*HERE*"    "(create-socket host port)")
      ("`(remove-if #'(lambda () ,@(swank::create-socket*HERE*" "(create-socket host port)")

      ;; Test &optional
      ("(swank::symbol-status foo *HERE*" 
       "(symbol-status symbol &optional ===> (package (symbol-package symbol)) <===)")

      ;; Test context-sensitive autodoc (DEFMETHOD)
      ("(defmethod swank::arglist-dispatch (*HERE*"
       "(defmethod arglist-dispatch (===> operator <=== arguments) &body body)")
      ("(defmethod swank::arglist-dispatch :before (*HERE*"
       "(defmethod arglist-dispatch :before (===> operator <=== arguments) &body body)")

      ;; Test context-sensitive autodoc (APPLY)
      ("(apply 'swank::eval-for-emacs*HERE*"
       "(apply 'eval-for-emacs &optional form buffer-package id &rest args)")
      ("(apply #'swank::eval-for-emacs*HERE*"
       "(apply #'eval-for-emacs &optional form buffer-package id &rest args)")
      ("(apply 'swank::eval-for-emacs foo *HERE*"
       "(apply 'eval-for-emacs &optional form ===> buffer-package <=== id &rest args)")
      ("(apply #'swank::eval-for-emacs foo *HERE*"
       "(apply #'eval-for-emacs &optional form ===> buffer-package <=== id &rest args)")

      ;; Test context-sensitive autodoc (ERROR, CERROR)
      ("(error 'simple-condition*HERE*"
       "(error 'simple-condition &rest arguments &key format-arguments format-control)")
      ("(cerror \"Foo\" 'simple-condition*HERE*"
       "(cerror \"Foo\" 'simple-condition &rest arguments &key format-arguments format-control)")
      
      ;; Test &KEY and nested arglists
      ("(swank::with-retry-restart (:msg *HERE*"
       "(with-retry-restart (&key ===> (msg \"Retry.\") <===) &body body)")
      ("(swank::start-server \"/tmp/foo\" :coding-system *HERE*"
       "(start-server port-file &key (style swank:*communication-style*) (dont-close swank:*dont-close*) ===> (coding-system swank::*coding-system*) <===)")
      
      ;; Test declarations and type specifiers
      ("(declare (string *HERE*" 
       "(declare (string &rest ===> variables <===))")
      ("(declare ((string *HERE*"
       "(declare ((string &optional ===> size <===) &rest variables))")
      ("(declare (type (string *HERE*"
       "(declare (type (string &optional ===> size <===) &rest variables))")

      ;; Test local functions
      ("(flet ((foo (x y) (+ x y))) (foo *HERE*" "(foo ===> x <=== y)")
      ("(macrolet ((foo (x y) `(+ ,x ,y))) (foo *HERE*" "(foo ===> x <=== y)")
      ("(labels ((foo (x y) (+ x y))) (foo *HERE*" "(foo ===> x <=== y)")
      ("(labels ((foo (x y) (+ x y)) 
                 (bar (y) (foo *HERE*" 
       "(foo ===> x <=== y)"))
  (slclj-check-top-level)
  (with-temp-buffer
    (setq slclj-buffer-package "COMMON-LISP-USER")
    (lisp-mode)
    (insert buffer-sexpr)
    (search-backward "*HERE*")
    (delete-region (match-beginning 0) (match-end 0))
    (slclj-check-autodoc-at-point wished-arglist)
    (unless skip-trailing-test-p
      (insert ")") (backward-char)
      (slclj-check-autodoc-at-point wished-arglist))
    ))

(provide 'slclj-autodoc)
