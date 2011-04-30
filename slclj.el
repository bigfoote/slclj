;;; slclj.el --- Superior Lisp Interaction Mode for Emacs
;;
;;;; License
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;
;;     For a detailed list of contributors, see the manual.
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


;;;; Commentary
;;
;; This file contains extensions for programming in Common Lisp. The
;; main features are:
;;
;;   A socket-based communication/RPC interface between Emacs and
;;   Lisp, enabling introspection and remote development.
;;
;;   The `slclj-mode' minor-mode complementing `lisp-mode'. This new
;;   mode includes many commands for interacting with the Common Lisp
;;   process.
;;
;;   A Common Lisp debugger written in Emacs Lisp. The debugger pops up
;;   an Emacs buffer similar to the Emacs/Elisp debugger.
;;
;;   A Common Lisp inspector to interactively look at run-time data.
;;
;;   Trapping compiler messages and creating annotations in the source
;;   file on the appropriate forms.
;;
;; SLCLJ should work with Emacs 22 and 23.  If it works on XEmacs,
;; consider yourself lucky.
;;
;; In order to run SLCLJ, a supporting Lisp server called Swank is
;; required. Swank is distributed with slclj.el and will automatically
;; be started in a normal installation.


;;;; Dependencies and setup

(eval-and-compile
  (when (<= emacs-major-version 20)
    (error "Slclj requires an Emacs version of 21, or above")))

(eval-and-compile
  (require 'cl)
  (when (locate-library "hyperspec")
    (require 'hyperspec)))
(require 'thingatpt)
(require 'comint)
(require 'timer)
(require 'pp)
(require 'hideshow)
(require 'font-lock)
(when (featurep 'xemacs)
  (require 'overlay))
(require 'easymenu)
(eval-when (compile)
  (require 'arc-mode)
  (require 'apropos)
  (require 'outline)
  (require 'etags)
  (require 'compile)
  (require 'gud))

(eval-and-compile 
  (defvar slclj-path
    (let ((path (or (locate-library "slclj") load-file-name)))
      (and path (file-name-directory path)))
    "Directory containing the Slclj package.
This is used to load the supporting Common Lisp library, Swank.
The default value is automatically computed from the location of the
Emacs Lisp package."))

(defvar slclj-lisp-modes '(lisp-mode))
(defvar slclj-setup-contribs nil)

(defun slclj-setup (&optional contribs)
  "Setup Emacs so that lisp-mode buffers always use SLCLJ.
CONTRIBS is a list of contrib packages to load."
  (when (member 'lisp-mode slclj-lisp-modes)
    (add-hook 'lisp-mode-hook 'slclj-lisp-mode-hook))
  (setq slclj-setup-contribs contribs)
  (slclj-setup-contribs))

(defun slclj-setup-contribs ()
  "Load and initialize contribs."
  (when slclj-setup-contribs
    (add-to-list 'load-path (expand-file-name "contrib" slclj-path))
    (dolist (c slclj-setup-contribs)
      (require c)
      (let ((init (intern (format "%s-init" c))))
        (when (fboundp init)
          (funcall init))))))

(defun slclj-lisp-mode-hook ()
  (slclj-mode 1)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function))

(eval-and-compile
  (defun slclj-changelog-date (&optional interactivep)
    "Return the datestring of the latest entry in the ChangeLog file.
Return nil if the ChangeLog file cannot be found."
    (interactive "p")
    (let ((changelog (concat slclj-path "ChangeLog"))
          (date nil))
      (when (file-exists-p changelog)
        (with-temp-buffer 
          (insert-file-contents-literally changelog nil 0 100)
          (goto-char (point-min))
          (setq date (symbol-name (read (current-buffer))))))
      (when interactivep
        (message "Slclj ChangeLog dates %s." date))
      date)))

(defvar slclj-protocol-version nil)
(setq slclj-protocol-version
      (eval-when-compile (slclj-changelog-date)))


;;;; Customize groups
;;
;;;;; slclj

(defgroup slclj nil
  "Interaction with the Superior Lisp Environment."
  :prefix "slclj-"
  :group 'applications)

;;;;; slclj-ui

(defgroup slclj-ui nil
  "Interaction with the Superior Lisp Environment."
  :prefix "slclj-"
  :group 'slclj)

(defcustom slclj-truncate-lines t
  "Set `truncate-lines' in popup buffers.
This applies to buffers that present lines as rows of data, such as
debugger backtraces and apropos listings."
  :type 'boolean
  :group 'slclj-ui)

(defcustom slclj-kill-without-query-p nil
  "If non-nil, kill SLCLJ processes without query when quitting Emacs.
This applies to the *inferior-lisp* buffer and the network connections."
  :type 'boolean
  :group 'slclj-ui)

;;;;; slclj-lisp

(defgroup slclj-lisp nil
  "Lisp server configuration."
  :prefix "slclj-"
  :group 'slclj)

(defcustom slclj-backend "swank-loader.lisp"
  "The name of the Lisp file that loads the Swank server.
This name is interpreted relative to the directory containing
slclj.el, but could also be set to an absolute filename."
  :type 'string
  :group 'slclj-lisp)

(defcustom slclj-connected-hook nil
  "List of functions to call when SLCLJ connects to Lisp."
  :type 'hook
  :group 'slclj-lisp)

(defcustom slclj-enable-evaluate-in-emacs nil
  "*If non-nil, the inferior Lisp can evaluate arbitrary forms in Emacs.
The default is nil, as this feature can be a security risk."
  :type '(boolean)
  :group 'slclj-lisp)

(defcustom slclj-lisp-host "127.0.0.1"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'slclj-lisp)

(defcustom slclj-port 4005
  "Port to use as the default for `slclj-connect'."
  :type 'integer
  :group 'slclj-lisp)

(defvar slclj-net-valid-coding-systems
  '((iso-latin-1-unix nil "iso-latin-1-unix")
    (iso-8859-1-unix  nil "iso-latin-1-unix")
    (binary           nil "iso-latin-1-unix")
    (utf-8-unix       t   "utf-8-unix")
    (emacs-mule-unix  t   "emacs-mule-unix")
    (euc-jp-unix      t   "euc-jp-unix"))
  "A list of valid coding systems. 
Each element is of the form: (NAME MULTIBYTEP CL-NAME)")

(defun slclj-find-coding-system (name)
  "Return the coding system for the symbol NAME.
The result is either an element in `slclj-net-valid-coding-systems'
of nil."
  (let ((probe (assq name slclj-net-valid-coding-systems)))
    (when (and probe (if (fboundp 'check-coding-system)
                         (ignore-errors (check-coding-system (car probe)))
                         (eq (car probe) 'binary)))
      probe)))

(defcustom slclj-net-coding-system
  (car (find-if 'slclj-find-coding-system
                slclj-net-valid-coding-systems :key 'car))
  "Coding system used for network connections.
See also `slclj-net-valid-coding-systems'."
  :type (cons 'choice
              (mapcar (lambda (x)
                        (list 'const (car x)))
                      slclj-net-valid-coding-systems))
  :group 'slclj-lisp)

;;;;; slclj-mode

(defgroup slclj-mode nil
  "Settings for slclj-mode Lisp source buffers."
  :prefix "slclj-"
  :group 'slclj)

(defcustom slclj-find-definitions-function 'slclj-find-definitions-rpc
  "Function to find definitions for a name.
The function is called with the definition name, a string, as its
argument."
  :type 'function
  :group 'slclj-mode
  :options '(slclj-find-definitions-rpc
             slclj-etags-definitions
             (lambda (name)
               (append (slclj-find-definitions-rpc name)
                       (slclj-etags-definitions name)))
             (lambda (name)
               (or (slclj-find-definitions-rpc name)
                   (and tags-table-list
                        (slclj-etags-definitions name))))))

(defcustom slclj-complete-symbol-function 'slclj-simple-complete-symbol
  "*Function to perform symbol completion."
  :group 'slclj-mode
  :type '(choice (const :tag "Simple" slclj-simple-complete-symbol)
                 (const :tag "Compound" slclj-complete-symbol*)
                 (const :tag "Fuzzy" slclj-fuzzy-complete-symbol)))

;;;;; slclj-mode-faces

(defgroup slclj-mode-faces nil
  "Faces in slclj-mode source code buffers."
  :prefix "slclj-"
  :group 'slclj-mode)

(defun slclj-underline-color (color)
  "Return a legal value for the :underline face attribute based on COLOR."
  ;; In XEmacs the :underline attribute can only be a boolean.
  ;; In GNU it can be the name of a colour.
  (if (featurep 'xemacs)
      (if color t nil)
    color))

(defface slclj-error-face
  `((((class color) (background light))
     (:underline ,(slclj-underline-color "red")))
    (((class color) (background dark))
     (:underline ,(slclj-underline-color "red")))
    (t (:underline t)))
  "Face for errors from the compiler."
  :group 'slclj-mode-faces)

(defface slclj-warning-face
  `((((class color) (background light))
     (:underline ,(slclj-underline-color "orange")))
    (((class color) (background dark))
     (:underline ,(slclj-underline-color "coral")))
    (t (:underline t)))
  "Face for warnings from the compiler."
  :group 'slclj-mode-faces)

(defface slclj-style-warning-face
  `((((class color) (background light))
     (:underline ,(slclj-underline-color "brown")))
    (((class color) (background dark))
     (:underline ,(slclj-underline-color "gold")))
    (t (:underline t)))
  "Face for style-warnings from the compiler."
  :group 'slclj-mode-faces)

(defface slclj-note-face
  `((((class color) (background light))
     (:underline ,(slclj-underline-color "brown4")))
    (((class color) (background dark))
     (:underline ,(slclj-underline-color "light goldenrod")))
    (t (:underline t)))
  "Face for notes from the compiler."
  :group 'slclj-mode-faces)

(defun slclj-face-inheritance-possible-p ()
  "Return true if the :inherit face attribute is supported." 
  (assq :inherit custom-face-attributes))

(defface slclj-highlight-face
  (if (slclj-face-inheritance-possible-p)
      '((t (:inherit highlight :underline nil)))
    '((((class color) (background light))
       (:background "darkseagreen2"))
      (((class color) (background dark))
       (:background "darkolivegreen"))
      (t (:inverse-video t))))
  "Face for compiler notes while selected."
  :group 'slclj-mode-faces)

;;;;; sldb

(defgroup slclj-debugger nil
  "Backtrace options and fontification."
  :prefix "sldb-"
  :group 'slclj)

(defmacro define-sldb-faces (&rest faces)
  "Define the set of SLDB faces.
Each face specifiation is (NAME DESCRIPTION &optional PROPERTIES).
NAME is a symbol; the face will be called sldb-NAME-face.
DESCRIPTION is a one-liner for the customization buffer.
PROPERTIES specifies any default face properties."
  `(progn ,@(loop for face in faces
                  collect `(define-sldb-face ,@face))))

(defmacro define-sldb-face (name description &optional default)
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name)))))
    `(defface ,facename
       (list (list t ,default))
      ,(format "Face for %s." description)
      :group 'slclj-debugger)))

(define-sldb-faces
  (topline        "the top line describing the error")
  (condition      "the condition class")
  (section        "the labels of major sections in the debugger buffer")
  (frame-label    "backtrace frame numbers")
  (restart-type   "restart names."
                  (if (slclj-face-inheritance-possible-p)
                      '(:inherit font-lock-keyword-face)))
  (restart        "restart descriptions")
  (restart-number "restart numbers (correspond to keystrokes to invoke)"
                  '(:bold t))
  (frame-line     "function names and arguments in the backtrace")
  (restartable-frame-line
   "frames which are surely restartable"
   '(:foreground "lime green"))
  (non-restartable-frame-line
   "frames which are surely not restartable")
  (detailed-frame-line
   "function names and arguments in a detailed (expanded) frame")
  (local-name     "local variable names")
  (local-value    "local variable values")
  (catch-tag      "catch tags"))


;;;; Minor modes

;;;;; slclj-mode

(defvar slclj-mode-indirect-map (make-sparse-keymap)
  "Empty keymap which has `slclj-mode-map' as it's parent.
This is a hack so that we can reinitilize the real slclj-mode-map
more easily. See `slclj-init-keymaps'.")

(define-minor-mode slclj-mode
  "\\<slclj-mode-map>\
SLCLJ: The Superior Lisp Interaction Mode for Emacs (minor-mode).

Commands to compile the current buffer's source file and visually
highlight any resulting compiler notes and warnings:
\\[slclj-compile-and-load-file]	- Compile and load the current buffer's file.
\\[slclj-compile-file]	- Compile (but not load) the current buffer's file.
\\[slclj-compile-defun]	- Compile the top-level form at point.

Commands for visiting compiler notes:
\\[slclj-next-note]	- Goto the next form with a compiler note.
\\[slclj-previous-note]	- Goto the previous form with a compiler note.
\\[slclj-remove-notes]	- Remove compiler-note annotations in buffer.

Finding definitions:
\\[slclj-edit-definition]	- Edit the definition of the function called at point.
\\[slclj-pop-find-definition-stack]	- Pop the definition stack to go back from a definition.

Documentation commands:
\\[slclj-describe-symbol]	- Describe symbol.
\\[slclj-apropos]	- Apropos search.
\\[slclj-disassemble-symbol]	- Disassemble a function.

Evaluation commands:
\\[slclj-eval-defun]	- Evaluate top-level from containing point.
\\[slclj-eval-last-expression]	- Evaluate sexp before point.
\\[slclj-pprint-eval-last-expression]	- Evaluate sexp before point, pretty-print result.

Full set of commands:
\\{slclj-mode-map}"
  nil
  nil
  slclj-mode-indirect-map
  (slclj-setup-command-hooks)
  (slclj-recompute-modelines))


;;;;;; Modeline

;; For XEmacs only
(make-variable-buffer-local
 (defvar slclj-modeline-string nil
   "The string that should be displayed in the modeline."))

(add-to-list 'minor-mode-alist
             `(slclj-mode ,(if (featurep 'xemacs)
                               'slclj-modeline-string
                             '(:eval (slclj-modeline-string)))))

(defun slclj-modeline-string ()
  "Return the string to display in the modeline.
\"Slclj\" only appears if we aren't connected.  If connected,
include package-name, connection-name, and possibly some state
information."
  (let ((conn (slclj-current-connection)))
    ;; Bail out early in case there's no connection, so we won't
    ;; implicitly invoke `slclj-connection' which may query the user.
    (if (not conn)
        (and slclj-mode " Slclj")
        (let ((local (eq conn slclj-buffer-connection))
              (pkg   (slclj-current-package)))
          (concat " "
                  (if local "{" "[")
                  (if pkg (slclj-pretty-package-name pkg) "?")
                  " "
                  ;; ignore errors for closed connections
                  (ignore-errors (slclj-connection-name conn))
                  (slclj-modeline-state-string conn)
                  (if local "}" "]"))))))

(defun slclj-pretty-package-name (name)
  "Return a pretty version of a package name NAME."
  (cond ((string-match "^#?:\\(.*\\)$" name)    
         (match-string 1 name))
        ((string-match "^\"\\(.*\\)\"$" name) 
         (match-string 1 name))
        (t name)))

(defun slclj-modeline-state-string (conn)
  "Return a string possibly describing CONN's state."
  (cond ((not (eq (process-status conn) 'open))
         (format " %s" (process-status conn)))
        ((let ((pending (length (slclj-rex-continuations conn)))
               (sldbs (length (sldb-buffers conn))))
           (cond ((and (zerop sldbs) (zerop pending)) nil)
                 ((zerop sldbs) (format " %s" pending))
                 (t (format " %s/%s" pending sldbs)))))))

(defun slclj-recompute-modelines ()
  (when (featurep 'xemacs)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (or slclj-mode slclj-popup-buffer-mode)
          (setq slclj-modeline-string (slclj-modeline-string)))))
    (force-mode-line-update t)))


;;;;; Key bindings

(defvar slclj-parent-map nil
  "Parent keymap for shared between all Slclj related modes.")

(defvar slclj-parent-bindings
  '(("\M-."      slclj-edit-definition)
    ("\M-,"      slclj-pop-find-definition-stack)
    ("\M-_"      slclj-edit-uses)    ; for German layout
    ("\M-?"      slclj-edit-uses)    ; for USian layout
    ("\C-x4." 	 slclj-edit-definition-other-window)
    ("\C-x5." 	 slclj-edit-definition-other-frame)
    ("\C-x\C-e"  slclj-eval-last-expression)
    ("\C-\M-x"   slclj-eval-defun)
    ;; Include PREFIX keys...
    ("\C-c"	 slclj-prefix-map)))

(defvar slclj-prefix-map nil
  "Keymap for commands prefixed with `slclj-prefix-key'.")

(defvar slclj-prefix-bindings
  '(("\C-r"  slclj-eval-region)
    (":"     slclj-interactive-eval)
    ("\C-e"  slclj-interactive-eval)
    ("E"     slclj-edit-value)
    ("\C-l"  slclj-load-file)
    ("\C-b"  slclj-interrupt)
    ("\M-d"  slclj-disassemble-symbol)
    ("\C-t"  slclj-toggle-trace-fdefinition)
    ("I"     slclj-inspect)
    ("\C-xt" slclj-list-threads)
    ("\C-xn" slclj-cycle-connections)
    ("\C-xc" slclj-list-connections)
    ("<"     slclj-list-callers)
    (">"     slclj-list-callees)
    ;; Include DOC keys...
    ("\C-d"  slclj-doc-map)
    ;; Include XREF WHO-FOO keys...
    ("\C-w"  slclj-who-map)
    ))

(defvar slclj-editing-map nil
  "These keys are useful for buffers where the user can insert and
edit s-exprs, e.g. for source buffers and the REPL.")

(defvar slclj-editing-keys
  `(;; Arglist display & completion
    ("\M-\t"      slclj-complete-symbol)
    (" "          slclj-space)
    ;; Evaluating
    ;;("\C-x\M-e" slclj-eval-last-expression-display-output :inferior t)
    ("\C-c\C-p"   slclj-pprint-eval-last-expression)
    ;; Macroexpand
    ("\C-c\C-m"   slclj-macroexpand-1)
    ("\C-c\M-m"   slclj-macroexpand-all)
    ;; Misc
    ("\C-c\C-u"   slclj-undefine-function)
    (,(kbd "C-M-.")   slclj-next-location)
    (,(kbd "C-M-,")   slclj-previous-location)
    ;; Obsolete, redundant bindings
    ("\C-c\C-i" slclj-complete-symbol)
    ;;("\M-*" pop-tag-mark) ; almost to clever
    ))

(defvar slclj-mode-map nil
  "Keymap for slclj-mode.")

(defvar slclj-keys
  '( ;; Compiler notes
    ("\M-p"       slclj-previous-note)
    ("\M-n"       slclj-next-note)
    ("\C-c\M-c"   slclj-remove-notes)
    ("\C-c\C-k"   slclj-compile-and-load-file)
    ("\C-c\M-k"   slclj-compile-file)
    ("\C-c\C-c"   slclj-compile-defun)))

(defun slclj-nop ()
  "The null command. Used to shadow currently-unused keybindings."
  (interactive)
  (call-interactively 'undefined))

(defvar slclj-doc-map nil
  "Keymap for documentation commands. Bound to a prefix key.")

(defvar slclj-doc-bindings
  '((?a slclj-apropos)
    (?z slclj-apropos-all)
    (?p slclj-apropos-package)
    (?d slclj-describe-symbol)
    (?f slclj-describe-function)
    (?h slclj-documentation-lookup)
    (?~ common-lisp-hyperspec-format)
    (?# common-lisp-hyperspec-lookup-reader-macro)))
  
(defvar slclj-who-map nil
  "Keymap for who-xref commands. Bound to a prefix key.")

(defvar slclj-who-bindings
  '((?c slclj-who-calls)
    (?w slclj-calls-who)
    (?r slclj-who-references)
    (?b slclj-who-binds)
    (?s slclj-who-sets)
    (?m slclj-who-macroexpands)
    (?a slclj-who-specializes)))

(defun slclj-init-keymaps ()
  "(Re)initialize the keymaps for `slclj-mode'."
  (interactive)
  (slclj-init-keymap 'slclj-doc-map t t slclj-doc-bindings)
  (slclj-init-keymap 'slclj-who-map t t slclj-who-bindings)
  (slclj-init-keymap 'slclj-prefix-map t nil slclj-prefix-bindings)
  (slclj-init-keymap 'slclj-parent-map nil nil slclj-parent-bindings)
  (slclj-init-keymap 'slclj-editing-map nil nil slclj-editing-keys)
  (set-keymap-parent slclj-editing-map slclj-parent-map)
  (slclj-init-keymap 'slclj-mode-map nil nil slclj-keys)
  (set-keymap-parent slclj-mode-map slclj-editing-map)
  (set-keymap-parent slclj-mode-indirect-map slclj-mode-map))

(defun slclj-init-keymap (keymap-name prefixp bothp bindings)
  (set keymap-name (make-sparse-keymap))
  (when prefixp (define-prefix-command keymap-name))
  (slclj-bind-keys (eval keymap-name) bothp bindings))

(defun slclj-bind-keys (keymap bothp bindings)
  "Add BINDINGS to KEYMAP.
If BOTHP is true also add bindings with control modifier."
  (loop for (key command) in bindings do
        (cond (bothp
               (define-key keymap `[,key] command)
               (unless (equal key ?h)     ; But don't bind C-h
                 (define-key keymap `[(control ,key)] command)))
              (t (define-key keymap key command)))))

(slclj-init-keymaps)

(define-minor-mode slclj-editing-mode
  "Minor mode which makes slclj-editing-map available.
\\{slclj-editing-map}"
  nil
  nil
  slclj-editing-map)


;;;; Setup initial `slclj-mode' hooks

(make-variable-buffer-local
 (defvar slclj-pre-command-actions nil
   "List of functions to execute before the next Emacs command.
This list of flushed between commands."))

(defun slclj-pre-command-hook ()
  "Execute all functions in `slclj-pre-command-actions', then NIL it."
  (dolist (undo-fn slclj-pre-command-actions)
    (funcall undo-fn))
  (setq slclj-pre-command-actions nil))

(defun slclj-post-command-hook ()
  (when (null pre-command-hook) ; sometimes this is lost
    (add-hook 'pre-command-hook 'slclj-pre-command-hook)))

(defun slclj-setup-command-hooks ()
  "Setup a buffer-local `pre-command-hook' to call `slclj-pre-command-hook'."
  (slclj-add-local-hook 'pre-command-hook 'slclj-pre-command-hook)
  (slclj-add-local-hook 'post-command-hook 'slclj-post-command-hook))


;;;; Framework'ey bits
;;;
;;; This section contains some standard SLCLJ idioms: basic macros,
;;; ways of showing messages to the user, etc. All the code in this
;;; file should use these functions when applicable.
;;;
;;;;; Syntactic sugar

(defmacro* when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY.

\(fn (VAR VALUE) &rest BODY)"
  `(let ((,var ,value))
     (when ,var ,@body)))

(put 'when-let 'lisp-indent-function 1)

(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
	 ,@(mapcar (lambda (clause)
                     (if (eq (car clause) t)
                         `(t ,@(cdr clause))
                       (destructuring-bind ((op &rest rands) &rest body) clause
                         `(,op (destructuring-bind ,rands ,operands
                                 . ,body)))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "Elisp destructure-case failed: %S" ,tmp))))))))

(put 'destructure-case 'lisp-indent-function 1)

(defmacro slclj-define-keys (keymap &rest key-command)
  "Define keys in KEYMAP. Each KEY-COMMAND is a list of (KEY COMMAND)."
  `(progn . ,(mapcar (lambda (k-c) `(define-key ,keymap . ,k-c))
		     key-command)))

(put 'slclj-define-keys 'lisp-indent-function 1)

(defmacro* with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (flet ((reader (slot) (intern (concat (symbol-name conc-name)
					(symbol-name slot)))))
    (let ((struct-var (gensym "struct")))
      `(let ((,struct-var ,struct))
	 (symbol-macrolet
	     ,(mapcar (lambda (slot)
			(etypecase slot
			  (symbol `(,slot (,(reader slot) ,struct-var)))
			  (cons `(,(first slot) (,(reader (second slot)) 
						 ,struct-var)))))
		      slots)
	   . ,body)))))

(put 'with-struct 'lisp-indent-function 2)

;;;;; Very-commonly-used functions

(defvar slclj-message-function 'message)

;; Interface
(defun slclj-message (format &rest args)
  "Like `message' but with special support for multi-line messages.
Single-line messages use the echo area."
  (apply slclj-message-function format args))

(defun slclj-display-warning (message &rest args)
  (display-warning '(slclj warning) (apply #'format message args)))

(defvar slclj-background-message-function 'slclj-display-oneliner)

;; Interface
(defun slclj-background-message (format-string &rest format-args)
  "Display a message in passing.
This is like `slclj-message', but less distracting because it
will never pop up a buffer or display multi-line messages.
It should be used for \"background\" messages such as argument lists."
  (apply slclj-background-message-function format-string format-args))

(defun slclj-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (message  "%s" (slclj-oneliner msg)))))

(defun slclj-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
                           (or (position ?\n string) most-positive-fixnum)
                           (1- (frame-width)))))

;; Interface
(defun slclj-set-truncate-lines ()
  "Apply `slclj-truncate-lines' to the current buffer."
  (when slclj-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

;; Interface
(defun slclj-read-package-name (prompt &optional initial-value)
  "Read a package name from the minibuffer, prompting with PROMPT."
  (let ((completion-ignore-case t))
    (completing-read prompt (slclj-bogus-completion-alist 
                             (slclj-eval 
                              `(swank:list-all-package-names t)))
		     nil t initial-value)))

(defun slclj-read-connection (prompt &optional initial-value)
  "Read a connection from the minibuffer. Returns the net
process, or nil."
  (assert (memq initial-value slclj-net-processes))
  (flet ((connection-identifier (p)
           (format "%s (pid %d)" (slclj-connection-name p) (slclj-pid p))))
    (let ((candidates (mapcar #'(lambda (p)
                                  (cons (connection-identifier p) p))
                              slclj-net-processes)))
      (cdr (assoc (completing-read prompt candidates 
                                   nil t (connection-identifier initial-value))
                  candidates)))))

;; Interface
(defun slclj-read-symbol-name (prompt &optional query)
  "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil."
  (cond ((or current-prefix-arg query (not (slclj-symbol-at-point)))
         (slclj-read-from-minibuffer prompt (slclj-symbol-at-point)))
        (t (slclj-symbol-at-point))))

;; Interface
(defmacro slclj-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(put 'slclj-propertize-region 'lisp-indent-function 1)

(defun slclj-add-face (face string)
  (add-text-properties 0 (length string) (list 'face face) string)
  string)

(put 'slclj-add-face 'lisp-indent-function 1)

;; Interface
(defsubst slclj-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (slclj-propertize-region props (apply #'insert args)))

(defmacro slclj-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (let ((start (gensym)) (l (gensym)))
    `(let ((,start (point)) (,l ,(or level '(current-column))))
       (prog1 (progn ,@body)
         (slclj-indent-rigidly ,start (point) ,l)))))

(put 'slclj-with-rigid-indentation 'lisp-indent-function 1)

(defun slclj-indent-rigidly (start end column)
  ;; Similar to `indent-rigidly' but doesn't inherit text props.
  (let ((indent (make-string column ?\ )))
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (while (and (<= start (point))
                  (progn
                    (insert-before-markers indent)
                    (zerop (forward-line -1))))))))

(defun slclj-insert-indented (&rest strings)
  "Insert all arguments rigidly indented."
  (slclj-with-rigid-indentation nil
    (apply #'insert strings)))

(defun slclj-property-bounds (prop)
  "Return two the positions of the previous and next changes to PROP.
PROP is the name of a text property."
  (assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun slclj-curry (fun &rest args)
  "Partially apply FUN to ARGS.  The result is a new function.
This idiom is preferred over `lexical-let'."
  `(lambda (&rest more) (apply ',fun (append ',args more))))

(defun slclj-rcurry (fun &rest args)
  "Like `slclj-curry' but ARGS on the right are applied."
  `(lambda (&rest more) (apply ',fun (append more ',args))))


;;;;; Temporary popup buffers

(defvar slclj-popup-restore-data nil
  "Data needed when closing popup windows.
This is used as buffer local variable.
The format is (POPUP-WINDOW SELECTED-WINDOW OLD-BUFFER).
POPUP-WINDOW is the window used to display the temp buffer.
That window may have been reused or freshly created.
SELECTED-WINDOW is the window that was selected before displaying
the popup buffer.
OLD-BUFFER is the buffer that was previously displayed in POPUP-WINDOW.
OLD-BUFFER is nil if POPUP-WINDOW was newly created.

See `view-return-to-alist' for a similar idea.")

;; keep compiler quiet
(defvar slclj-buffer-package)
(defvar slclj-buffer-connection)

;; Interface
(defmacro* slclj-with-popup-buffer ((name &optional package connection select)
                                    &body body)
  "Similar to `with-output-to-temp-buffer'.
Bind standard-output and initialize some buffer-local variables.
Restore window configuration when closed.

NAME is the name of the buffer to be created.
PACKAGE is the value `slclj-buffer-package'.
CONNECTION is the value for `slclj-buffer-connection'.
If nil, no explicit connection is associated with
the buffer.  If t, the current connection is taken.
"
  `(let* ((vars% (list ,(if (eq package t) '(slclj-current-package) package)
                       ,(if (eq connection t) '(slclj-connection) connection)))
          (standard-output (slclj-make-popup-buffer ,name vars%)))
     (with-current-buffer standard-output
       (prog1 (progn ,@body)
         (assert (eq (current-buffer) standard-output))
         (slclj-init-popup-buffer vars%)
         (setq buffer-read-only t)
         (set-window-point (slclj-display-popup-buffer ,(or select 'nil))
                           (point))))))

(put 'slclj-with-popup-buffer 'lisp-indent-function 1)

(defun slclj-make-popup-buffer (name buffer-vars)
  "Return a temporary buffer called NAME.
The buffer also uses the minor-mode `slclj-popup-buffer-mode'."
  (with-current-buffer (get-buffer-create name)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-syntax-table lisp-mode-syntax-table)
    (slclj-init-popup-buffer buffer-vars)
    (current-buffer)))

(defun slclj-init-popup-buffer (buffer-vars)
  (slclj-popup-buffer-mode 1)
  (multiple-value-setq (slclj-buffer-package slclj-buffer-connection)
    buffer-vars))

(defun slclj-display-popup-buffer (select)
  "Display the current buffer.
Save the selected-window in a buffer-local variable, so that we
can restore it later."
  (let ((selected-window (selected-window))
        (old-windows))
    (walk-windows (lambda (w) (push (cons w (window-buffer w)) old-windows))
                  nil t)
    (let ((new-window (display-buffer (current-buffer))))
      (unless slclj-popup-restore-data
        (set (make-local-variable 'slclj-popup-restore-data)
             (list new-window
                   selected-window
                   (cdr (find new-window old-windows :key #'car)))))
      (when select
        (select-window new-window))
      new-window)))

(defun slclj-close-popup-window ()
  (when slclj-popup-restore-data
    (destructuring-bind (popup-window selected-window old-buffer)
        slclj-popup-restore-data
      (bury-buffer)
      (when (eq popup-window (selected-window))
        (cond ((and (not old-buffer) (not (one-window-p)))
               (delete-window popup-window))
              ((and old-buffer (buffer-live-p old-buffer))
               (set-window-buffer popup-window old-buffer))))
      (when (window-live-p selected-window)
        (select-window selected-window)))
    (kill-local-variable 'slclj-popup-restore-data)))

(defmacro slclj-save-local-variables (vars &rest body)
  (let ((vals (make-symbol "vals")))
  `(let ((,vals (mapcar (lambda (var)
                          (if (slclj-local-variable-p var)
                              (cons var (eval var))))
                        ',vars)))
     (prog1 (progn . ,body)
       (mapc (lambda (var+val)
               (when (consp var+val)
                 (set (make-local-variable (car var+val)) (cdr var+val))))
             ,vals)))))

(put 'slclj-save-local-variables 'lisp-indent-function 1)

(define-minor-mode slclj-popup-buffer-mode 
  "Mode for displaying read only stuff"
  nil
  nil
  '(("q" . slclj-popup-buffer-quit-function)
    ;;("\C-c\C-z" . slclj-switch-to-output-buffer)
    ("\M-." . slclj-edit-definition)))

(add-to-list 'minor-mode-alist
             `(slclj-popup-buffer-mode
               ,(if (featurep 'xemacs)
                    'slclj-modeline-string
                    '(:eval (unless slclj-mode
                              (slclj-modeline-string))))))

(set-keymap-parent slclj-popup-buffer-mode-map slclj-parent-map)

(make-variable-buffer-local
 (defvar slclj-popup-buffer-quit-function 'slclj-popup-buffer-quit
   "The function that is used to quit a temporary popup buffer."))

(defun slclj-popup-buffer-quit-function (&optional kill-buffer-p)
  "Wrapper to invoke the value of `slclj-popup-buffer-quit-function'."
  (interactive)
  (funcall slclj-popup-buffer-quit-function kill-buffer-p))

;; Interface
(defun slclj-popup-buffer-quit (&optional kill-buffer-p)
  "Get rid of the current (temp) buffer without asking.
Restore the window configuration unless it was changed since we
last activated the buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (slclj-close-popup-window)
    (when kill-buffer-p
      (kill-buffer buffer))))

;;;;; Filename translation
;;;
;;; Filenames passed between Emacs and Lisp should be translated using
;;; these functions. This way users who run Emacs and Lisp on separate
;;; machines have a chance to integrate file operations somehow.

(defvar slclj-to-lisp-filename-function #'convert-standard-filename
  "Function to translate Emacs filenames to CL namestrings.")
(defvar slclj-from-lisp-filename-function #'identity
  "Function to translate CL namestrings to Emacs filenames.")

(defun slclj-to-lisp-filename (filename)
  "Translate the string FILENAME to a Lisp filename."
  (funcall slclj-to-lisp-filename-function filename))

(defun slclj-from-lisp-filename (filename)
  "Translate the Lisp filename FILENAME to an Emacs filename."
  (funcall slclj-from-lisp-filename-function filename))


;;;; Starting SLCLJ
;;;
;;; This section covers starting an inferior-lisp, compiling and
;;; starting the server, initiating a network connection.

;;;;; Entry points

;; We no longer load inf-lisp, but we use this variable for backward
;; compatibility.
(defvar inferior-lisp-program "lisp" 
  "*Program name for invoking an inferior Lisp with for Inferior Lisp mode.")

(defvar slclj-lisp-implementations nil
  "*A list of known Lisp implementations.
The list should have the form: 
  ((NAME (PROGRAM PROGRAM-ARGS...) &key KEYWORD-ARGS) ...)

NAME is a symbol for the implementation.
PROGRAM and PROGRAM-ARGS are strings used to start the Lisp process.
For KEYWORD-ARGS see `slclj-start'.

Here's an example: 
 ((cmucl (\"/opt/cmucl/bin/lisp\" \"-quiet\") :init slclj-init-command)
  (acl (\"acl7\") :coding-system emacs-mule))")

(defvar slclj-default-lisp nil
  "*The name of the default Lisp implementation.
See `slclj-lisp-implementations'")

;; dummy definitions for the compiler
(defvar slclj-net-processes)
(defvar slclj-default-connection)

(defun slclj (&optional command coding-system)
  "Start an inferior^_superior Lisp and connect to its Swank server."
  (interactive)
  (let ((inferior-lisp-program (or command inferior-lisp-program))
        (slclj-net-coding-system (or coding-system slclj-net-coding-system)))
    (slclj-start* (cond ((and command (symbolp command))
                         (slclj-lisp-options command))
                        (t (slclj-read-interactive-args))))))

(defvar slclj-inferior-lisp-program-history '()
  "History list of command strings.  Used by `slclj'.")
                                                  
(defun slclj-read-interactive-args ()
  "Return the list of args which should be passed to `slclj-start'.

The rules for selecting the arguments are rather complicated:

- In the most common case, i.e. if there's no prefix-arg in
  effect and if `slclj-lisp-implementations' is nil, use
  `inferior-lisp-program' as fallback.

- If the table `slclj-lisp-implementations' is non-nil use the
  implementation with name `slclj-default-lisp' or if that's nil
  the first entry in the table.

- If the prefix-arg is `-', prompt for one of the registered
  lisps.

- If the prefix-arg is positive, read the command to start the
  process."
  (let ((table slclj-lisp-implementations))
    (cond ((not current-prefix-arg) (slclj-lisp-options))
          ((eq current-prefix-arg '-)
           (let ((key (completing-read 
                       "Lisp name: " (mapcar (lambda (x) 
                                               (list (symbol-name (car x)))) 
                                             table)
                       nil t)))
             (slclj-lookup-lisp-implementation table (intern key))))
          (t
           (destructuring-bind (program &rest program-args)
               (split-string (read-string 
                              "Run lisp: " inferior-lisp-program
                              'slclj-inferior-lisp-program-history))
             (let ((coding-system 
                    (if (eq 16 (prefix-numeric-value current-prefix-arg))
                        (read-coding-system "set slclj-coding-system: "
                                            slclj-net-coding-system)
                      slclj-net-coding-system)))
               (list :program program :program-args program-args
                     :coding-system coding-system)))))))

(defun slclj-lisp-options (&optional name)
  (let ((table slclj-lisp-implementations))
    (assert (or (not name) table))
    (cond (table (slclj-lookup-lisp-implementation slclj-lisp-implementations 
                                                   (or name slclj-default-lisp
                                                       (car (car table)))))
          (t (destructuring-bind (program &rest args)
                 (split-string inferior-lisp-program)
               (list :program program :program-args args))))))

(defun slclj-lookup-lisp-implementation (table name)
  (destructuring-bind (name (prog &rest args) &rest keys) (assoc name table)
    (list* :name name :program prog :program-args args keys)))

(defun* slclj-start (&key (program inferior-lisp-program) program-args 
                          directory
                          (coding-system slclj-net-coding-system)
                          (init 'slclj-init-command)
                          name
                          (buffer "*inferior-lisp*")
                          init-function
                          env)
  "Start a Lisp process and connect to it.
This function is intended for programmatic use if `slclj' is not
flexible enough.

PROGRAM and PROGRAM-ARGS are the filename and argument strings
  for the subprocess.
INIT is a function that should return a string to load and start
  Swank. The function will be called with the PORT-FILENAME and ENCODING as
  arguments.  INIT defaults to `slclj-init-command'. 
CODING-SYSTEM a symbol for the coding system. The default is 
  slclj-net-coding-system
ENV environment variables for the subprocess (see `process-environment').
INIT-FUNCTION function to call right after the connection is established.
BUFFER the name of the buffer to use for the subprocess.
NAME a symbol to describe the Lisp implementation
DIRECTORY change to this directory before starting the process.
"
  (let ((args (list :program program :program-args program-args :buffer buffer 
                    :coding-system coding-system :init init :name name
                    :init-function init-function :env env)))
    (slclj-check-coding-system coding-system)
    (when (slclj-bytecode-stale-p)
      (slclj-urge-bytecode-recompile))
    (let ((proc (slclj-maybe-start-lisp program program-args env
                                        directory buffer)))
      (slclj-inferior-connect proc args)
      (pop-to-buffer (process-buffer proc)))))

(defun slclj-start* (options)
  (apply #'slclj-start options))

(defun slclj-connect (host port &optional coding-system)
  "Connect to a running Swank server. Return the connection."
  (interactive (list (read-from-minibuffer "Host: " slclj-lisp-host)
                     (read-from-minibuffer "Port: " (format "%d" slclj-port)
                                           nil t)))
  (when (and (interactive-p) slclj-net-processes
             (y-or-n-p "Close old connections first? "))
    (slclj-disconnect-all))
  (message "Connecting to Swank on port %S.." port)
  (let ((coding-system (or coding-system slclj-net-coding-system)))
    (slclj-check-coding-system coding-system)
    (message "Connecting to Swank on port %S.." port)
    (let* ((process (slclj-net-connect host port coding-system))
           (slclj-dispatching-connection process))
      (slclj-setup-connection process))))

;; FIXME: seems redundant
(defun slclj-start-and-init (options fun)
  (let* ((rest (plist-get options :init-function))
         (init (cond (rest `(lambda () (funcall ',rest) (funcall ',fun)))
                     (t fun))))
    (slclj-start* (plist-put (copy-list options) :init-function init))))

;;;;; Start inferior lisp
;;;
;;; Here is the protocol for starting SLCLJ:
;;;
;;;   0. Emacs recompiles/reloads slclj.elc if it exists and is stale.
;;;   1. Emacs starts an inferior Lisp process.
;;;   2. Emacs tells Lisp (via stdio) to load and start Swank.
;;;   3. Lisp recompiles the Swank if needed.
;;;   4. Lisp starts the Swank server and writes its TCP port to a temp file.
;;;   5. Emacs reads the temp file to get the port and then connects.
;;;   6. Emacs prints a message of warm encouragement for the hacking ahead.
;;;
;;; Between steps 2-5 Emacs polls for the creation of the temp file so
;;; that it can make the connection. This polling may continue for a
;;; fair while if Swank needs recompilation.

(defvar slclj-connect-retry-timer nil
  "Timer object while waiting for an inferior-lisp to start.")

;;; Recompiling bytecode:

(defun slclj-bytecode-stale-p ()
  "Return true if slclj.elc is older than slclj.el."
  (when-let (libfile (locate-library "slclj"))
    (let* ((basename (file-name-sans-extension libfile))
           (sourcefile (concat basename ".el"))
           (bytefile (concat basename ".elc")))
      (and (file-exists-p bytefile)
           (file-newer-than-file-p sourcefile bytefile)))))

(defun slclj-recompile-bytecode ()
  "Recompile and reload slclj.
Warning: don't use this in XEmacs, it seems to crash it!"
  (interactive)
  (let ((sourcefile (concat (file-name-sans-extension (locate-library "slclj"))
                            ".el")))
    (byte-compile-file sourcefile t)))

(defun slclj-urge-bytecode-recompile ()
  "Urge the user to recompile slclj.elc.
Return true if we have been given permission to continue."
  (cond ((featurep 'xemacs)
         ;; My XEmacs crashes and burns if I recompile/reload an elisp
         ;; file from itself. So they have to do it themself.
         (or (y-or-n-p "slclj.elc is older than source.  Continue? ")
             (signal 'quit nil)))
        ((y-or-n-p "slclj.elc is older than source.  Recompile first? ")
         (slclj-recompile-bytecode))
        (t)))

(defun slclj-abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (cond (slclj-connect-retry-timer
         (slclj-cancel-connect-retry-timer)
         (message "Cancelled connection attempt."))
        (t (error "Not connecting"))))

;;; Starting the inferior Lisp and loading Swank:

(defun slclj-maybe-start-lisp (program program-args env directory buffer)
  "Return a new or existing inferior lisp process."
  (cond ((not (comint-check-proc buffer))
         (slclj-start-lisp program program-args env directory buffer))
        ((slclj-reinitialize-inferior-lisp-p program program-args env buffer)
         (when-let (conn (find (get-buffer-process buffer) slclj-net-processes 
                               :key #'slclj-inferior-process))
           (slclj-net-close conn))
         (get-buffer-process buffer))
        (t (slclj-start-lisp program program-args env directory
                             (generate-new-buffer-name buffer)))))

(defun slclj-reinitialize-inferior-lisp-p (program program-args env buffer)
  (let ((args (slclj-inferior-lisp-args (get-buffer-process buffer))))
    (and (equal (plist-get args :program) program)
         (equal (plist-get args :program-args) program-args)
         (equal (plist-get args :env) env)
         (not (y-or-n-p "Create an additional *inferior-lisp*? ")))))

(defvar slclj-inferior-process-start-hook nil
  "Hook called whenever a new process gets started.")

(defun slclj-start-lisp (program program-args env directory buffer)
  "Does the same as `inferior-lisp' but less ugly.
Return the created process."
  (with-current-buffer (get-buffer-create buffer)
    (when directory
      (cd (expand-file-name directory)))
    (comint-mode)
    (let ((process-environment (append env process-environment))
          (process-connection-type nil))
      (comint-exec (current-buffer) "inferior-lisp" program nil program-args))
    (lisp-mode-variables t)
    (let ((proc (get-buffer-process (current-buffer))))
      (slclj-set-query-on-exit-flag proc)
      (run-hooks 'slclj-inferior-process-start-hook)
      proc)))

(defun slclj-inferior-connect (process args)
  "Start a Swank server in the inferior Lisp and connect."
  (slclj-delete-swank-port-file 'quiet)
  (slclj-start-swank-server process args)
  (slclj-read-port-and-connect process nil))

(defvar slclj-inferior-lisp-args nil
  "A buffer local variable in the inferior proccess.
See `slclj-start'.")

(defun slclj-start-swank-server (process args)
  "Start a Swank server on the inferior lisp."
  (destructuring-bind (&key coding-system init &allow-other-keys) args
    (with-current-buffer (process-buffer process)
      (make-local-variable 'slclj-inferior-lisp-args)
      (setq slclj-inferior-lisp-args args)
      (let ((str (funcall init (slclj-swank-port-file) coding-system)))
        (goto-char (process-mark process)) 
        (insert-before-markers str)
        (process-send-string process str)))))

(defun slclj-inferior-lisp-args (process)
  "Return the initial process arguments.
See `slclj-start'."
  (with-current-buffer (process-buffer process)
    slclj-inferior-lisp-args))

;; XXX load-server & start-server used to be separated. maybe that was  better.
(defun slclj-init-command (port-filename coding-system)
  "Return a string to initialize Lisp."
  (let ((loader (if (file-name-absolute-p slclj-backend)
                    slclj-backend
                  (concat slclj-path slclj-backend)))
        (encoding (slclj-coding-system-cl-name coding-system)))
    ;; Return a single form to avoid problems with buffered input.
    (format "%S\n\n"
            `(progn
               (load ,(expand-file-name loader) :verbose t)
               (funcall (read-from-string "swank-loader:init"))
               (funcall (read-from-string "swank:start-server")
                        ,port-filename
                        :coding-system ,encoding)))))

(defun slclj-swank-port-file ()
  "Filename where the SWANK server writes its TCP port number."
  (concat (file-name-as-directory (slclj-temp-directory))
          (format "slclj.%S" (emacs-pid))))

(defun slclj-temp-directory ()
  (cond ((fboundp 'temp-directory) (temp-directory))
        ((boundp 'temporary-file-directory) temporary-file-directory)
        (t "/tmp/")))

(defun slclj-delete-swank-port-file (&optional quiet)
  (condition-case data
      (delete-file (slclj-swank-port-file))
    (error
     (ecase quiet
       ((nil) (signal (car data) (cdr data)))
       (quiet)
       (message (message "Unable to delete swank port file %S"
                         (slclj-swank-port-file)))))))

(defun slclj-read-port-and-connect (inferior-process retries)
  (slclj-cancel-connect-retry-timer)
  (slclj-attempt-connection inferior-process retries 1))

(defun slclj-attempt-connection (process retries attempt)
  ;; A small one-state machine to attempt a connection with
  ;; timer-based retries.
  (let ((file (slclj-swank-port-file))) 
    (unless (active-minibuffer-window)
      (message "Polling %S.. (Abort with `M-x slclj-abort-connection'.)" file))
    (cond ((and (file-exists-p file)
                (> (nth 7 (file-attributes file)) 0)) ; file size
           (slclj-cancel-connect-retry-timer)
           (let ((port (slclj-read-swank-port))
                 (args (slclj-inferior-lisp-args process)))
             (slclj-delete-swank-port-file 'message)
             (let ((c (slclj-connect slclj-lisp-host port
                                     (plist-get args :coding-system))))
               (slclj-set-inferior-process c process))))
          ((and retries (zerop retries))
           (slclj-cancel-connect-retry-timer)
           (message "Gave up connecting to Swank after %d attempts." attempt))
          ((eq (process-status process) 'exit)
           (slclj-cancel-connect-retry-timer)
           (message "Failed to connect to Swank: inferior process exited."))
          (t
           (when (and (file-exists-p file) 
                      (zerop (nth 7 (file-attributes file))))
             (message "(Zero length port file)")
             ;; the file may be in the filesystem but not yet written
             (unless retries (setq retries 3)))
           (unless slclj-connect-retry-timer
             (setq slclj-connect-retry-timer
                   (run-with-timer
                    0.3 0.3
                    #'slclj-timer-call #'slclj-attempt-connection 
                    process (and retries (1- retries)) 
                    (1+ attempt))))))))
    
(defun slclj-timer-call (fun &rest args)
  "Call function FUN with ARGS, reporting all errors.

The default condition handler for timer functions (see
`timer-event-handler') ignores errors."
  (condition-case data
      (apply fun args)
    (error (debug nil (list "Error in timer" fun args data)))))

(defun slclj-cancel-connect-retry-timer ()
  (when slclj-connect-retry-timer
    (cancel-timer slclj-connect-retry-timer)
    (setq slclj-connect-retry-timer nil)))

(defun slclj-read-swank-port ()
  "Read the Swank server port number from the `slclj-swank-port-file'."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (slclj-swank-port-file))
      (goto-char (point-min))
      (let ((port (read (current-buffer))))
        (assert (integerp port))
        port))))

(defun slclj-toggle-debug-on-swank-error ()
  (interactive)
  (if (slclj-eval `(swank:toggle-debug-on-swank-error))
      (message "Debug on SWANK error enabled.")
      (message "Debug on SWANK error disabled.")))

;;; Words of encouragement

(defun slclj-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar slclj-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the source be with you!"
    "Take this REPL, brother, and may it serve you well."
    "Lemonodor-fame is but a hack away!"
    ,(format "%s, this could be the start of a beautiful program."
             (slclj-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun slclj-random-words-of-encouragement ()
  "Return a string of hackerish encouragement."
  (eval (nth (random (length slclj-words-of-encouragement))
             slclj-words-of-encouragement)))


;;;; Networking
;;;
;;; This section covers the low-level networking: establishing
;;; connections and encoding/decoding protocol messages.
;;;
;;; Each SLCLJ protocol message beings with a 3-byte length header
;;; followed by an S-expression as text. The sexp must be readable
;;; both by Emacs and by Common Lisp, so if it contains any embedded
;;; code fragments they should be sent as strings.
;;;
;;; The set of meaningful protocol messages are not specified
;;; here. They are defined elsewhere by the event-dispatching
;;; functions in this file and in swank.lisp.

(defvar slclj-net-processes nil
  "List of processes (sockets) connected to Lisps.")

(defvar slclj-net-process-close-hooks '()
  "List of functions called when a slclj network connection closes.
The functions are called with the process as their argument.")

(defun slclj-secret ()
  "Find the magic secret from the user's home directory.
Return nil if the file doesn't exist or is empty; otherwise the
first line of the file."
  (condition-case err
      (with-temp-buffer
	(insert-file-contents "~/.slclj-secret")
	(goto-char (point-min))
	(buffer-substring (point-min) (line-end-position)))
    (file-error nil)))

;;; Interface
(defun slclj-net-connect (host port coding-system)
  "Establish a connection with a CL."
  (let* ((inhibit-quit nil)
         (proc (open-network-stream "SLCLJ Lisp" nil host port))
         (buffer (slclj-make-net-buffer " *cl-connection*")))
    (push proc slclj-net-processes)
    (set-process-buffer proc buffer)
    (set-process-filter proc 'slclj-net-filter)
    (set-process-sentinel proc 'slclj-net-sentinel)
    (slclj-set-query-on-exit-flag proc)
    (when (fboundp 'set-process-coding-system)
      (slclj-check-coding-system coding-system)
      (set-process-coding-system proc coding-system coding-system))
    (when-let (secret (slclj-secret))
      (slclj-net-send secret proc))
    proc))

(defun slclj-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (set (make-local-variable 'kill-buffer-query-functions) nil))
    buffer))

(defun slclj-set-query-on-exit-flag (process)
  "Set PROCESS's query-on-exit-flag to `slclj-kill-without-query-p'."
  (when slclj-kill-without-query-p
    ;; avoid byte-compiler warnings
    (let ((fun (if (fboundp 'set-process-query-on-exit-flag)
                   'set-process-query-on-exit-flag
                 'process-kill-without-query)))
      (funcall fun process nil))))

;;;;; Coding system madness

(defun slclj-check-coding-system (coding-system)
  "Signal an error if CODING-SYSTEM isn't a valid coding system."
  (interactive)
  (let ((props (slclj-find-coding-system coding-system)))
    (unless props
      (error "Invalid slclj-net-coding-system: %s. %s"
             coding-system (mapcar #'car slclj-net-valid-coding-systems)))
    (when (and (second props) (boundp 'default-enable-multibyte-characters))
      (assert default-enable-multibyte-characters))
    t))

(defun slclj-coding-system-mulibyte-p (coding-system)
  (second (slclj-find-coding-system coding-system)))

(defun slclj-coding-system-cl-name (coding-system)
  (third (slclj-find-coding-system coding-system)))

;;; Interface
(defun slclj-net-send (sexp proc)
  "Send a SEXP to Lisp over the socket PROC.
This is the lowest level of communication. The sexp will be READ and
EVAL'd by Lisp."
  (let* ((msg (concat (slclj-prin1-to-string sexp) "\n"))
         (string (concat (slclj-net-encode-length (length msg)) msg))
         (coding-system (cdr (process-coding-system proc))))
    (slclj-log-event sexp)
    (cond ((slclj-safe-encoding-p coding-system string)
           (process-send-string proc string))
          (t (error "Coding system %s not suitable for %S"
                    coding-system string)))))

(defun slclj-safe-encoding-p (coding-system string)
  "Return true iff CODING-SYSTEM can safely encode STRING."
  (if (featurep 'xemacs)
      ;; FIXME: XEmacs encodes non-encodeable chars as ?~ automatically
      t
    (or (let ((candidates (find-coding-systems-string string))
              (base (coding-system-base coding-system)))
          (or (equal candidates '(undecided))
              (memq base candidates)))
        (and (not (multibyte-string-p string))
             (not (slclj-coding-system-mulibyte-p coding-system))))))

(defun slclj-net-close (process &optional debug)
  (setq slclj-net-processes (remove process slclj-net-processes))
  (when (eq process slclj-default-connection)
    (setq slclj-default-connection nil))
  (cond (debug         
         (set-process-sentinel process 'ignore)
         (set-process-filter process 'ignore)
         (delete-process process))
        (t
         (run-hook-with-args 'slclj-net-process-close-hooks process)
         ;; killing the buffer also closes the socket
         (kill-buffer (process-buffer process)))))

(defun slclj-net-sentinel (process message)
  (message "Lisp connection closed unexpectedly: %s" message)
  (slclj-net-close process))

;;; Socket input is handled by `slclj-net-filter', which decodes any
;;; complete messages and hands them off to the event dispatcher.

(defun slclj-net-filter (process string)
  "Accept output from the socket and process all complete messages."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (slclj-process-available-input process))

(defun slclj-process-available-input (process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (slclj-net-have-input-p)
      (let ((event (slclj-net-read-or-lose process))
            (ok nil))
        (slclj-log-event event)
        (unwind-protect
            (save-current-buffer
              (slclj-dispatch-event event process)
              (setq ok t))
          (unless ok
            (slclj-run-when-idle 'slclj-process-available-input process)))))))

(defun slclj-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (slclj-net-decode-length))))

(defun slclj-run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time 
         (if (featurep 'xemacs) itimer-short-interval 0) 
         nil function args))

(defun slclj-net-read-or-lose (process)
  (condition-case error
      (slclj-net-read)
    (error
     (debug 'error error)
     (slclj-net-close process t)
     (error "net-read error: %S" error))))

(defun slclj-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (slclj-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)))
    (assert (plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (current-buffer)))
      (delete-region (point-min) end))))

(defun slclj-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun slclj-net-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun slclj-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length 
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))


;;;; Connections
;;;
;;; "Connections" are the high-level Emacs<->Lisp networking concept.
;;;
;;; Emacs has a connection to each Lisp process that it's interacting
;;; with. Typically there would only be one, but a user can choose to
;;; connect to many Lisps simultaneously.
;;;
;;; A connection consists of a control socket, optionally an extra
;;; socket dedicated to receiving Lisp output (an optimization), and a
;;; set of connection-local state variables.
;;;
;;; The state variables are stored as buffer-local variables in the
;;; control socket's process-buffer and are used via accessor
;;; functions. These variables include things like the *FEATURES* list
;;; and Unix Pid of the Lisp process.
;;;
;;; One connection is "current" at any given time. This is:
;;;   `slclj-dispatching-connection' if dynamically bound, or
;;;   `slclj-buffer-connection' if this is set buffer-local, or
;;;   `slclj-default-connection' otherwise. 
;;;
;;; When you're invoking commands in your source files you'll be using
;;; `slclj-default-connection'. This connection can be interactively
;;; reassigned via the connection-list buffer.
;;;
;;; When a command creates a new buffer it will set
;;; `slclj-buffer-connection' so that commands in the new buffer will
;;; use the connection that the buffer originated from. For example,
;;; the apropos command creates the *Apropos* buffer and any command
;;; in that buffer (e.g. `M-.') will go to the same Lisp that did the
;;; apropos search. REPL buffers are similarly tied to their
;;; respective connections.
;;;
;;; When Emacs is dispatching some network message that arrived from a
;;; connection it will dynamically bind `slclj-dispatching-connection'
;;; so that the event will be processed in the context of that
;;; connection.
;;;
;;; This is mostly transparent. The user should be aware that he can
;;; set the default connection to pick which Lisp handles commands in
;;; Lisp-mode source buffers, and slclj hackers should be aware that
;;; they can tie a buffer to a specific connection. The rest takes
;;; care of itself.

(defvar slclj-dispatching-connection nil
  "Network process currently executing.
This is dynamically bound while handling messages from Lisp; it
overrides `slclj-buffer-connection' and `slclj-default-connection'.")

(make-variable-buffer-local
 (defvar slclj-buffer-connection nil
   "Network connection to use in the current buffer.
This overrides `slclj-default-connection'."))

(defvar slclj-default-connection nil
  "Network connection to use by default.
Used for all Lisp communication, except when overridden by
`slclj-dispatching-connection' or `slclj-buffer-connection'.")

(defun slclj-current-connection ()
  "Return the connection to use for Lisp interaction.
Return nil if there's no connection."
  (or slclj-dispatching-connection
      slclj-buffer-connection
      slclj-default-connection))

(defun slclj-connection ()
  "Return the connection to use for Lisp interaction.
Signal an error if there's no connection."
  (let ((conn (slclj-current-connection)))
    (cond ((and (not conn) slclj-net-processes)
           (or (slclj-auto-select-connection)
               (error "No default connection selected.")))
          ((not conn)
           (or (slclj-auto-connect)
               (error "Not connected.")))
          ((not (eq (process-status conn) 'open))
           (error "Connection closed."))
          (t conn))))

;; FIXME: should be called auto-start
(defcustom slclj-auto-connect 'never
  "Controls auto connection when information from lisp process is needed.
This doesn't mean it will connect right after Slclj is loaded."
  :group 'slclj-mode
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defun slclj-auto-connect ()
  (cond ((or (eq slclj-auto-connect 'always)
             (and (eq slclj-auto-connect 'ask)
                  (y-or-n-p "No connection.  Start Slclj? ")))
         (save-window-excursion
           (slclj)
           (while (not (slclj-current-connection))
             (sleep-for 1))
           (slclj-connection)))
        (t nil)))

(defcustom slclj-auto-select-connection 'ask
  "Controls auto selection after the default connection was closed."
  :group 'slclj-mode
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defun slclj-auto-select-connection ()
  (let* ((c0 (car slclj-net-processes))
         (c (cond ((eq slclj-auto-select-connection 'always) c0)
                  ((and (eq slclj-auto-select-connection 'ask)
                        (y-or-n-p 
                         (format "No default connection selected.  %s %s? "
                                 "Switch to" (slclj-connection-name c0))))
                   c0))))
    (when c
      (slclj-select-connection c)
      (message "Switching to connection: %s" (slclj-connection-name c))
      c)))

(defun slclj-select-connection (process)
  "Make PROCESS the default connection."
  (setq slclj-default-connection process))

(defun slclj-cycle-connections ()
  "Change current slclj connection, cycling through all connections."
  (interactive)
  (let* ((tail (or (cdr (member (slclj-current-connection)
                                slclj-net-processes))
                   slclj-net-processes))
         (p (car tail)))
    (slclj-select-connection p)
    (message "Lisp: %s %s" (slclj-connection-name p) (process-contact p))))

(defmacro* slclj-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `slclj-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  `(with-current-buffer
       (process-buffer (or ,process (slclj-connection)
                           (error "No connection")))
     ,@body))

(put 'slclj-with-connection-buffer 'lisp-indent-function 1)

;;; Connection-local variables:

(defmacro slclj-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `slclj-connection'."
  (let ((real-var (intern (format "%s:connlocal" varname))))
    `(progn
       ;; Variable
       (make-variable-buffer-local
        (defvar ,real-var ,@initial-value-and-doc))
       ;; Accessor
       (defun ,varname (&optional process)
         (slclj-with-connection-buffer (process) ,real-var))
       ;; Setf
       (defsetf ,varname (&optional process) (store)
         `(slclj-with-connection-buffer (,process)
            (setq (\, (quote (\, real-var))) (\, store))
            (\, store)))
       '(\, varname))))

(put 'slclj-def-connection-var 'lisp-indent-function 2)
(put 'slclj-indulge-pretty-colors 'slclj-def-connection-var t)

(slclj-def-connection-var slclj-connection-number nil
  "Serial number of a connection.
Bound in the connection's process-buffer.")

(slclj-def-connection-var slclj-lisp-features '()
  "The symbol-names of Lisp's *FEATURES*.
This is automatically synchronized from Lisp.")

(slclj-def-connection-var slclj-lisp-modules '()
  "The strings of Lisp's *MODULES*.")

(slclj-def-connection-var slclj-pid nil
  "The process id of the Lisp process.")

(slclj-def-connection-var slclj-lisp-implementation-type nil
  "The implementation type of the Lisp process.")

(slclj-def-connection-var slclj-lisp-implementation-version nil
  "The implementation type of the Lisp process.")

(slclj-def-connection-var slclj-lisp-implementation-name nil
  "The short name for the Lisp implementation.")

(slclj-def-connection-var slclj-lisp-implementation-program nil
  "The argv[0] of the process running the Lisp implementation.")

(slclj-def-connection-var slclj-connection-name nil
  "The short name for connection.")

(slclj-def-connection-var slclj-inferior-process nil
  "The inferior process for the connection if any.")

(slclj-def-connection-var slclj-communication-style nil
  "The communication style.")

(slclj-def-connection-var slclj-machine-instance nil
  "The name of the (remote) machine running the Lisp process.")

;;;;; Connection setup

(defvar slclj-connection-counter 0
  "The number of SLCLJ connections made. For generating serial numbers.")

;;; Interface
(defun slclj-setup-connection (process)
  "Make a connection out of PROCESS."
  (let ((slclj-dispatching-connection process))
    (slclj-init-connection-state process)
    (slclj-select-connection process)
    process))

(defun slclj-init-connection-state (proc)
  "Initialize connection state in the process-buffer of PROC."
  ;; To make life simpler for the user: if this is the only open
  ;; connection then reset the connection counter.
  (when (equal slclj-net-processes (list proc))
    (setq slclj-connection-counter 0))
  (slclj-with-connection-buffer ()
    (setq slclj-buffer-connection proc))
  (setf (slclj-connection-number proc) (incf slclj-connection-counter))
  ;; We do the rest of our initialization asynchronously. The current
  ;; function may be called from a timer, and if we setup the REPL
  ;; from a timer then it mysteriously uses the wrong keymap for the
  ;; first command.
  (let ((slclj-current-thread t))
    (slclj-eval-async '(swank:connection-info)
                      (slclj-curry #'slclj-set-connection-info proc))))

(defun slclj-set-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (let ((slclj-dispatching-connection connection)
        (slclj-current-thread t))
    (destructuring-bind (&key pid style lisp-implementation machine
                              features package version modules
                              &allow-other-keys) info
      (slclj-check-version version connection)
      (setf (slclj-pid) pid
            (slclj-communication-style) style
            (slclj-lisp-features) features
            (slclj-lisp-modules) modules)
      (destructuring-bind (&key type name version program) lisp-implementation
        (setf (slclj-lisp-implementation-type) type
              (slclj-lisp-implementation-version) version
              (slclj-lisp-implementation-name) name
              (slclj-lisp-implementation-program) program
              (slclj-connection-name) (slclj-generate-connection-name name)))
      (destructuring-bind (&key instance type version) machine
        (setf (slclj-machine-instance) instance)))
    (let ((args (when-let (p (slclj-inferior-process))
                  (slclj-inferior-lisp-args p))))
      (when-let (name (plist-get args ':name))
        (unless (string= (slclj-lisp-implementation-name) name)
          (setf (slclj-connection-name)
                (slclj-generate-connection-name (symbol-name name)))))
      (slclj-load-contribs)
      (run-hooks 'slclj-connected-hook)
      (when-let (fun (plist-get args ':init-function))
        (funcall fun)))
    (message "Connected. %s" (slclj-random-words-of-encouragement))))

(defun slclj-check-version (version conn)
  (or (equal version slclj-protocol-version)
      (equal slclj-protocol-version 'ignore)
      (y-or-n-p 
       (format "Versions differ: %s (slclj) vs. %s (swank). Continue? "
               slclj-protocol-version version))
      (slclj-net-close conn)
      (top-level)))

(defun slclj-generate-connection-name (lisp-name)
  (loop for i from 1
        for name = lisp-name then (format "%s<%d>" lisp-name i)
        while (find name slclj-net-processes 
                    :key #'slclj-connection-name :test #'equal)
        finally (return name)))

(defun slclj-connection-close-hook (process)
  (when (eq process slclj-default-connection)
    (when slclj-net-processes
      (slclj-select-connection (car slclj-net-processes))
      (message "Default connection closed; switched to #%S (%S)"
               (slclj-connection-number)
               (slclj-connection-name)))))

(add-hook 'slclj-net-process-close-hooks 'slclj-connection-close-hook)

;;;;; Commands on connections

(defun slclj-disconnect ()
  "Close the current connection."
  (interactive)
  (slclj-net-close (slclj-connection)))

(defun slclj-disconnect-all ()
  "Disconnect all connections."
  (interactive)
  (mapc #'slclj-net-close slclj-net-processes))

(defun slclj-connection-port (connection)
  "Return the remote port number of CONNECTION."
  (if (featurep 'xemacs)
      (car (process-id connection))
    (cadr (process-contact connection))))

(defun slclj-process (&optional connection)
  "Return the Lisp process for CONNECTION (default `slclj-connection').
Return nil if there's no process object for the connection."
  (let ((proc (slclj-inferior-process connection)))
    (if (and proc 
             (memq (process-status proc) '(run stop)))
        proc)))

;; Non-macro version to keep the file byte-compilable. 
(defun slclj-set-inferior-process (connection process)
  (setf (slclj-inferior-process connection) process))

(defun slclj-use-sigint-for-interrupt (&optional connection)
  (let ((c (or connection (slclj-connection))))
    (ecase (slclj-communication-style c)
      ((:fd-handler nil) t)
      ((:spawn :sigio) nil))))

(defvar slclj-inhibit-pipelining t
  "*If true, don't send background requests if Lisp is already busy.")

(defun slclj-background-activities-enabled-p ()
  (and (let ((con (slclj-current-connection)))
         (and con
              (eq (process-status con) 'open)))
       (or (not (slclj-busy-p))
           (not slclj-inhibit-pipelining))))


;;;; Communication protocol

;;;;; Emacs Lisp programming interface
;;;
;;; The programming interface for writing Emacs commands is based on
;;; remote procedure calls (RPCs). The basic operation is to ask Lisp
;;; to apply a named Lisp function to some arguments, then to do
;;; something with the result.
;;;
;;; Requests can be either synchronous (blocking) or asynchronous
;;; (with the result passed to a callback/continuation function).  If
;;; an error occurs during the request then the debugger is entered
;;; before the result arrives -- for synchronous evaluations this
;;; requires a recursive edit.
;;;
;;; You should use asynchronous evaluations (`slclj-eval-async') for
;;; most things. Reserve synchronous evaluations (`slclj-eval') for
;;; the cases where blocking Emacs is really appropriate (like
;;; completion) and that shouldn't trigger errors (e.g. not evaluate
;;; user-entered code).
;;;
;;; We have the concept of the "current Lisp package". RPC requests
;;; always say what package the user is making them from and the Lisp
;;; side binds that package to *BUFFER-PACKAGE* to use as it sees
;;; fit. The current package is defined as the buffer-local value of
;;; `slclj-buffer-package' if set, and otherwise the package named by
;;; the nearest IN-PACKAGE as found by text search (first backwards,
;;; then forwards).
;;;
;;; Similarly we have the concept of the current thread, i.e. which
;;; thread in the Lisp process should handle the request. The current
;;; thread is determined solely by the buffer-local value of
;;; `slclj-current-thread'. This is usually bound to t meaning "no
;;; particular thread", but can also be used to nominate a specific
;;; thread. The REPL and the debugger both use this feature to deal
;;; with specific threads.

(make-variable-buffer-local
 (defvar slclj-current-thread t
   "The id of the current thread on the Lisp side.  
t means the \"current\" thread;
:repl-thread the thread that executes REPL requests;
fixnum a specific thread."))

(make-variable-buffer-local
 (defvar slclj-buffer-package nil
   "The Lisp package associated with the current buffer.
This is set only in buffers bound to specific packages."))

;;; `slclj-rex' is the RPC primitive which is used to implement both
;;; `slclj-eval' and `slclj-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(defmacro* slclj-rex ((&rest saved-vars)
                      (sexp &optional 
                            (package '(slclj-current-package))
                            (thread 'slclj-current-thread))
                      &rest continuations)
  "(slclj-rex (VAR ...) (SEXP &optional PACKAGE THREAD) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

PACKAGE is evaluated and Lisp binds *BUFFER-PACKAGE* to this package.
The default value is (slclj-current-package).

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (let ((result (gensym)))
    `(lexical-let ,(loop for var in saved-vars
                         collect (etypecase var
                                   (symbol (list var var))
                                   (cons var)))
       (slclj-dispatch-event 
        (list :emacs-rex ,sexp ,package ,thread
              (lambda (,result)
                (destructure-case ,result
                  ,@continuations)))))))

(put 'slclj-rex 'lisp-indent-function 2)

;;; Interface
(defun slclj-current-package ()
  "Return the Common Lisp package in the current context.
If `slclj-buffer-package' has a value then return that, otherwise
search for and read an `in-package' form."
  (or slclj-buffer-package
      (save-restriction
        (widen)
        (slclj-find-buffer-package))))

(defvar slclj-find-buffer-package-function 'slclj-search-buffer-package
  "*Function to use for `slclj-find-buffer-package'.  
The result should be the package-name (a string)
or nil if nothing suitable can be found.")

(defun slclj-find-buffer-package ()
  "Figure out which Lisp package the current buffer is associated with."
  (funcall slclj-find-buffer-package-function))

;; When modifing this code consider cases like:
;;  (in-package #.*foo*)
;;  (in-package #:cl)
;;  (in-package :cl)
;;  (in-package "CL")
;;  (in-package |CL|)
;;  (in-package #+ansi-cl :cl #-ansi-cl 'lisp)
(defun slclj-search-buffer-package ()
  (let ((case-fold-search t)
        (regexp (concat "^(\\(cl:\\|common-lisp:\\)?in-package\\>[ \t']*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 2)))))

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar slclj-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun slclj-eval (sexp &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
  (when (null package) (setq package (slclj-current-package)))
  (let* ((tag (gensym (format "slclj-result-%d-" 
                              (1+ (slclj-continuation-counter)))))
	 (slclj-stack-eval-tags (cons tag slclj-stack-eval-tags)))
    (apply
     #'funcall 
     (catch tag
       (slclj-rex (tag sexp)
           (sexp package)
         ((:ok value)
          (unless (member tag slclj-stack-eval-tags)
            (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
                   tag sexp))
          (throw tag (list #'identity value)))
         ((:abort)
          (throw tag (list #'error "Synchronous Lisp Evaluation aborted"))))
       (let ((debug-on-quit t)
             (inhibit-quit nil)
             (conn (slclj-connection)))
         (while t 
           (unless (eq (process-status conn) 'open)
             (error "Lisp connection closed unexpectedly"))
           (slclj-accept-process-output nil 0.01)))))))

(defun slclj-eval-async (sexp &optional cont package)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (slclj-rex (cont (buffer (current-buffer)))
      (sexp (or package (slclj-current-package)))
    ((:ok result)
     (when cont
       (set-buffer buffer)
       (funcall cont result)))
    ((:abort)
     (message "Evaluation aborted.")))
  ;; Guard against arbitrary return values which once upon a time
  ;; showed up in the minibuffer spuriously (due to a bug in
  ;; slclj-autodoc.)  If this ever happens again, returning the
  ;; following will make debugging much easier:
  :slclj-eval-async)

(put 'slclj-eval-async 'lisp-indent-function 1)

;;; These functions can be handy too:

(defun slclj-connected-p ()
  "Return true if the Swank connection is open."
  (not (null slclj-net-processes)))

(defun slclj-check-connected ()
  "Signal an error if we are not connected to Lisp."
  (unless (slclj-connected-p)
    (error "Not connected. Use `%s' to start a Lisp."
           (substitute-command-keys "\\[slclj]"))))

;; UNUSED
(defun slclj-debugged-connection-p (conn)
  ;; This previously was (AND (SLDB-DEBUGGED-CONTINUATIONS CONN) T),
  ;; but an SLDB buffer may exist without having continuations
  ;; attached to it, e.g. the one resulting from `slclj-interrupt'.
  (loop for b in (sldb-buffers)
        thereis (with-current-buffer b
                  (eq slclj-buffer-connection conn))))

(defun slclj-busy-p (&optional conn)
  "True if Lisp has outstanding requests.
Debugged requests are ignored."
  (let ((debugged (sldb-debugged-continuations (or conn (slclj-connection)))))
    (remove-if (lambda (id) 
                 (memq id debugged))
               (slclj-rex-continuations)
               :key #'car)))

(defun slclj-sync ()
  "Block until the most recent request has finished."
  (when (slclj-rex-continuations)
    (let ((tag (caar (slclj-rex-continuations))))
      (while (find tag (slclj-rex-continuations) :key #'car)
        (slclj-accept-process-output nil 0.1)))))

(defun slclj-ping ()
  "Check that communication works."
  (interactive)
  (message "%s" (slclj-eval "PONG")))
 
;;;;; Protocol event handler (the guts)
;;;
;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from Lisp.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from Lisp don't.

(slclj-def-connection-var slclj-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(slclj-def-connection-var slclj-continuation-counter 0
  "Continuation serial number counter.")

(defvar slclj-event-hooks)

(defun slclj-dispatch-event (event &optional process)
  (let ((slclj-dispatching-connection (or process (slclj-connection))))
    (or (run-hook-with-args-until-success 'slclj-event-hooks event)
        (destructure-case event
          ((:emacs-rex form package thread continuation)
           (when (and (slclj-use-sigint-for-interrupt) (slclj-busy-p))
             (slclj-display-oneliner "; pipelined request... %S" form))
           (let ((id (incf (slclj-continuation-counter))))
             (slclj-send `(:emacs-rex ,form ,package ,thread ,id))
             (push (cons id continuation) (slclj-rex-continuations))
             (slclj-recompute-modelines)))
          ((:return value id)
           (let ((rec (assq id (slclj-rex-continuations))))
             (cond (rec (setf (slclj-rex-continuations)
                              (remove rec (slclj-rex-continuations)))
                        (slclj-recompute-modelines)
                        (funcall (cdr rec) value))
                   (t
                    (error "Unexpected reply: %S %S" id value)))))
          ((:debug-activate thread level &optional select)
           (assert thread)
           (sldb-activate thread level select))
          ((:debug thread level condition restarts frames conts)
           (assert thread)
           (sldb-setup thread level condition restarts frames conts))
          ((:debug-return thread level stepping)
           (assert thread)
           (sldb-exit thread level stepping))
          ((:emacs-interrupt thread)
           (slclj-send `(:emacs-interrupt ,thread)))
          ((:channel-send id msg)
           (slclj-channel-send (or (slclj-find-channel id)
                                   (error "Invalid channel id: %S %S" id msg))
                               msg))
          ((:emacs-channel-send id msg)
           (slclj-send `(:emacs-channel-send ,id ,msg)))
          ((:read-from-minibuffer thread tag prompt initial-value)
           (slclj-read-from-minibuffer-for-swank thread tag prompt initial-value))
          ((:y-or-n-p thread tag question)
           (slclj-y-or-n-p thread tag question))
          ((:emacs-return-string thread tag string)
           (slclj-send `(:emacs-return-string ,thread ,tag ,string)))
          ((:new-features features)
           (setf (slclj-lisp-features) features))
          ((:indentation-update info)
           (slclj-handle-indentation-update info))
          ((:eval-no-wait fun args)
           (apply (intern fun) args))
          ((:eval thread tag form-string)
           (slclj-check-eval-in-emacs-enabled)
           (slclj-eval-for-lisp thread tag form-string))
          ((:emacs-return thread tag value)
           (slclj-send `(:emacs-return ,thread ,tag ,value)))
          ((:ed what)
           (slclj-ed what))
          ((:inspect what wait-thread wait-tag)
           (let ((hook (when (and wait-thread wait-tag)
                         (lexical-let ((thread wait-thread)
                                       (tag wait-tag))
                           (lambda ()
                             (slclj-send `(:emacs-return ,thread ,tag nil)))))))
             (slclj-open-inspector what nil hook)))
          ((:background-message message)
           (slclj-background-message "%s" message))
          ((:debug-condition thread message)
           (assert thread)
           (message "%s" message))
          ((:ping thread tag)
           (slclj-send `(:emacs-pong ,thread ,tag)))
          ((:reader-error packet condition)
           (slclj-with-popup-buffer ("*Slclj Error*")
             (princ (format "Invalid protocol message:\n%s\n\n%S"
                            condition packet))
             (goto-char (point-min)))
           (error "Invalid protocol message"))
          ((:invalid-rpc id message)
           (setf (slclj-rex-continuations)
                 (remove* id (slclj-rex-continuations) :key #'car))
           (error "Invalid rpc: %s" message))))))

(defun slclj-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (slclj-net-send sexp (slclj-connection)))

(defun slclj-reset ()
  "Clear all pending continuations and erase connection buffer."
  (interactive)
  (setf (slclj-rex-continuations) '())
  (mapc #'kill-buffer (sldb-buffers))
  (slclj-with-connection-buffer ()
    (erase-buffer)))

(defun slclj-send-sigint ()
  (interactive)
  (signal-process (slclj-pid) 'SIGINT))

;;;;; Channels

;;; A channel implements a set of operations.  Those operations can be
;;; invoked by sending messages to the channel.  Channels are used for
;;; protocols which can't be expressed naturally with RPCs, e.g. for
;;; streaming data over the wire.
;;;
;;; A channel can be "remote" or "local".  Remote channels are
;;; represented by integers.  Local channels are structures.  Messages
;;; sent to a closed (remote) channel are ignored.

(slclj-def-connection-var slclj-channels '()
  "Alist of the form (ID . CHANNEL).")

(slclj-def-connection-var slclj-channels-counter 0
  "Channel serial number counter.")

(defstruct (slclj-channel (:conc-name slclj-channel.)
                          (:constructor 
                           slclj-make-channel% (operations name id plist)))
  operations name id plist)

(defun slclj-make-channel (operations &optional name)
  (let* ((id (incf (slclj-channels-counter)))
         (ch (slclj-make-channel% operations name id nil)))
    (push (cons id ch) (slclj-channels))
    ch))

(defun slclj-close-channel (channel)
  (setf (slclj-channel.operations channel) 'closed-channel)
  (let ((probe (assq (slclj-channel.id channel) (slclj-channels))))
    (cond (probe (setf (slclj-channels) (delete probe (slclj-channels))))
          (t (error "Invalid channel: %s" channel)))))

(defun slclj-find-channel (id)
  (cdr (assq id (slclj-channels))))

(defun slclj-channel-send (channel message)
  (apply (or (gethash (car message) (slclj-channel.operations channel))
             (error "Unsupported operation: %S %S" message channel))
         channel (cdr message)))

(defun slclj-channel-put (channel prop value)
  (setf (slclj-channel.plist channel) 
        (plist-put (slclj-channel.plist channel) prop value)))

(defun slclj-channel-get (channel prop)
  (plist-get (slclj-channel.plist channel) prop))

(eval-and-compile 
  (defun slclj-channel-method-table-name (type)
    (intern (format "slclj-%s-channel-methods" type))))

(defmacro slclj-define-channel-type (name)
  (let ((tab (slclj-channel-method-table-name name)))
    `(progn
       (defvar ,tab)
       (setq ,tab (make-hash-table :size 10)))))

(put 'slclj-indulge-pretty-colors 'slclj-define-channel-type t)

(defmacro slclj-define-channel-method (type method args &rest body)
  `(puthash ',method
            (lambda (self . ,args) . ,body)
            ,(slclj-channel-method-table-name type)))

(put 'slclj-define-channel-method 'lisp-indent-function 3)
(put 'slclj-indulge-pretty-colors 'slclj-define-channel-method t)


(defun slclj-send-to-remote-channel (channel-id msg)
  (slclj-dispatch-event `(:emacs-channel-send ,channel-id ,msg)))

;;;;; Event logging to *slclj-events*
;;;
;;; The *slclj-events* buffer logs all protocol messages for debugging
;;; purposes. Optionally you can enable outline-mode in that buffer,
;;; which is convenient but slows things down significantly.

(defvar slclj-log-events t
  "*Log protocol events to the *slclj-events* buffer.")

(defvar slclj-outline-mode-in-events-buffer nil
  "*Non-nil means use outline-mode in *slclj-events*.")

(defvar slclj-event-buffer-name "*slclj-events*"
  "The name of the slclj event buffer.")

(defun slclj-log-event (event)
  "Record the fact that EVENT occurred."
  (when slclj-log-events
    (with-current-buffer (slclj-events-buffer)
      ;; trim?
      (when (> (buffer-size) 100000)
        (goto-char (/ (buffer-size) 2))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
        (slclj-pprint-event event (current-buffer)))
      (when (and (boundp 'outline-minor-mode)
                 outline-minor-mode)
        (hide-entry))
      (goto-char (point-max)))))

(defun slclj-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun slclj-events-buffer ()
  "Return or create the event log buffer."
  (or (get-buffer slclj-event-buffer-name)
      (let ((buffer (get-buffer-create slclj-event-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (set (make-local-variable 'outline-regexp) "^(")
          (set (make-local-variable 'comment-start) ";")
          (set (make-local-variable 'comment-end) "")
          (when slclj-outline-mode-in-events-buffer
            (outline-minor-mode)))
        buffer)))


;;;;; Cleanup after a quit

(defun slclj-restart-inferior-lisp ()
  "Kill and restart the Lisp subprocess."
  (interactive)
  (assert (slclj-inferior-process) () "No inferior lisp process")
  (slclj-quit-lisp-internal (slclj-connection) 'slclj-restart-sentinel t))

(defun slclj-restart-sentinel (process message)
  "Restart the inferior lisp process.
Also rearrange windows."
  (assert (process-status process) 'closed)
  (let* ((proc (slclj-inferior-process process))
         (args (slclj-inferior-lisp-args proc))
         (buffer (buffer-name (process-buffer proc)))
         (buffer-window (get-buffer-window buffer))
         (new-proc (slclj-start-lisp (plist-get args :program)
                                     (plist-get args :program-args)
                                     (plist-get args :env)
                                     nil
                                     buffer)))
    (slclj-net-close process)
    (slclj-inferior-connect new-proc args)
    (pop-to-buffer buffer)
    (switch-to-buffer buffer)
    (goto-char (point-max))))

;; FIXME: move to slclj-repl
(defun slclj-kill-all-buffers ()
  "Kill all the slclj related buffers.
This is only used by the repl command sayoonara."
  (dolist (buf (buffer-list))
    (when (or (string= (buffer-name buf) slclj-event-buffer-name)
              (string-match "^\\*inferior-lisp*" (buffer-name buf))
              (string-match "^\\*slclj-repl .*\\*$" (buffer-name buf))
              (string-match "^\\*sldb .*\\*$" (buffer-name buf))
              (string-match "^\\*SLCLJ.*\\*$" (buffer-name buf)))
      (kill-buffer buf))))


;;;; Compilation and the creation of compiler-note annotations

(defvar slclj-highlight-compiler-notes t
  "*When non-nil annotate buffers with compilation notes etc.")

(defvar slclj-before-compile-functions nil
  "A list of function called before compiling a buffer or region.
The function receive two arguments: the beginning and the end of the 
region that will be compiled.")

;; FIXME: remove some of the options
(defcustom slclj-compilation-finished-hook 'slclj-maybe-show-compilation-log
  "Hook called with a list of compiler notes after a compilation."
  :group 'slclj-mode
  :type 'hook
  :options '(slclj-maybe-show-compilation-log
             slclj-create-compilation-log
             slclj-show-compilation-log
             slclj-maybe-list-compiler-notes
             slclj-list-compiler-notes
             slclj-maybe-show-xrefs-for-notes
             slclj-goto-first-note))

;; FIXME: I doubt that anybody uses this directly and it seems to be
;; only an ugly way to pass arguments.
(defvar slclj-compilation-policy nil
  "When non-nil compile with these optimization settings.")

(defun slclj-compute-policy (arg)
  "Return the policy for the prefix argument ARG."
  (flet ((between (min n max)
           (if (< n min)
               min
               (if (> n max) max n))))
    (let ((n (prefix-numeric-value arg)))
      (cond ((not arg)   slclj-compilation-policy)
            ((plusp n)   `((cl:debug . ,(between 0 n 3))))
            ((eq arg '-) `((cl:speed . 3)))
            (t           `((cl:speed . ,(between 0 (abs n) 3))))))))

(defstruct (slclj-compilation-result
             (:type list)
             (:conc-name slclj-compilation-result.)
             (:constructor nil)
             (:copier nil))
  tag notes successp duration)

(defvar slclj-last-compilation-result nil
  "The result of the most recently issued compilation.")

(defun slclj-compiler-notes ()
  "Return all compiler notes, warnings, and errors."
  (slclj-compilation-result.notes slclj-last-compilation-result))

(defun slclj-compile-and-load-file (&optional policy)
  "Compile and load the buffer's file and highlight compiler notes.

With (positive) prefix argument the file is compiled with maximal
debug settings (`C-u'). With negative prefix argument it is compiled for
speed (`M--'). If a numeric argument is passed set debug or speed settings
to it depending on its sign.

Each source location that is the subject of a compiler note is
underlined and annotated with the relevant information. The commands
`slclj-next-note' and `slclj-previous-note' can be used to navigate
between compiler notes and to display their full details."
  (interactive "P")
  (let ((slclj-compilation-policy (slclj-compute-policy policy)))
    (slclj-compile-file t)))

;;; FIXME: This should become a DEFCUSTOM
(defvar slclj-compile-file-options '()
  "Plist of additional options that C-c C-k should pass to Lisp.
Currently only :fasl-directory is supported.")

(defun slclj-compile-file (&optional load)
  "Compile current buffer's file and highlight resulting compiler notes.

See `slclj-compile-and-load-file' for further details."
  (interactive)
  (check-parens)
  (unless buffer-file-name
    (error "Buffer %s is not associated with a file." (buffer-name)))
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save file %s? " (buffer-file-name))))
    (save-buffer))
  (run-hook-with-args 'slclj-before-compile-functions (point-min) (point-max))
  (let ((file (slclj-to-lisp-filename (buffer-file-name))))
    (slclj-eval-async
     `(swank:compile-file-for-emacs ,file ,(if load t nil) 
                                    :options ',slclj-compile-file-options
                                    :policy ',slclj-compilation-policy)
     #'slclj-compilation-finished)
    (message "Compiling %s..." file)))

(defun slclj-compile-defun (&optional raw-prefix-arg)
  "Compile the current toplevel form. 

With (positive) prefix argument the form is compiled with maximal
debug settings (`C-u'). With negative prefix argument it is compiled for
speed (`M--'). If a numeric argument is passed set debug or speed settings
to it depending on its sign."
  
  (interactive "P")
  (let ((slclj-compilation-policy (slclj-compute-policy raw-prefix-arg)))
    (if (use-region-p)
        (slclj-compile-region (region-beginning) (region-end))
        (apply #'slclj-compile-region (slclj-region-for-defun-at-point)))))

(defun slclj-compile-region (start end)
  "Compile the region."
  (interactive "r")
  (slclj-flash-region start end)
  (run-hook-with-args 'slclj-before-compile-functions start end)
  (slclj-compile-string (buffer-substring-no-properties start end) start))

(defun slclj-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end))) 
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun slclj-compile-string (string start-offset)
  (slclj-eval-async 
   `(swank:compile-string-for-emacs
     ,string
     ,(buffer-name)
     ,start-offset
     ,(if (buffer-file-name) (slclj-to-lisp-filename (buffer-file-name)))
     ',slclj-compilation-policy)
   #'slclj-compilation-finished))

(defun slclj-compilation-finished (result)
  (with-struct (slclj-compilation-result. notes duration successp) result
    (setf slclj-last-compilation-result result)
    (slclj-show-note-counts notes duration successp)
    (when slclj-highlight-compiler-notes
      (slclj-highlight-notes notes))
    (run-hook-with-args 'slclj-compilation-finished-hook notes)))

(defun slclj-show-note-counts (notes secs successp)
  (message (concat 
            (cond (successp "Compilation finished")
                  (t (slclj-add-face 'font-lock-warning-face
                       "Compilation failed")))
            (if (null notes) ". (No warnings)" ": ")
            (mapconcat
             (lambda (messages)
               (destructuring-bind (sev . notes) messages
                 (let ((len (length notes)))
                   (format "%d %s%s" len (slclj-severity-label sev) 
                           (if (= len 1) "" "s")))))
             (sort (slclj-alistify notes #'slclj-note.severity #'eq)
                   (lambda (x y) (slclj-severity< (car y) (car x))))
             "  ")
            (if secs (format "  [%.2f secs]" secs)))))

(defun slclj-highlight-notes (notes)
  "Highlight compiler notes, warnings, and errors in the buffer."
  (interactive (list (slclj-compiler-notes)))
  (with-temp-message "Highlighting notes..."
    (save-excursion
      (save-restriction
        (widen)                  ; highlight notes on the whole buffer
        (slclj-remove-old-overlays)
        (mapc #'slclj-overlay-note (slclj-merge-notes-for-display notes))))))

(defvar slclj-note-overlays '()
  "List of overlays created by `slclj-make-note-overlay'")

(defun slclj-remove-old-overlays ()
  "Delete the existing note overlays."
  (mapc #'delete-overlay slclj-note-overlays)
  (setq slclj-note-overlays '()))

(defun slclj-filter-buffers (predicate)
  "Return a list of where PREDICATE returns true.
PREDICATE is executed in the buffer to test."
  (remove-if-not (lambda (%buffer)
                   (with-current-buffer %buffer
                     (funcall predicate)))
                 (buffer-list)))

;;;;; Recompilation.

;; FIXME: This whole idea is questionable since it depends so
;; crucially on precise source-locs.

(defun slclj-recompile-location (location)
  (save-excursion
    (slclj-goto-source-location location)
    (slclj-compile-defun)))

(defun slclj-recompile-locations (locations cont)
  (slclj-eval-async 
   `(swank:compile-multiple-strings-for-emacs
     ',(loop for loc in locations collect
             (save-excursion 
               (slclj-goto-source-location loc)
               (destructuring-bind (start end)
                   (slclj-region-for-defun-at-point)
                 (list (buffer-substring-no-properties start end)
                       (buffer-name)
                       (slclj-current-package)
                       start
                       (if (buffer-file-name)
                           (file-name-directory (buffer-file-name))
                         nil)))))
     ',slclj-compilation-policy)
   cont))


;;;;; Merging together compiler notes in the same location.

(defun slclj-merge-notes-for-display (notes)
  "Merge together notes that refer to the same location.
This operation is \"lossy\" in the broad sense but not for display purposes."
  (mapcar #'slclj-merge-notes
          (slclj-group-similar 'slclj-notes-in-same-location-p notes)))

(defun slclj-merge-notes (notes)
  "Merge NOTES together. Keep the highest severity, concatenate the messages."
  (let* ((new-severity (reduce #'slclj-most-severe notes
                               :key #'slclj-note.severity))
         (new-message (mapconcat #'slclj-note.message notes "\n")))
    (let ((new-note (copy-list (car notes))))
      (setf (getf new-note :message) new-message)
      (setf (getf new-note :severity) new-severity)
      new-note)))

(defun slclj-notes-in-same-location-p (a b)
  (equal (slclj-note.location a) (slclj-note.location b)))


;;;;; Compiler notes list

(defun slclj-one-line-ify (string)
  "Return a single-line version of STRING.
Each newlines and following indentation is replaced by a single space."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\n[\n \t]*" nil t)
      (replace-match " "))
    (buffer-string)))

(defun slclj-xrefs-for-notes (notes)
  (let ((xrefs))
    (dolist (note notes)
      (let* ((location (getf note :location))
             (fn (cadr (assq :file (cdr location))))
             (file (assoc fn xrefs))
             (node
              (cons (format "%s: %s" 
                            (getf note :severity)
                            (slclj-one-line-ify (getf note :message)))
                    location)))
        (when fn
          (if file
              (push node (cdr file))
              (setf xrefs (acons fn (list node) xrefs))))))
    xrefs))

(defun slclj-maybe-show-xrefs-for-notes (notes)
  "Show the compiler notes NOTES if they come from more than one file."
  (let ((xrefs (slclj-xrefs-for-notes notes)))
    (when (slclj-length> xrefs 1)          ; >1 file
      (slclj-show-xrefs
       xrefs 'definition "Compiler notes" (slclj-current-package)))))

(defun slclj-note-has-location-p (note)
  (not (eq ':error (car (slclj-note.location note)))))

(defun slclj-redefinition-note-p (note)
  (eq (slclj-note.severity note) :redefinition))

(defun slclj-create-compilation-log (notes)
  "Create a buffer for `next-error' to use."
  (with-current-buffer (get-buffer-create "*SLCLJ Compilation*")
    (let ((inhibit-read-only t))
      (erase-buffer))
    (slclj-insert-compilation-log notes)))

(defun slclj-maybe-show-compilation-log (notes)
  "Display the log on failed compilations or if NOTES is non-nil."
  (slclj-create-compilation-log notes)
  (with-struct (slclj-compilation-result. notes duration successp)
      slclj-last-compilation-result
    (unless successp
      (with-current-buffer "*SLCLJ Compilation*"
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "Compilation " (if successp "succeeded." "failed."))
          (goto-char (point-min))
          (display-buffer (current-buffer)))))))

(defun slclj-show-compilation-log (notes)
  "Create and display the compilation log buffer."
  (interactive (list (slclj-compiler-notes)))
  (slclj-with-popup-buffer ("*SLCLJ Compilation*")
    (slclj-insert-compilation-log notes)))

(defun slclj-insert-compilation-log (notes)
  "Insert NOTES in format suitable for `compilation-mode'."
  (multiple-value-bind (grouped-notes canonicalized-locs-table)
      (slclj-group-and-sort-notes notes)
    (with-temp-message "Preparing compilation log..."
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)) ; inefficient font-lock-hook
        (insert (format "cd %s\n%d compiler notes:\n\n"
                        default-directory (length notes)))
        (dolist (notes grouped-notes)
          (let ((loc (gethash (first notes) canonicalized-locs-table))
                (start (point)))
            (insert (slclj-canonicalized-location-to-string loc) ":")
            (slclj-insert-note-group notes)
            (insert "\n")
            (slclj-make-note-overlay (first notes) start (1- (point))))))
      (compilation-mode)
      (set (make-local-variable 'compilation-skip-threshold) 0)
      (setq next-error-last-buffer (current-buffer)))))

(defun slclj-insert-note-group (notes)
  "Insert a group of compiler messages."
  (insert "\n")
  (dolist (note notes)
    (insert "  " (slclj-severity-label (slclj-note.severity note)) ": ")
    (let ((start (point)))
      (insert (slclj-note.message note))
      (let ((ctx (slclj-note.source-context note)))
        (if ctx (insert "\n" ctx)))
      (slclj-indent-block start 4))
    (insert "\n")))

(defun slclj-indent-block (start column)
  "If the region back to START isn't a one-liner indent it."
  (when (< start (line-beginning-position))
    (save-excursion 
      (goto-char start) 
      (insert "\n"))
    (slclj-indent-rigidly start (point) column)))

(defun slclj-canonicalized-location (location)
  "Return a list (FILE LINE COLUMN) for slclj-location LOCATION.
This is quite an expensive operation so use carefully."
  (save-excursion
    (slclj-goto-location-buffer (slclj-location.buffer location))
    (save-excursion
      (slclj-goto-source-location location)
      (list (or (buffer-file-name) (buffer-name))
            (line-number-at-pos)
            (1+ (current-column))))))

(defun slclj-canonicalized-location-to-string (loc)
  (if loc
      (destructuring-bind (filename line col) loc
        (format "%s:%d:%d" 
                (cond ((not filename) "")
                      ((let ((rel (file-relative-name filename)))
                         (if (< (length rel) (length filename))
                             rel)))
                      (t filename))
                line col))
      (format "Unknown location")))

(defun slclj-goto-note-in-compilation-log (note)
  "Find `note' in the compilation log and display it."
  (with-current-buffer (get-buffer "*SLCLJ Compilation*")
    (let ((origin (point))
          (foundp nil))
      (goto-char (point-min))
      (let ((overlay))
        (while (and (setq overlay (slclj-find-next-note))
                    (not foundp))
          (let ((other-note (overlay-get overlay 'slclj-note)))
            (when (slclj-notes-in-same-location-p note other-note)
              (slclj-show-buffer-position (overlay-start overlay) 'top)
              (setq foundp t)))))
      (unless foundp
        (goto-char origin)))))

(defun slclj-group-and-sort-notes (notes)
  "First sort, then group NOTES according to their canonicalized locs."
  (let ((locs (make-hash-table :test #'eq)))
    (mapc (lambda (note)
            (let ((loc (slclj-note.location note)))
              (when (slclj-location-p loc)
                (puthash note (slclj-canonicalized-location loc) locs))))
          notes)
    (values (slclj-group-similar 
             (lambda (n1 n2)
               (equal (gethash n1 locs nil) (gethash n2 locs t)))
             (let* ((bottom most-negative-fixnum) 
                    (+default+ (list "" bottom bottom)))
               (sort notes
                     (lambda (n1 n2)
                       (destructuring-bind (filename1 line1 col1) 
                           (gethash n1 locs +default+)
                         (destructuring-bind (filename2 line2 col2) 
                             (gethash n2 locs +default+)
                           (cond ((string-lessp filename1 filename2) t)
                                 ((string-lessp filename2 filename1) nil)
                                 ((< line1 line2) t)
                                 ((> line1 line2) nil)
                                 (t (< col1 col2)))))))))
            locs)))

(defun slclj-note.severity (note)
  (plist-get note :severity))

(defun slclj-note.message (note)
  (plist-get note :message))

(defun slclj-note.source-context (note)
  (plist-get note :source-context))

(defun slclj-note.location (note)
  (plist-get note :location))

(defun slclj-severity-label (severity)
  (subseq (symbol-name severity) 1))


;;;;; Adding a single compiler note

(defun slclj-overlay-note (note)
  "Add a compiler note to the buffer as an overlay.
If an appropriate overlay for a compiler note in the same location
already exists then the new information is merged into it. Otherwise a
new overlay is created."
  (multiple-value-bind (start end) (slclj-choose-overlay-region note)
    (when start
      (goto-char start)
      (let ((severity (plist-get note :severity))
            (message (plist-get note :message))
            (overlay (slclj-note-at-point)))
        (if overlay
            (slclj-merge-note-into-overlay overlay severity message)
            (slclj-create-note-overlay note start end severity message))))))

(defun slclj-make-note-overlay (note start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'slclj-note note)
    (push overlay slclj-note-overlays)
    overlay))

(defun slclj-create-note-overlay (note start end severity message)
  "Create an overlay representing a compiler note.
The overlay has several properties:
  FACE       - to underline the relevant text.
  SEVERITY   - for future reference :NOTE, :STYLE-WARNING, :WARNING, or :ERROR.
  MOUSE-FACE - highlight the note when the mouse passes over.
  HELP-ECHO  - a string describing the note, both for future reference
               and for display as a tooltip (due to the special
               property name)."
  (let ((overlay (slclj-make-note-overlay note start end)))
    (flet ((putp (name value) (overlay-put overlay name value)))
      (putp 'face (slclj-severity-face severity))
      (putp 'severity severity)
      (putp 'mouse-face 'highlight)
      (putp 'help-echo message)
      overlay)))

;; XXX Obsolete due to `slclj-merge-notes-for-display' doing the
;; work already -- unless we decide to put several sets of notes on a
;; buffer without clearing in between, which only this handles.
(defun slclj-merge-note-into-overlay (overlay severity message)
  "Merge another compiler note into an existing overlay.
The help text describes both notes, and the highest of the severities
is kept."
  (flet ((putp (name value) (overlay-put overlay name value))
	 (getp (name)       (overlay-get overlay name)))
    (putp 'severity (slclj-most-severe severity (getp 'severity)))
    (putp 'face (slclj-severity-face (getp 'severity)))
    (putp 'help-echo (concat (getp 'help-echo) "\n" message))))

(defun slclj-choose-overlay-region (note)
  "Choose the start and end points for an overlay over NOTE.
If the location's sexp is a list spanning multiple lines, then the
region around the first element is used.
Return nil if there's no useful source location."
  (let ((location (slclj-note.location note)))
    (when location 
      (destructure-case location
        ((:error _) _ nil)                 ; do nothing
        ((:location file pos _hints)
         (cond ((eq (car file) ':source-form) nil)
               ((eq (slclj-note.severity note) :read-error)
                (slclj-choose-overlay-for-read-error location))
               ((equal pos '(:eof))
                (list (1- (point-max)) (point-max)))
               (t
                (slclj-choose-overlay-for-sexp location))))))))

(defun slclj-choose-overlay-for-read-error (location)
  (let ((pos (slclj-location-offset location)))
    (save-excursion
      (goto-char pos)
      (cond ((slclj-symbol-at-point)
             ;; package not found, &c.
             (values (slclj-symbol-start-pos) (slclj-symbol-end-pos)))
            (t
             (values pos (1+ pos)))))))
          
(defun slclj-choose-overlay-for-sexp (location)
  (slclj-goto-source-location location)
  (skip-chars-forward "'#`")
  (let ((start (point)))
    (ignore-errors (slclj-forward-sexp))
    (if (slclj-same-line-p start (point))
        (values start (point))
      (values (1+ start)
              (progn (goto-char (1+ start))
                     (ignore-errors (forward-sexp 1))
                     (point))))))

(defun slclj-same-line-p (pos1 pos2)
  "Return t if buffer positions POS1 and POS2 are on the same line."
  (save-excursion (goto-char (min pos1 pos2))
                  (<= (max pos1 pos2) (line-end-position))))

(defvar slclj-severity-face-plist 
  '(:error         slclj-error-face
    :read-error    slclj-error-face
    :warning       slclj-warning-face
    :redefinition  slclj-style-warning-face
    :style-warning slclj-style-warning-face
    :note          slclj-note-face))

(defun slclj-severity-face (severity)
  "Return the name of the font-lock face representing SEVERITY."
  (or (plist-get slclj-severity-face-plist severity)
      (error "No face for: %S" severity)))

(defvar slclj-severity-order 
  '(:note :style-warning :redefinition :warning :error :read-error))

(defun slclj-severity< (sev1 sev2)
  "Return true if SEV1 is less severe than SEV2."
  (< (position sev1 slclj-severity-order)
     (position sev2 slclj-severity-order)))

(defun slclj-most-severe (sev1 sev2)
  "Return the most servere of two conditions."
  (if (slclj-severity< sev1 sev2) sev2 sev1))

;; XXX: unused function
(defun slclj-visit-source-path (source-path)
  "Visit a full source path including the top-level form."
  (goto-char (point-min))
  (slclj-forward-source-path source-path))

;;; The following two functions can be handy when inspecting
;;; source-location while debugging `M-.'.
;;;
(defun slclj-current-tlf-number ()
  "Return the current toplevel number."
  (interactive)
  (let ((original-pos (car (slclj-region-for-defun-at-point)))
        (n 0))
    (save-excursion
      ;; We use this and no repeated `beginning-of-defun's to get
      ;; reader conditionals right.
      (goto-char (point-min))
      (while (progn (slclj-forward-sexp)
                    (< (point) original-pos))
        (incf n)))
    n))

;;; This is similiar to `slclj-enclosing-form-paths' in the
;;; `slclj-parse' contrib except that this does not do any duck-tape
;;; parsing, and gets reader conditionals right.
(defun slclj-current-form-path ()
  "Returns the path from the beginning of the current toplevel
form to the atom at point, or nil if we're in front of a tlf."
  (interactive)
  (let ((source-path nil))
    (save-excursion
      ;; Moving forward to get reader conditionals right.
      (loop for inner-pos = (point)
            for outer-pos = (nth-value 1 (slclj-current-parser-state))
            while outer-pos do
            (goto-char outer-pos)
            (unless (eq (char-before) ?#) ; when at #(...) continue.
              (forward-char)
              (let ((n 0))
                (while (progn (slclj-forward-sexp)
                              (< (point) inner-pos))
                  (incf n))
                (push n source-path)
                (goto-char outer-pos)))))
    source-path))

(defun slclj-forward-positioned-source-path (source-path)
  "Move forward through a sourcepath from a fixed position.
The point is assumed to already be at the outermost sexp, making the
first element of the source-path redundant."
  (ignore-errors 
    (slclj-forward-sexp)
    (beginning-of-defun))
  (when-let (source-path (cdr source-path))
    (down-list 1)
    (slclj-forward-source-path source-path)))

(defun slclj-forward-source-path (source-path)
  (let ((origin (point)))
    (condition-case nil
        (progn
          (loop for (count . more) on source-path
                do (progn
                     (slclj-forward-sexp count)
                     (when more (down-list 1))))
          ;; Align at beginning
          (slclj-forward-sexp)
          (beginning-of-sexp))
      (error (goto-char origin)))))


;; FIXME: really fix this mess
;; FIXME: the check shouln't be done here anyway but by M-. itself.

(defun slclj-filesystem-toplevel-directory ()
  ;; Windows doesn't have a true toplevel root directory, and all
  ;; filenames look like "c:/foo/bar/quux.baz" from an Emacs
  ;; perspective anyway.
  (if (memq system-type '(ms-dos windows-nt))
      ""
      (file-name-as-directory "/")))

(defun slclj-file-name-merge-source-root (target-filename buffer-filename)
  "Returns a filename where the source root directory of TARGET-FILENAME
is replaced with the source root directory of BUFFER-FILENAME.

If no common source root could be determined, return NIL.

E.g. (slclj-file-name-merge-source-root
       \"/usr/local/src/joe/upstream/sbcl/code/late-extensions.lisp\"
       \"/usr/local/src/joe/hacked/sbcl/compiler/deftype.lisp\")
 
        ==> \"/usr/local/src/joe/hacked/sbcl/code/late-extensions.lisp\"
"
  (let ((target-dirs (slclj-split-string (file-name-directory target-filename) "/" t))
        (buffer-dirs (slclj-split-string (file-name-directory buffer-filename) "/" t)))
    ;; Starting from the end, we look if one of the TARGET-DIRS exists
    ;; in BUFFER-FILENAME---if so, it and everything left from that dirname
    ;; is considered to be the source root directory of BUFFER-FILENAME.
    (loop with target-suffix-dirs = nil
          with buffer-dirs* = (reverse buffer-dirs)
          with target-dirs* = (reverse target-dirs)
          for target-dir in target-dirs*
          do (flet ((concat-dirs (dirs)
                      (apply #'concat (mapcar #'file-name-as-directory dirs))))
               (let ((pos (position target-dir buffer-dirs* :test #'equal)))
                 (if (not pos)    ; TARGET-DIR not in BUFFER-FILENAME?
                     (push target-dir target-suffix-dirs)
                     (let* ((target-suffix (concat-dirs target-suffix-dirs)) ; PUSH reversed for us!
                            (buffer-root   (concat-dirs (reverse (nthcdr pos buffer-dirs*)))))
                       (return (concat (slclj-filesystem-toplevel-directory)
                                       buffer-root
                                       target-suffix
                                       (file-name-nondirectory target-filename))))))))))

(defun slclj-highlight-differences-in-dirname (base-dirname contrast-dirname)
  "Returns a copy of BASE-DIRNAME where all differences between
BASE-DIRNAME and CONTRAST-DIRNAME are propertized with a
highlighting face."
  (setq base-dirname (file-name-as-directory base-dirname))
  (setq contrast-dirname (file-name-as-directory contrast-dirname))
  (flet ((insert-dir (dirname)
           (insert (file-name-as-directory dirname)))
         (insert-dir/propzd (dirname)
           (slclj-insert-propertized '(face highlight) dirname)
           (insert "/")))  ; Not exactly portable (to VMS...)
    (let ((base-dirs (slclj-split-string base-dirname "/" t))
          (contrast-dirs (slclj-split-string contrast-dirname "/" t)))
      (with-temp-buffer
        (loop initially (insert (slclj-filesystem-toplevel-directory))
              for base-dir in base-dirs do
              (let ((pos (position base-dir contrast-dirs :test #'equal)))
                (if (not pos)
                    (insert-dir/propzd base-dir)
                    (progn (insert-dir base-dir)
                           (setq contrast-dirs (nthcdr (1+ pos) contrast-dirs))))))
        (buffer-substring (point-min) (point-max))))))

(defvar slclj-warn-when-possibly-tricked-by-M-. t
  "When working on multiple source trees simultaneously, the way
`slclj-edit-definition' (M-.) works can sometimes be confusing:

`M-.' visits locations that are present in the current Lisp image,
which works perfectly well as long as the image reflects the source
tree that one is currently looking at.

In the other case, however, one can easily end up visiting a file
in a different source root directory (the one corresponding to
the Lisp image), and is thus easily tricked to modify the wrong
source files---which can lead to quite some stressfull cursing.

If this variable is T, a warning message is issued to raise the
user's attention whenever `M-.' is about opening a file in a
different source root that also exists in the source root
directory of the user's current buffer.

There's no guarantee that all possible cases are covered, but
if you encounter such a warning, it's a strong indication that
you should check twice before modifying.")

(defun slclj-maybe-warn-for-different-source-root (target-filename buffer-filename)
  (let ((guessed-target (slclj-file-name-merge-source-root target-filename
                                                           buffer-filename)))
    (when (and guessed-target
               (not (equal guessed-target target-filename))
               (file-exists-p guessed-target))
      (slclj-message "Attention: This is `%s'."
                     (concat (slclj-highlight-differences-in-dirname
                              (file-name-directory target-filename)
                              (file-name-directory guessed-target))
                             (file-name-nondirectory target-filename))))))

(defun slclj-check-location-filename-sanity (filename)
  (when slclj-warn-when-possibly-tricked-by-M-.
    (flet ((file-truename-safe (filename) (and filename (file-truename filename))))
      (let ((target-filename (file-truename-safe filename))
            (buffer-filename (file-truename-safe (buffer-file-name))))
        (when buffer-filename
          (slclj-maybe-warn-for-different-source-root
           target-filename buffer-filename))))))

(defun slclj-check-location-buffer-name-sanity (buffer-name)
  (slclj-check-location-filename-sanity
   (buffer-file-name (get-buffer buffer-name))))



(defun slclj-goto-location-buffer (buffer)
  (destructure-case buffer
    ((:file filename)
     (let ((filename (slclj-from-lisp-filename filename)))
       (slclj-check-location-filename-sanity filename)
       (set-buffer (or (get-file-buffer filename)
                       (let ((find-file-suppress-same-file-warnings t))
                         (find-file-noselect filename))))))
    ((:buffer buffer-name)
     (slclj-check-location-buffer-name-sanity buffer-name)
     (set-buffer buffer-name))
    ((:source-form string)
     (set-buffer (get-buffer-create "*SLCLJ Source Form*"))
     (erase-buffer)
     (lisp-mode)
     (insert string)
     (goto-char (point-min)))
    ((:zip file entry)
     (require 'arc-mode)
     (set-buffer (find-file-noselect file t))
     (goto-char (point-min))
     (re-search-forward (concat "  " entry "$"))
     (let ((buffer (save-window-excursion
                     (archive-extract)
                     (current-buffer))))
       (set-buffer buffer)
       (goto-char (point-min))))))

(defun slclj-goto-location-position (position)
  (destructure-case position
    ((:position pos)
     (goto-char 1)
     (forward-char (- (1- pos) (slclj-eol-conversion-fixup (1- pos)))))
    ((:offset start offset)
     (goto-char start)
     (forward-char offset))
    ((:line start &optional column)
     (goto-char (point-min))
     (beginning-of-line start)
     (cond (column (move-to-column column))
           (t (skip-chars-forward " \t"))))
    ((:function-name name)
     (let ((case-fold-search t)
           (name (regexp-quote name)))
       (when (or 
              (re-search-forward 
               (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +%s\\S_" name) nil t)
              (re-search-forward 
               (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +(*%s\\S_" name) nil t)
              (re-search-forward 
               (format "[( \t]%s\\>\\(\\s \\|$\\)" name) nil t))
         (goto-char (match-beginning 0)))))
    ((:method name specializers &rest qualifiers)
     (slclj-search-method-location name specializers qualifiers))
    ((:source-path source-path start-position)
     (cond (start-position
            (goto-char start-position)
            (slclj-forward-positioned-source-path source-path))
           (t
            (slclj-forward-source-path source-path))))
    ((:eof)
     (goto-char (point-max)))))

(defun slclj-eol-conversion-fixup (n)
  ;; Return the number of \r\n eol markers that we need to cross when
  ;; moving N chars forward.  N is the number of chars but \r\n are
  ;; counted as 2 separate chars.
  (case (coding-system-eol-type buffer-file-coding-system)
    ((1) 
     (save-excursion 
       (do ((pos (+ (point) n))
            (count 0 (1+ count)))
           ((>= (point) pos) (1- count))
         (forward-line)
         (decf pos))))
    (t 0)))

(defun slclj-search-method-location (name specializers qualifiers)
  ;; Look for a sequence of words (def<something> method name
  ;; qualifers specializers don't look for "T" since it isn't requires
  ;; (arg without t) as class is taken as such.
  (let* ((case-fold-search t)
         (name (regexp-quote name))
         (qualifiers (mapconcat (lambda (el) (concat ".+?\\<" el "\\>"))
                                qualifiers ""))
         (specializers (mapconcat (lambda (el) 
                                    (if (eql (aref el 0) ?\()
                                        (let ((spec (read el)))
                                          (if (eq (car spec) 'EQL)
                                              (concat ".*?\\n\\{0,1\\}.*?(EQL.*?'\\{0,1\\}"
                                                      (format "%s" (second spec)) ")")
                                            (error "don't understand specializer: %s,%s" el (car spec))))
                                      (concat ".+?\n\\{0,1\\}.+?\\<" el "\\>")))
                                  (remove "T" specializers) ""))
         (regexp (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +%s\\s +%s%s" name
                         qualifiers specializers)))
    (or (and (re-search-forward regexp  nil t)
             (goto-char (match-beginning 0)))
        ;;	(slclj-goto-location-position `(:function-name ,name))
        )))

(defun slclj-search-call-site (fname)
  "Move to the place where FNAME called.
Don't move if there are multiple or no calls in the current defun."
  (save-restriction 
    (narrow-to-defun)
    (let ((start (point))
          (regexp (concat "(" fname "[\n \t]")))
      (cond ((and (re-search-forward regexp nil t)
                  (not (re-search-forward regexp nil t)))
             (goto-char (match-beginning 0)))
            (t (goto-char start))))))

(defun slclj-goto-source-location (location &optional noerror)
  "Move to the source location LOCATION.  Several kinds of locations
are supported:

<location> ::= (:location <buffer> <position> <hints>)
             | (:error <message>) 

<buffer>   ::= (:file <filename>)
             | (:buffer <buffername>)
             | (:source-form <string>)
             | (:zip <file> <entry>)

<position> ::= (:position <fixnum>) ; 1 based (for files)
             | (:offset <start> <offset>) ; start+offset (for C-c C-c)
             | (:line <line> [<column>])
             | (:function-name <string>)
             | (:source-path <list> <start-position>) 
             | (:method <name string> <specializer strings> . <qualifiers strings>)"
  (destructure-case location
    ((:location buffer position hints)
     (slclj-goto-location-buffer buffer)
     (let ((pos (slclj-location-offset location)))
       (cond ((and (<= (point-min) pos) (<= pos (point-max))))
             (widen-automatically (widen))
             (t (error "Location is outside accessible part of buffer")))
       (goto-char pos)))
    ((:error message)
     (if noerror
         (slclj-message "%s" message)
       (error "%s" message)))))

(defun slclj-location-offset (location)
  "Return the position, as character number, of LOCATION."
  (save-restriction
    (widen)
    (slclj-goto-location-position (slclj-location.position location))
    (let ((hints (slclj-location.hints location)))
      (when-let (snippet (getf hints :snippet))
        (slclj-isearch snippet))
      (when-let (fname (getf hints :call-site))
        (slclj-search-call-site fname))
      (when (getf hints :align)
        (slclj-forward-sexp)
        (beginning-of-sexp)))
    (point)))


;;;;; Incremental search
;;
;; Search for the longest match of a string in either direction.
;;
;; This is for locating text that is expected to be near the point and
;; may have been modified (but hopefully not near the beginning!)

(defun slclj-isearch (string)
  "Find the longest occurence of STRING either backwards of forwards.
If multiple matches exist the choose the one nearest to point."
  (goto-char
   (let* ((start (point))
          (len1 (slclj-isearch-with-function 'search-forward string))
          (pos1 (point)))
     (goto-char start)
     (let* ((len2 (slclj-isearch-with-function 'search-backward string))
            (pos2 (point)))
       (cond ((and len1 len2)
              ;; Have a match in both directions
              (cond ((= len1 len2)
                     ;; Both are full matches -- choose the nearest.
                     (if (< (abs (- start pos1))
                            (abs (- start pos2)))
                         pos1 pos2))
                    ((> len1 len2) pos1)
                    ((> len2 len1) pos2)))
             (len1 pos1)
             (len2 pos2)
             (t start))))))

(defun slclj-isearch-with-function (search-fn string)
  "Search for the longest substring of STRING using SEARCH-FN.
SEARCH-FN is either the symbol `search-forward' or `search-backward'."
  (unless (string= string "")
    (loop for i from 1 to (length string)
          while (funcall search-fn (substring string 0 i) nil t)
          for match-data = (match-data)
          do (case search-fn
               (search-forward  (goto-char (match-beginning 0)))
               (search-backward (goto-char (1+ (match-end 0)))))
          finally (return (if (null match-data)
                              nil
                            ;; Finish based on the last successful match
                            (store-match-data match-data)
                            (goto-char (match-beginning 0))
                            (- (match-end 0) (match-beginning 0)))))))


;;;;; Visiting and navigating the overlays of compiler notes

(defun slclj-next-note ()
  "Go to and describe the next compiler note in the buffer."
  (interactive)
  (let ((here (point))
        (note (slclj-find-next-note)))
    (if note
        (slclj-show-note note)
      (goto-char here)
      (message "No next note."))))

(defun slclj-previous-note ()
  "Go to and describe the previous compiler note in the buffer."
  (interactive)
  (let ((here (point))
        (note (slclj-find-previous-note)))
    (if note
        (slclj-show-note note)
      (goto-char here)
      (message "No previous note."))))

(defun slclj-goto-first-note (&rest ignore)
  "Go to the first note in the buffer."
  (let ((point (point)))
    (goto-char (point-min))
    (cond ((slclj-find-next-note)
           (slclj-show-note (slclj-note-at-point)))
          (t (goto-char point)))))

(defun slclj-remove-notes ()
  "Remove compiler-note annotations from the current buffer."
  (interactive)
  (slclj-remove-old-overlays))

(defun slclj-show-note (overlay)
  "Present the details of a compiler note to the user."
  (slclj-temporarily-highlight-note overlay)
  (if (get-buffer-window "*SLCLJ Compilation*" t)
      (slclj-goto-note-in-compilation-log (overlay-get overlay 'slclj-note))
      (let ((message (get-char-property (point) 'help-echo)))
        (slclj-message "%s" (if (zerop (length message)) "\"\"" message)))))

;; FIXME: could probably use flash region
(defun slclj-temporarily-highlight-note (overlay)
  "Temporarily highlight a compiler note's overlay.
The highlighting is designed to both make the relevant source more
visible, and to highlight any further notes that are nested inside the
current one.

The highlighting is automatically undone with a timer."
  (run-with-timer 0.2 nil
                  #'overlay-put overlay 'face (overlay-get overlay 'face))
  (overlay-put overlay 'face 'slclj-highlight-face))


;;;;; Overlay lookup operations

(defun slclj-note-at-point ()
  "Return the overlay for a note starting at point, otherwise NIL."
  (find (point) (slclj-note-overlays-at-point)
	:key 'overlay-start))

(defun slclj-note-overlay-p (overlay)
  "Return true if OVERLAY represents a compiler note."
  (overlay-get overlay 'slclj-note))

(defun slclj-note-overlays-at-point ()
  "Return a list of all note overlays that are under the point."
  (remove-if-not 'slclj-note-overlay-p (overlays-at (point))))

(defun slclj-find-next-note ()
  "Go to the next position with the `slclj-note' text property.
Retuns the note overlay if such a position is found, otherwise nil."
  (slclj-search-property 'slclj-note nil #'slclj-note-at-point))

(defun slclj-find-previous-note ()
  "Go to the next position with the `slclj-note' text property.
Retuns the note overlay if such a position is found, otherwise nil."
  (slclj-search-property 'slclj-note t #'slclj-note-at-point))


;;;; Arglist Display

(defun slclj-space (n)
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key.  Prefix argument can be used to insert
more than one space."
  (interactive "p")
  (self-insert-command n)
  (when (slclj-background-activities-enabled-p)
    (slclj-echo-arglist)))

(put 'slclj-space 'delete-selection t) ; for delete-section-mode & CUA

(defvar slclj-echo-arglist-function 'slclj-show-arglist)

(defun slclj-echo-arglist ()
  "Display the arglist of the current form in the echo area."
  (funcall slclj-echo-arglist-function))

(defun slclj-show-arglist ()
  (let ((op (slclj-operator-before-point)))
    (when op 
      (slclj-eval-async `(swank:operator-arglist ,op ,(slclj-current-package))
			(lambda (arglist)
			  (when arglist
			    (slclj-message "%s" arglist)))))))

(defun slclj-operator-before-point ()
  (ignore-errors 
    (save-excursion
      (backward-up-list 1)
      (down-list 1)
      (slclj-symbol-at-point))))


;;;; Completion

;; XXX those long names are ugly to read; long names an indicator for
;; bad factoring?

(defvar slclj-completions-buffer-name "*Completions*")

(make-variable-buffer-local
 (defvar slclj-complete-saved-window-configuration nil
   "Window configuration before we show the *Completions* buffer.
This is buffer local in the buffer where the completion is
performed."))

(make-variable-buffer-local
 (defvar slclj-completions-window nil
   "The window displaying *Completions* after saving window configuration.
If this window is no longer active or displaying the completions
buffer then we can ignore `slclj-complete-saved-window-configuration'."))

(defun slclj-complete-maybe-save-window-configuration ()
  "Maybe save the current window configuration.
Return true if the configuration was saved."
  (unless (or slclj-complete-saved-window-configuration
              (get-buffer-window slclj-completions-buffer-name))
    (setq slclj-complete-saved-window-configuration
          (current-window-configuration))
    t))

(defun slclj-complete-delay-restoration ()
  (slclj-add-local-hook 'pre-command-hook
                        'slclj-complete-maybe-restore-window-configuration))

(defun slclj-complete-forget-window-configuration ()
  (setq slclj-complete-saved-window-configuration nil)
  (setq slclj-completions-window nil))

(defun slclj-complete-restore-window-configuration ()
  "Restore the window config if available."
  (remove-hook 'pre-command-hook
               'slclj-complete-maybe-restore-window-configuration)
  (when (and slclj-complete-saved-window-configuration
             (slclj-completion-window-active-p))
    ;; XEmacs does not allow us to restore a window configuration from
    ;; pre-command-hook, so we do it asynchronously.
    (slclj-run-when-idle
     (lambda ()
       (save-excursion
         (set-window-configuration
          slclj-complete-saved-window-configuration))
       (setq slclj-complete-saved-window-configuration nil)
       (when (buffer-live-p slclj-completions-buffer-name)
         (kill-buffer slclj-completions-buffer-name))))))

(defun slclj-complete-maybe-restore-window-configuration ()
  "Restore the window configuration, if the following command
terminates a current completion."
  (remove-hook 'pre-command-hook
               'slclj-complete-maybe-restore-window-configuration)
  (condition-case err
      (cond ((find last-command-char "()\"'`,# \r\n:")
             (slclj-complete-restore-window-configuration))
            ((not (slclj-completion-window-active-p))
             (slclj-complete-forget-window-configuration))
            (t
             (slclj-complete-delay-restoration)))
    (error
     ;; Because this is called on the pre-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in slclj-complete-restore-window-configuration: %S" err))))

(defun slclj-completion-window-active-p ()
  "Is the completion window currently active?"
  (and (window-live-p slclj-completions-window)
       (equal (buffer-name (window-buffer slclj-completions-window))
              slclj-completions-buffer-name)))

(defun slclj-display-completion-list (completions base)
  (let ((savedp (slclj-complete-maybe-save-window-configuration)))
    (with-output-to-temp-buffer slclj-completions-buffer-name
      (display-completion-list completions)
      (let ((offset (- (point) 1 (length base))))
        (with-current-buffer standard-output
          (setq completion-base-size offset)
          (set-syntax-table lisp-mode-syntax-table))))
    (when savedp
      (setq slclj-completions-window
            (get-buffer-window slclj-completions-buffer-name)))))
  
(defun slclj-display-or-scroll-completions (completions base)
  (cond ((and (eq last-command this-command)
              (slclj-completion-window-active-p))
         (slclj-scroll-completions))
        (t
         (slclj-display-completion-list completions base)))
  (slclj-complete-delay-restoration))

(defun slclj-scroll-completions ()
  (let ((window slclj-completions-window))
    (with-current-buffer (window-buffer window)
      (if (pos-visible-in-window-p (point-max) window)
          (set-window-start window (point-min))
        (save-selected-window
          (select-window window)
          (scroll-up))))))

(defun slclj-complete-symbol ()
  "Complete the symbol at point.

Completion is performed by `slclj-complete-symbol-function'."
  (interactive)
  (funcall slclj-complete-symbol-function))

(defun slclj-simple-complete-symbol ()
  "Complete the symbol at point.  
Perform completion more similar to Emacs' complete-symbol."
  (or (slclj-maybe-complete-as-filename)
      (let* ((end (point))
             (beg (slclj-symbol-start-pos))
             (prefix (buffer-substring-no-properties beg end))
             (result (slclj-simple-completions prefix)))
        (destructuring-bind (completions partial) result
          (if (null completions)
              (progn (slclj-minibuffer-respecting-message
                      "Can't find completion for \"%s\"" prefix)
                     (ding)
                     (slclj-complete-restore-window-configuration))
            (insert-and-inherit (substring partial (length prefix)))
            (cond ((slclj-length= completions 1)
                   (slclj-minibuffer-respecting-message "Sole completion")
                   (slclj-complete-restore-window-configuration))
                  ;; Incomplete
                  (t
                   (slclj-minibuffer-respecting-message
                    "Complete but not unique")
                   (slclj-display-or-scroll-completions completions
                                                        partial))))))))

(defun slclj-maybe-complete-as-filename ()
  "If point is at a string starting with \", complete it as filename.
Return nil if point is not at filename."
  (if (save-excursion (re-search-backward "\"[^ \t\n]+\\=" nil t))
      (let ((comint-completion-addsuffix '("/" . "\"")))
        (comint-replace-by-expanded-filename)
        t)
    nil))

(defun slclj-minibuffer-respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
        (if (fboundp 'temp-minibuffer-message) ;; XEmacs
            (temp-minibuffer-message text)
          (minibuffer-message text))
      (message "%s" text))))

(defun slclj-indent-and-complete-symbol ()
  "Indent the current line and perform symbol completion.
First indent the line. If indenting doesn't move point, complete
the symbol. If there's no symbol at the point, show the arglist
for the most recently enclosed macro or function."
  (interactive)
  (let ((pos (point)))
    (unless (get-text-property (line-beginning-position) 'slclj-repl-prompt)
      (lisp-indent-line))
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (slclj-complete-symbol))
            ((memq (char-before) '(?\t ?\ ))
             (slclj-echo-arglist))))))

(defvar slclj-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" 'slclj-complete-symbol)
    (define-key map "\M-\t" 'slclj-complete-symbol)
    map)
  "Minibuffer keymap used for reading CL expressions.")

(defvar slclj-minibuffer-history '()
  "History list of expressions read from the minibuffer.")
 
(defun slclj-minibuffer-setup-hook ()
  (cons (lexical-let ((package (slclj-current-package))
                      (connection (slclj-connection)))
          (lambda ()
            (setq slclj-buffer-package package)
            (setq slclj-buffer-connection connection)
            (set-syntax-table lisp-mode-syntax-table)))
        minibuffer-setup-hook))

(defun slclj-read-from-minibuffer (prompt &optional initial-value history)
  "Read a string from the minibuffer, prompting with PROMPT.  
If INITIAL-VALUE is non-nil, it is inserted into the minibuffer before
reading input.  The result is a string (\"\" if no input was given)."
  (let ((minibuffer-setup-hook (slclj-minibuffer-setup-hook)))
    (read-from-minibuffer prompt initial-value slclj-minibuffer-map
			  nil 'slclj-minibuffer-history)))

(defun slclj-bogus-completion-alist (list)
  "Make an alist out of list.
The same elements go in the CAR, and nil in the CDR. To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))

(defun slclj-simple-completions (prefix)
  (let ((slclj-current-thread t))
    (slclj-eval
     `(swank:simple-completions ,prefix ',(slclj-current-package)))))


;;;; Edit definition

(defun slclj-push-definition-stack ()
  "Add point to find-tag-marker-ring."
  (require 'etags)
  (cond ((featurep 'xemacs)
         (push-tag-mark))
        (t (ring-insert find-tag-marker-ring (point-marker)))))

(defun slclj-pop-find-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (cond ((featurep 'xemacs) (pop-tag-mark nil))
        (t (pop-tag-mark))))

(defstruct (slclj-xref (:conc-name slclj-xref.) (:type list))
  dspec location)

(defstruct (slclj-location (:conc-name slclj-location.) (:type list)
                           (:constructor nil)
                           (:copier nil))
  tag buffer position hints)

(defun slclj-location-p (o) (and (consp o) (eq (car o) :location)))

(defun slclj-xref-has-location-p (xref)
  (slclj-location-p (slclj-xref.location xref)))

(defun make-slclj-buffer-location (buffer-name position &optional hints)
  `(:location (:buffer ,buffer-name) (:position ,position)
              ,(when hints `(:hints ,hints))))

(defun make-slclj-file-location (file-name position &optional hints)
  `(:location (:file ,file-name) (:position ,position)
              ,(when hints `(:hints ,hints))))

;;; The hooks are tried in order until one succeeds, otherwise the
;;; default implementation involving `slclj-find-definitions-function'
;;; is used. The hooks are called with the same arguments as
;;; `slclj-edit-definition'.
(defvar slclj-edit-definition-hooks)

(defun slclj-edit-definition (name &optional where)
  "Lookup the definition of the name at point.  
If there's no name at point, or a prefix argument is given, then the
function name is prompted."
  (interactive (list (slclj-read-symbol-name "Edit Definition of: ")))
  (or (run-hook-with-args-until-success 'slclj-edit-definition-hooks 
                                        name where)
      (slclj-edit-definition-cont (slclj-find-definitions name)
                                  name where)))

(defun slclj-edit-definition-cont (xrefs name where)
  (destructuring-bind (1loc file-alist) (slclj-analyze-xrefs xrefs)
    (cond ((null xrefs) 
           (error "No known definition for: %s (in %s)"
                  name (slclj-current-package)))
          (1loc
           (slclj-push-definition-stack)
           (slclj-pop-to-location (slclj-xref.location (car xrefs)) where))
          ((slclj-length= xrefs 1)      ; ((:error "..."))
           (error "%s" (cadr (slclj-xref.location (car xrefs)))))
          (t
           (slclj-push-definition-stack)
           (slclj-show-xrefs file-alist 'definition name
                             (slclj-current-package))))))

(defvar slclj-edit-uses-xrefs 
  '(:calls :macroexpands :binds :references :sets :specializes))

;;; FIXME. TODO: Would be nice to group the symbols (in each
;;;              type-group) by their home-package.
(defun slclj-edit-uses (symbol)
  "Lookup all the uses of SYMBOL."
  (interactive (list (slclj-read-symbol-name "Edit Uses of: ")))
  (slclj-xrefs slclj-edit-uses-xrefs
               symbol
               (lambda (xrefs type symbol package)
                 (cond
                  ((null xrefs)
                   (message "No xref information found for %s." symbol))
                  ((and (slclj-length= xrefs 1)          ; one group
                        (slclj-length= (cdar  xrefs) 1)) ; one ref in group
                   (destructuring-bind (_ (_ loc)) (first xrefs)
                     (slclj-push-definition-stack)
                     (slclj-pop-to-location loc)))
                  (t
                   (slclj-push-definition-stack)
                   (slclj-show-xref-buffer xrefs type symbol package))))))

(defun slclj-analyze-xrefs (xrefs)
  "Find common filenames in XREFS.
Return a list (SINGLE-LOCATION FILE-ALIST).
SINGLE-LOCATION is true if all xrefs point to the same location.
FILE-ALIST is an alist of the form ((FILENAME . (XREF ...)) ...)."
  (list (and xrefs
             (let ((loc (slclj-xref.location (car xrefs))))
               (and (slclj-location-p loc)
                    (every (lambda (x) (equal (slclj-xref.location x) loc))
                           (cdr xrefs)))))
        (slclj-alistify xrefs #'slclj-xref-group #'equal)))

(defun slclj-xref-group (xref)
  (cond ((slclj-xref-has-location-p xref)
         (destructure-case (slclj-location.buffer (slclj-xref.location xref))
           ((:file filename) filename)
           ((:buffer bufname)
            (let ((buffer (get-buffer bufname)))
              (if buffer 
                  (format "%S" buffer) ; "#<buffer foo.lisp>"
                (format "%s (previously existing buffer)" bufname))))
           ((:source-form _) "(S-Exp)")
           ((:zip zip entry) entry)))
        (t
         "(No location)")))

(defun slclj-pop-to-location (location &optional where)
  (slclj-goto-source-location location)
  (ecase where
    ((nil)     (switch-to-buffer (current-buffer)))
    (window    (pop-to-buffer (current-buffer) t))
    (frame     (let ((pop-up-frames t)) (pop-to-buffer (current-buffer) t)))))

(defun slclj-postprocess-xref (original-xref)
  "Process (for normalization purposes) an Xref comming directly
from SWANK before the rest of Slclj sees it. In particular,
convert ETAGS based xrefs to actual file+position based
locations."
  (if (not (slclj-xref-has-location-p original-xref))
      (list original-xref)
      (let ((loc (slclj-xref.location original-xref)))
        (destructure-case (slclj-location.buffer loc)
          ((:etags-file tags-file)
           (destructure-case (slclj-location.position loc)
             ((:tag &rest tags)
              (visit-tags-table tags-file)
              (mapcar #'(lambda (xref)
                          (let ((old-dspec (slclj-xref.dspec original-xref))
                                (new-dspec (slclj-xref.dspec xref)))
                            (setf (slclj-xref.dspec xref) 
                                  (format "%s: %s" old-dspec new-dspec))
                            xref))
                      (mapcan #'slclj-etags-definitions tags)))))
          (t 
           (list original-xref))))))

(defun slclj-postprocess-xrefs (xrefs)
  (mapcan #'slclj-postprocess-xref xrefs))

(defun slclj-find-definitions (name)
  "Find definitions for NAME."
  (slclj-postprocess-xrefs (funcall slclj-find-definitions-function name)))

(defun slclj-find-definitions-rpc (name)
  (slclj-eval `(swank:find-definitions-for-emacs ,name)))
 
(defun slclj-edit-definition-other-window (name)
  "Like `slclj-edit-definition' but switch to the other window."
  (interactive (list (slclj-read-symbol-name "Symbol: ")))
  (slclj-edit-definition name 'window))

(defun slclj-edit-definition-other-frame (name)
  "Like `slclj-edit-definition' but switch to the other window."
  (interactive (list (slclj-read-symbol-name "Symbol: ")))
  (slclj-edit-definition name 'frame))

(defun slclj-edit-definition-with-etags (name)
  (interactive (list (slclj-read-symbol-name "Symbol: ")))
  (let ((xrefs (slclj-etags-definitions name)))
    (cond (xrefs 
           (message "Using tag file...")
           (slclj-edit-definition-cont xrefs name nil))
          (t
           (error "No known definition for: %s" name)))))

(defun slclj-etags-to-locations (name)
  "Search for definitions matching `name' in the currently active
tags table. Return a possibly empty list of slclj-locations."
  (let ((locs '()))
    (save-excursion
      (let ((first-time t))
        (while (visit-tags-table-buffer (not first-time))
          (setq first-time nil)
          (goto-char (point-min))
          (while (search-forward name nil t)
            (beginning-of-line)
            (destructuring-bind (hint line &rest pos) (etags-snarf-tag)
              (unless (eq hint t) ; hint==t if we are in a filename line
                (push `(:location (:file ,(expand-file-name (file-of-tag)))
                                  (:line ,line)
                                  (:snippet ,hint)) 
                       locs))))))
      (nreverse locs))))

(defun slclj-etags-definitions (name)
  "Search definitions matching NAME in the tags file.
The result is a (possibly empty) list of definitions."
  (mapcar #'(lambda (loc)
              (make-slclj-xref :dspec (second (slclj-location.hints loc))
                               :location loc))
          (slclj-etags-to-locations name)))

;;;;; first-change-hook

(defun slclj-first-change-hook ()
  "Notify Lisp that a source file's buffer has been modified."
  ;; Be careful not to disturb anything!
  ;; In particular if we muck up the match-data then query-replace
  ;; breaks. -luke (26/Jul/2004)
  (save-excursion
    (save-match-data
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (slclj-background-activities-enabled-p))
        (let ((filename (slclj-to-lisp-filename (buffer-file-name))))          
           (slclj-eval-async `(swank:buffer-first-change ,filename)))))))

(defun slclj-setup-first-change-hook ()
  (add-hook (make-local-variable 'first-change-hook)
            'slclj-first-change-hook))

(add-hook 'slclj-mode-hook 'slclj-setup-first-change-hook)


;;;; Eval for Lisp

(defun slclj-eval-for-lisp (thread tag form-string)
  (let ((ok nil) 
        (value nil)
        (c (slclj-connection)))
    (unwind-protect (progn
                      (slclj-check-eval-in-emacs-enabled)
                      (setq value (eval (read form-string)))
                      (setq ok t))
      (let ((result (if ok `(:ok ,value) `(:abort))))
        (slclj-dispatch-event `(:emacs-return ,thread ,tag ,result) c)))))

(defun slclj-check-eval-in-emacs-enabled ()
  "Raise an error if `slclj-enable-evaluate-in-emacs' isn't true."
  (unless slclj-enable-evaluate-in-emacs
    (error (concat "slclj-eval-in-emacs disabled for security."
                   "Set slclj-enable-evaluate-in-emacs true to enable it."))))


;;;; `ED'

(defvar slclj-ed-frame nil
  "The frame used by `slclj-ed'.")

(defcustom slclj-ed-use-dedicated-frame t
  "*When non-nil, `slclj-ed' will create and reuse a dedicated frame."
  :type 'boolean
  :group 'slclj-mode)

(defun slclj-ed (what)
  "Edit WHAT.

WHAT can be:
  A filename (string),
  A list (:filename FILENAME &key LINE COLUMN POSITION),
  A function name (:function-name STRING)
  nil.

This is for use in the implementation of COMMON-LISP:ED."
  (when slclj-ed-use-dedicated-frame
    (unless (and slclj-ed-frame (frame-live-p slclj-ed-frame))
      (setq slclj-ed-frame (make-frame)))
    (select-frame slclj-ed-frame))
  (when what
    (destructure-case what
      ((:filename file &key line column position)
       (find-file (slclj-from-lisp-filename file))
       (when line (goto-line line))
       (when column (move-to-column column))
       (when position (goto-char position)))
      ((:function-name name)
       (slclj-edit-definition name)))))

(defun slclj-y-or-n-p (thread tag question)
  (slclj-dispatch-event `(:emacs-return ,thread ,tag ,(y-or-n-p question))))

(defun slclj-read-from-minibuffer-for-swank (thread tag prompt initial-value)
  (let ((answer (condition-case nil 
                    (slclj-read-from-minibuffer prompt initial-value)
                  (quit nil))))
    (slclj-dispatch-event `(:emacs-return ,thread ,tag ,answer))))

;;;; Interactive evaluation.

(defun slclj-interactive-eval (string)
  "Read and evaluate STRING and print value in minibuffer.

Note: If a prefix argument is in effect then the result will be
inserted in the current buffer."
  (interactive (list (slclj-read-from-minibuffer "Slclj Eval: ")))
  (cond ((not current-prefix-arg)
         (slclj-eval-with-transcript `(swank:interactive-eval ,string)))
        (t
         (slclj-eval-print string))))

(defun slclj-display-eval-result (value)
  (slclj-message "%s" value))

(defun slclj-eval-print (string)
  "Eval STRING in Lisp; insert any output and the result at point."
  (slclj-eval-async `(swank:eval-and-grab-output ,string)
                    (lambda (result)
                      (destructuring-bind (output value) result
                        (insert output value)))))

(defvar slclj-transcript-start-hook nil
  "Hook run before start an evalution.")
(defvar slclj-transcript-stop-hook nil
  "Hook run after finishing a evalution.")

(defun slclj-eval-with-transcript (form)
  "Eval FROM in Lisp.  Display output, if any."
  (run-hooks 'slclj-transcript-start-hook)
  (slclj-rex () (form)
    ((:ok value)
     (run-hooks 'slclj-transcript-stop-hook)
     (slclj-display-eval-result value))
    ((:abort)
     (run-hooks 'slclj-transcript-stop-hook)
     (message "Evaluation aborted."))))
        
(defun slclj-eval-describe (form)
  "Evaluate FORM in Lisp and display the result in a new buffer."
  (slclj-eval-async form (slclj-rcurry #'slclj-show-description
                                       (slclj-current-package))))

(defvar slclj-description-autofocus nil
  "If non-nil select description windows on display.")

(defun slclj-show-description (string package)
  ;; So we can have one description buffer open per connection. Useful
  ;; for comparing the output of DISASSEMBLE across implementations.
  ;; FIXME: could easily be achieved with M-x rename-buffer
  (let ((bufname (format "*SLCLJ Description <%s>*" (slclj-connection-name))))
    (slclj-with-popup-buffer (bufname package t slclj-description-autofocus)
      (princ string)
      (goto-char (point-min)))))

(defun slclj-last-expression ()
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun slclj-eval-last-expression ()
  "Evaluate the expression preceding point."
  (interactive)
  (slclj-interactive-eval (slclj-last-expression)))
  
(defun slclj-eval-defun ()
  "Evaluate the current toplevel form.
Use `slclj-re-evaluate-defvar' if the from starts with '(defvar'"
  (interactive)
  (let ((form (slclj-defun-at-point)))
    (cond ((string-match "^(defvar " form)
           (slclj-re-evaluate-defvar form))
          (t
           (slclj-interactive-eval form)))))

(defun slclj-eval-region (start end)
  "Evaluate region."
  (interactive "r")
  (slclj-eval-with-transcript 
   `(swank:interactive-eval-region 
     ,(buffer-substring-no-properties start end))))

(defun slclj-eval-buffer ()
  "Evaluate the current buffer.
The value is printed in the echo area."
  (interactive)
  (slclj-eval-region (point-min) (point-max)))

(defun slclj-re-evaluate-defvar (form)
  "Force the re-evaluaton of the defvar form before point.  

First make the variable unbound, then evaluate the entire form."
  (interactive (list (slclj-last-expression)))
  (slclj-eval-with-transcript `(swank:re-evaluate-defvar ,form)))

(defun slclj-pprint-eval-last-expression ()
  "Evaluate the form before point; pprint the value in a buffer."
  (interactive)
  (slclj-eval-describe `(swank:pprint-eval ,(slclj-last-expression))))

(defun slclj-eval-print-last-expression (string)
  "Evaluate sexp before point; print value into the current buffer"
  (interactive (list (slclj-last-expression)))
  (insert "\n")
  (slclj-eval-print string))

;;;; Edit Lisp value
;;;
(defun slclj-edit-value (form-string)
  "\\<slclj-edit-value-mode-map>\
Edit the value of a setf'able form in a new buffer.
The value is inserted into a temporary buffer for editing and then set
in Lisp when committed with \\[slclj-edit-value-commit]."
  (interactive 
   (list (slclj-read-from-minibuffer "Edit value (evaluated): "
				     (slclj-sexp-at-point))))
  (slclj-eval-async `(swank:value-for-editing ,form-string)
                    (lexical-let ((form-string form-string)
                                  (package (slclj-current-package)))
                      (lambda (result)
                        (slclj-edit-value-callback form-string result 
                                                   package)))))

(make-variable-buffer-local
 (defvar slclj-edit-form-string nil
   "The form being edited by `slclj-edit-value'."))

(define-minor-mode slclj-edit-value-mode
  "Mode for editing a Lisp value."
  nil
  " Edit-Value"
  '(("\C-c\C-c" . slclj-edit-value-commit)))

(defun slclj-edit-value-callback (form-string current-value package)
  (let* ((name (generate-new-buffer-name (format "*Edit %s*" form-string)))
         (buffer (slclj-with-popup-buffer (name package t t)
                  (lisp-mode)
                  (slclj-mode 1)
                  (slclj-popup-buffer-mode -1) ; don't want binding of 'q'
                  (slclj-edit-value-mode 1)
                  (setq slclj-edit-form-string form-string)
                  (insert current-value)
                  (current-buffer))))
    (with-current-buffer buffer
      (setq buffer-read-only nil))))

(defun slclj-edit-value-commit ()
  "Commit the edited value to the Lisp image.
\\(See `slclj-edit-value'.)"
  (interactive)
  (if (null slclj-edit-form-string)
      (error "Not editing a value.")
    (let ((value (buffer-substring-no-properties (point-min) (point-max))))
      (lexical-let ((buffer (current-buffer)))
        (slclj-eval-async `(swank:commit-edited-value ,slclj-edit-form-string
                                                      ,value)
                          (lambda (_)
                            (with-current-buffer buffer
                              (slclj-popup-buffer-quit t))))))))

;;;; Tracing

(defun slclj-untrace-all ()
  "Untrace all functions."
  (interactive)
  (slclj-eval `(swank:untrace-all)))

(defun slclj-toggle-trace-fdefinition (&optional using-context-p)
  "Toggle trace."
  (interactive "P")
  (let* ((spec (if using-context-p
                  (slclj-extract-context)
                 (slclj-symbol-at-point)))
         (spec (slclj-trace-query spec)))
    (message "%s" (slclj-eval `(swank:swank-toggle-trace ,spec)))))


;; FIXME: move this to contrib

(defun slclj-trace-query (spec)
  "Ask the user which function to trace; SPEC is the default.
The result is a string."
  (cond ((null spec)
         (slclj-read-from-minibuffer "(Un)trace: "))
        ((stringp spec)
         (slclj-read-from-minibuffer "(Un)trace: " spec))
        ((symbolp spec)    ; `slclj-extract-context' can return symbols.
         (slclj-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
        (t
         (destructure-case spec
           ((setf n)
            (slclj-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
           ((:defun n)
            (slclj-read-from-minibuffer "(Un)trace: " (prin1-to-string n)))
           ((:defgeneric n)
            (let* ((name (prin1-to-string n))
                   (answer (slclj-read-from-minibuffer "(Un)trace: " name)))
              (cond ((and (string= name answer)
                          (y-or-n-p (concat "(Un)trace also all " 
                                            "methods implementing " 
                                            name "? ")))
                     (prin1-to-string `(:defgeneric ,n)))
                    (t
                     answer))))
           ((:defmethod &rest _)
            (slclj-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
           ((:call caller callee)
            (let* ((callerstr (prin1-to-string caller))
                   (calleestr (prin1-to-string callee))
                   (answer (slclj-read-from-minibuffer "(Un)trace: " 
                                                       calleestr)))
              (cond ((and (string= calleestr answer)
                          (y-or-n-p (concat "(Un)trace only when " calleestr
                                            " is called by " callerstr "? ")))
                     (prin1-to-string `(:call ,caller ,callee)))
                    (t
                     answer))))
           (((:labels :flet) &rest _)
            (slclj-read-from-minibuffer "(Un)trace local function: "
                                        (prin1-to-string spec)))
           (t (error "Don't know how to trace the spec %S" spec))))))

(defun slclj-extract-context ()
  "Parse the context for the symbol at point.  
Nil is returned if there's no symbol at point.  Otherwise we detect
the following cases (the . shows the point position):

 (defun n.ame (...) ...)                 -> (:defun name)
 (defun (setf n.ame) (...) ...)          -> (:defun (setf name))
 (defmethod n.ame (...) ...)             -> (:defmethod name (...))
 (defun ... (...) (labels ((n.ame (...)  -> (:labels (:defun ...) name)
 (defun ... (...) (flet ((n.ame (...)    -> (:flet (:defun ...) name)
 (defun ... (...) ... (n.ame ...) ...)   -> (:call (:defun ...) name)
 (defun ... (...) ... (setf (n.ame ...)  -> (:call (:defun ...) (setf name))

 (defmacro n.ame (...) ...)              -> (:defmacro name)
 (defsetf n.ame (...) ...)               -> (:defsetf name)
 (define-setf-expander n.ame (...) ...)  -> (:define-setf-expander name)
 (define-modify-macro n.ame (...) ...)   -> (:define-modify-macro name)
 (define-compiler-macro n.ame (...) ...) -> (:define-compiler-macro name)
 (defvar n.ame (...) ...)                -> (:defvar name)
 (defparameter n.ame ...)                -> (:defparameter name)
 (defconstant n.ame ...)                 -> (:defconstant name)
 (defclass n.ame ...)                    -> (:defclass name)

For other contexts we return the symbol at point."
  (let ((name (slclj-symbol-at-point)))
    (if name
        (let ((symbol (read name)))
          (or (progn ;;ignore-errors 
                (slclj-parse-context symbol))
              symbol)))))

(defun slclj-parse-context (name)
  (save-excursion 
    (cond ((slclj-in-expression-p '(defun *))          `(:defun ,name))
          ((slclj-in-expression-p '(defmacro *))       `(:defmacro ,name))
          ((slclj-in-expression-p '(defgeneric *))     `(:defgeneric ,name))
          ((slclj-in-expression-p '(setf *))
           ;;a setf-definition, but which?
           (backward-up-list 1)
           (slclj-parse-context `(setf ,name)))
          ((slclj-in-expression-p '(defmethod *))
           (unless (looking-at "\\s ")
             (forward-sexp 1)) ; skip over the methodname
           (let (qualifiers arglist)
             (loop for e = (read (current-buffer))
                   until (listp e) do (push e qualifiers)
                   finally (setq arglist e))
             `(:defmethod ,name ,@qualifiers
                          ,(slclj-arglist-specializers arglist))))
          ((and (symbolp name) 
                (slclj-in-expression-p `(,name)))
           ;; looks like a regular call
           (let ((toplevel (ignore-errors (slclj-parse-toplevel-form))))
             (cond ((slclj-in-expression-p `(setf (*)))  ;a setf-call
                    (if toplevel
                        `(:call ,toplevel (setf ,name))
                      `(setf ,name)))
                   ((not toplevel)
                    name)
                   ((slclj-in-expression-p `(labels ((*))))
                    `(:labels ,toplevel ,name))
                   ((slclj-in-expression-p `(flet ((*))))
                    `(:flet ,toplevel ,name))
                   (t
                    `(:call ,toplevel ,name)))))
          ((slclj-in-expression-p '(define-compiler-macro *))
           `(:define-compiler-macro ,name))
          ((slclj-in-expression-p '(define-modify-macro *))
           `(:define-modify-macro ,name))
          ((slclj-in-expression-p '(define-setf-expander *))
           `(:define-setf-expander ,name))
          ((slclj-in-expression-p '(defsetf *))
           `(:defsetf ,name))
          ((slclj-in-expression-p '(defvar *))       `(:defvar ,name))
          ((slclj-in-expression-p '(defparameter *)) `(:defparameter ,name))
          ((slclj-in-expression-p '(defconstant *))  `(:defconstant ,name))
          ((slclj-in-expression-p '(defclass *))     `(:defclass ,name))
          (t 
           name))))


(defun slclj-in-expression-p (pattern)
  "A helper function to determine the current context.
The pattern can have the form:
 pattern ::= ()    ;matches always
           | (*)   ;matches inside a list
           | (<symbol> <pattern>)   ;matches if the first element in
				    ; the current list is <symbol> and
                                    ; if <pattern> matches.
           | ((<pattern>))          ;matches if we are in a nested list."
  (save-excursion
    (let ((path (reverse (slclj-pattern-path pattern))))
      (loop for p in path
            always (ignore-errors 
                     (etypecase p
                       (symbol (slclj-beginning-of-list) 
                               (eq (read (current-buffer)) p))
                       (number (backward-up-list p)
                               t)))))))

(defun slclj-pattern-path (pattern)
  ;; Compute the path to the * in the pattern to make matching
  ;; easier. The path is a list of symbols and numbers.  A number
  ;; means "(down-list <n>)" and a symbol "(look-at <sym>)")
  (if (null pattern)
      '()
    (etypecase (car pattern)
      ((member *) '())
      (symbol (cons (car pattern) (slclj-pattern-path (cdr pattern))))
      (cons (cons 1 (slclj-pattern-path (car pattern)))))))

(defun slclj-beginning-of-list (&optional up)
  "Move backward to the beginning of the current expression.
Point is placed before the first expression in the list."
  (backward-up-list (or up 1))
  (down-list 1)
  (skip-syntax-forward " "))

(defun slclj-end-of-list (&optional up)
  (backward-up-list (or up 1))
  (forward-list 1)
  (down-list -1))

(defun slclj-parse-toplevel-form ()
  (ignore-errors                        ; (foo)
    (save-excursion
      (goto-char (car (slclj-region-for-defun-at-point)))
      (down-list 1)
      (forward-sexp 1)
      (slclj-parse-context (read (current-buffer))))))
		 
(defun slclj-arglist-specializers (arglist)
  (cond ((or (null arglist)
	     (member (first arglist) '(&optional &key &rest &aux)))
	 (list))
	((consp (first arglist))
	 (cons (second (first arglist))
	       (slclj-arglist-specializers (rest arglist))))
	(t
	 (cons 't 
	       (slclj-arglist-specializers (rest arglist))))))


(defun slclj-disassemble-symbol (symbol-name)
  "Display the disassembly for SYMBOL-NAME."
  (interactive (list (slclj-read-symbol-name "Disassemble: ")))
  (slclj-eval-describe `(swank:disassemble-symbol ,symbol-name)))

(defun slclj-undefine-function (symbol-name)
  "Unbind the function slot of SYMBOL-NAME."
  (interactive (list (slclj-read-symbol-name "fmakunbound: " t)))
  (slclj-eval-async `(swank:undefine-function ,symbol-name)
                    (lambda (result) (message "%s" result))))

(defun slclj-load-file (filename)
  "Load the Lisp file FILENAME."
  (interactive (list 
		(read-file-name "Load file: " nil nil
				nil (if (buffer-file-name)
                                        (file-name-nondirectory 
                                         (buffer-file-name))))))
  (let ((lisp-filename (slclj-to-lisp-filename (expand-file-name filename))))
    (slclj-eval-with-transcript `(swank:load-file ,lisp-filename))))

(defvar slclj-change-directory-hooks nil
  "Hook run by `slclj-change-directory'.
The functions are called with the new (absolute) directory.")

(defun slclj-change-directory (directory)
  "Make DIRECTORY become Lisp's current directory.
Return whatever swank:set-default-directory returns."
  (let ((dir (expand-file-name directory)))
    (prog1 (slclj-eval `(swank:set-default-directory
                         ,(slclj-to-lisp-filename dir)))
      (slclj-with-connection-buffer nil (cd-absolute dir))
      (run-hook-with-args 'slclj-change-directory-hooks dir))))
 
(defun slclj-cd (directory)
  "Make DIRECTORY become Lisp's current directory.
Return whatever swank:set-default-directory returns."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (message "default-directory: %s" (slclj-change-directory directory)))

(defun slclj-pwd ()
  "Show Lisp's default directory."
  (interactive)
  (message "Directory %s" (slclj-eval `(swank:default-directory))))


;;;; Profiling

(defun slclj-toggle-profile-fdefinition (fname-string)
  "Toggle profiling for FNAME-STRING."
  (interactive (list (slclj-read-from-minibuffer 
                      "(Un)Profile: "
                      (slclj-symbol-at-point))))
  (slclj-eval-async `(swank:toggle-profile-fdefinition ,fname-string)
                    (lambda (r) (message "%s" r))))

(defun slclj-unprofile-all ()
  "Unprofile all functions."
  (interactive)
  (slclj-eval-async '(swank:unprofile-all)
                    (lambda (r) (message "%s" r))))

(defun slclj-profile-report ()
  "Print profile report."
  (interactive)
  (slclj-eval-with-transcript '(swank:profile-report)))

(defun slclj-profile-reset ()
  "Reset profile counters."
  (interactive)
  (slclj-eval-async (slclj-eval `(swank:profile-reset))
                    (lambda (r) (message "%s" r))))

(defun slclj-profiled-functions ()
  "Return list of names of currently profiled functions."
  (interactive)
  (slclj-eval-async `(swank:profiled-functions)
                    (lambda (r) (message "%s" r))))

(defun slclj-profile-package (package callers methods)
  "Profile all functions in PACKAGE.  
If CALLER is non-nil names have counts of the most common calling
functions recorded. 
If METHODS is non-nil, profile all methods of all generic function
having names in the given package."
  (interactive (list (slclj-read-package-name "Package: ")
                     (y-or-n-p "Record the most common callers? ")
                     (y-or-n-p "Profile methods? ")))
  (slclj-eval-async `(swank:profile-package ,package ,callers ,methods)
                    (lambda (r) (message "%s" r))))

(defun slclj-profile-by-substring (substring &optional package)
  "Profile all functions which names contain SUBSTRING.
If PACKAGE is NIL, then search in all packages."
  (interactive (list
                (slclj-read-from-minibuffer 
                 "Profile by matching substring: "
                 (slclj-symbol-at-point))
                (slclj-read-package-name "Package (RET for all packages): ")))
  (let ((package (unless (equal package "") package)))
    (slclj-eval-async `(swank:profile-by-substring ,substring ,package)
                      (lambda (r) (message "%s" r)) )))

;;;; Documentation

(defvar slclj-documentation-lookup-function 
  'slclj-hyperspec-lookup)

(defun slclj-documentation-lookup ()
  "Generalized documentation lookup. Defaults to hyperspec lookup."
  (interactive)
  (call-interactively slclj-documentation-lookup-function))

(defun slclj-hyperspec-lookup (symbol-name)
  "A wrapper for `hyperspec-lookup'"
  (interactive (list (let* ((symbol-at-point (slclj-symbol-at-point))
                            (stripped-symbol 
                             (and symbol-at-point
                                  (downcase
                                   (common-lisp-hyperspec-strip-cl-package 
                                    symbol-at-point)))))
                       (if (and stripped-symbol
                                (intern-soft stripped-symbol
                                             common-lisp-hyperspec-symbols))
                           stripped-symbol
                         (completing-read
                          "Look up symbol in Common Lisp HyperSpec: "
                          common-lisp-hyperspec-symbols #'boundp
                          t stripped-symbol
                          'common-lisp-hyperspec-history)))))
  (hyperspec-lookup symbol-name))
  
(defun slclj-describe-symbol (symbol-name)
  "Describe the symbol at point."
  (interactive (list (slclj-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slclj-eval-describe `(swank:describe-symbol ,symbol-name)))

(defun slclj-documentation (symbol-name)
  "Display function- or symbol-documentation for SYMBOL-NAME."
  (interactive (list (slclj-read-symbol-name "Documentation for symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slclj-eval-describe 
   `(swank:documentation-symbol ,symbol-name)))

(defun slclj-describe-function (symbol-name)
  (interactive (list (slclj-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slclj-eval-describe `(swank:describe-function ,symbol-name)))

(defun slclj-apropos-summary (string case-sensitive-p package only-external-p)
  "Return a short description for the performed apropos search."
  (concat (if case-sensitive-p "Case-sensitive " "")
          "Apropos for "
          (format "%S" string)
          (if package (format " in package %S" package) "")
          (if only-external-p " (external symbols only)" "")))

(defun slclj-apropos (string &optional only-external-p package 
                             case-sensitive-p)
  "Show all bound symbols whose names match STRING. With prefix
arg, you're interactively asked for parameters of the search."
  (interactive
   (if current-prefix-arg
       (list (read-string "SLCLJ Apropos: ")
             (y-or-n-p "External symbols only? ")
             (let ((pkg (slclj-read-package-name "Package: ")))
               (if (string= pkg "") nil pkg))
             (y-or-n-p "Case-sensitive? "))
     (list (read-string "SLCLJ Apropos: ") t nil nil)))
  (let ((buffer-package (or package (slclj-current-package))))
    (slclj-eval-async
     `(swank:apropos-list-for-emacs ,string ,only-external-p
                                    ,case-sensitive-p ',package)
     (slclj-rcurry #'slclj-show-apropos string buffer-package
                   (slclj-apropos-summary string case-sensitive-p
                                          package only-external-p)))))

(defun slclj-apropos-all ()
  "Shortcut for (slclj-apropos <string> nil nil)"
  (interactive)
  (slclj-apropos (read-string "SLCLJ Apropos: ") nil nil))

(defun slclj-apropos-package (package &optional internal)
  "Show apropos listing for symbols in PACKAGE.
With prefix argument include internal symbols."
  (interactive (list (let ((pkg (slclj-read-package-name "Package: ")))
                       (if (string= pkg "") (slclj-current-package) pkg))
                     current-prefix-arg))
  (slclj-apropos "" (not internal) package))

(defun slclj-show-apropos (plists string package summary)
  (if (null plists)
      (message "No apropos matches for %S" string)
    (slclj-with-popup-buffer ("*SLCLJ Apropos*" package t)
      (apropos-mode)
      (if (boundp 'header-line-format)
          (setq header-line-format summary)
        (insert summary "\n\n"))
      (slclj-set-truncate-lines)
      (slclj-print-apropos plists)
      (set-syntax-table lisp-mode-syntax-table)
      (goto-char (point-min)))))

(defvar slclj-apropos-label-properties
  (progn
    (require 'apropos)
    (cond ((and (boundp 'apropos-label-properties) 
                (symbol-value 'apropos-label-properties)))
          ((boundp 'apropos-label-face)
           (etypecase (symbol-value 'apropos-label-face)
             (symbol `(face ,(or (symbol-value 'apropos-label-face)
                                 'italic)
                            mouse-face highlight))
             (list (symbol-value 'apropos-label-face)))))))

(defun slclj-print-apropos (plists)
  (dolist (plist plists)
    (let ((designator (plist-get plist :designator)))
      (assert designator)
      (slclj-insert-propertized `(face ,apropos-symbol-face) designator))
    (terpri)
    (let ((apropos-label-properties slclj-apropos-label-properties))
      (loop for (prop namespace) 
	    in '((:variable "Variable")
		 (:function "Function")
		 (:generic-function "Generic Function")
                 (:macro "Macro")
                 (:special-operator "Special Operator")
		 (:setf "Setf")
		 (:type "Type")
		 (:class "Class")
                 (:alien-type "Alien type")
                 (:alien-struct "Alien struct")
                 (:alien-union "Alien type")
                 (:alien-enum "Alien enum"))
            ;; Properties not listed here will not show up in the buffer
	    do
	    (let ((value (plist-get plist prop))
		  (start (point)))
	      (when value
		(princ "  ") 
		(slclj-insert-propertized apropos-label-properties namespace)
		(princ ": ")
		(princ (etypecase value
			 (string value)
			 ((member :not-documented) "(not documented)")))
                (add-text-properties 
                 start (point)
                 (list 'type prop 'action 'slclj-call-describer
                       'button t 'apropos-label namespace 
                       'item (plist-get plist :designator)))
		(terpri)))))))

(defun slclj-call-describer (arg)
  (let* ((pos (if (markerp arg) arg (point)))
         (type (get-text-property pos 'type))
         (item (get-text-property pos 'item)))
    (slclj-eval-describe `(swank:describe-definition-for-emacs ,item ,type))))


;;;; XREF: cross-referencing

(defvar slclj-xref-mode-map)

(define-derived-mode slclj-xref-mode lisp-mode "Xref"
  "slclj-xref-mode: Major mode for cross-referencing.
\\<slclj-xref-mode-map>\
The most important commands:
\\[slclj-xref-quit]	- Dismiss buffer.
\\[slclj-show-xref]	- Display referenced source and keep xref window.
\\[slclj-goto-xref]	- Jump to referenced source and dismiss xref window.

\\{slclj-xref-mode-map}
\\{slclj-popup-buffer-mode-map}
"
  (setq font-lock-defaults nil)
  (setq delayed-mode-hooks nil)
  (slclj-mode -1))

(slclj-define-keys slclj-xref-mode-map
  ((kbd "RET") 'slclj-goto-xref)
  ((kbd "SPC") 'slclj-goto-xref)
  ("v" 'slclj-show-xref)
  ("n" (lambda () (interactive) (next-line)))
  ("p" (lambda () (interactive) (previous-line)))
  ("\C-c\C-c" 'slclj-recompile-xref)
  ("\C-c\C-k" 'slclj-recompile-all-xrefs)
  ("\M-," 'slclj-xref-retract)
  ([remap next-line] 'slclj-xref-next-line)
  ([remap previous-line] 'slclj-xref-prev-line)
  ;; for XEmacs:
  ([down] 'slclj-xref-next-line)
  ([up] 'slclj-xref-prev-line))

(defun slclj-next-line/not-add-newlines ()
  (interactive)
  (let ((next-line-add-newlines nil))
    (next-line 1)))


;;;;; XREF results buffer and window management

(defmacro* slclj-with-xref-buffer ((xref-type symbol &optional package)
                                   &body body)
  "Execute BODY in a xref buffer, then show that buffer."
  `(let ((xref-buffer-name% (format "*slclj xref[%s: %s]*" 
                                    ,xref-type ,symbol)))
     (slclj-with-popup-buffer (xref-buffer-name% ,package t t)
       (slclj-xref-mode)
       (slclj-set-truncate-lines)
       (erase-buffer)
       ,@body)))

(put 'slclj-with-xref-buffer 'lisp-indent-function 1)

(defun slclj-insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current-buffer.
XREF-ALIST is of the form ((GROUP . ((LABEL LOCATION) ...)) ...).
GROUP and LABEL are for decoration purposes.  LOCATION is a
source-location."
  (loop for (group . refs) in xref-alist do 
        (slclj-insert-propertized '(face bold) group "\n")
        (loop for (label location) in refs do
              (slclj-insert-propertized 
               (list 'slclj-location location 'face 'font-lock-keyword-face)
               "  " (slclj-one-line-ify label) "\n")))
  ;; Remove the final newline to prevent accidental window-scrolling
  (backward-delete-char 1))

(defun slclj-xref-next-line ()
  (interactive)
  (slclj-xref-show-location (slclj-search-property 'slclj-location)))

(defun slclj-xref-prev-line ()
  (interactive)
  (slclj-xref-show-location (slclj-search-property 'slclj-location t)))

(defun slclj-xref-show-location (loc)
  (ecase (car loc)
    (:location (slclj-show-source-location loc t))
    (:error (message "%s" (cadr loc)))
    ((nil))))

(defun slclj-show-xref-buffer (xrefs type symbol package)
  (slclj-with-xref-buffer (type symbol package)
    (slclj-insert-xrefs xrefs)
    (setq slclj-next-location-function 'slclj-goto-next-xref)
    (setq slclj-previous-location-function 'slclj-goto-previous-xref)
    (setq slclj-xref-last-buffer (current-buffer))
    (goto-char (point-min))))

(defvar slclj-next-location-function nil
  "Function to call for going to the next location.")

(defvar slclj-previous-location-function nil
  "Function to call for going to the previous location.")

(defvar slclj-xref-last-buffer nil
  "The most recent XREF results buffer.
This is used by `slclj-goto-next-xref'")

(defun slclj-show-xrefs (xrefs type symbol package)
  "Show the results of an XREF query."
  (if (null xrefs)
      (message "No references found for %s." symbol)
      (slclj-show-xref-buffer xrefs type symbol package)))


;;;;; XREF commands

(defun slclj-who-calls (symbol)
  "Show all known callers of the function SYMBOL."
  (interactive (list (slclj-read-symbol-name "Who calls: " t)))
  (slclj-xref :calls symbol))

(defun slclj-calls-who (symbol)
  "Show all known functions called by the function SYMBOL."
  (interactive (list (slclj-read-symbol-name "Who calls: " t)))
  (slclj-xref :calls-who symbol))

(defun slclj-who-references (symbol)
  "Show all known referrers of the global variable SYMBOL."
  (interactive (list (slclj-read-symbol-name "Who references: " t)))
  (slclj-xref :references symbol))

(defun slclj-who-binds (symbol)
  "Show all known binders of the global variable SYMBOL."
  (interactive (list (slclj-read-symbol-name "Who binds: " t)))
  (slclj-xref :binds symbol))

(defun slclj-who-sets (symbol)
  "Show all known setters of the global variable SYMBOL."
  (interactive (list (slclj-read-symbol-name "Who sets: " t)))
  (slclj-xref :sets symbol))

(defun slclj-who-macroexpands (symbol)
  "Show all known expanders of the macro SYMBOL."
  (interactive (list (slclj-read-symbol-name "Who macroexpands: " t)))
  (slclj-xref :macroexpands symbol))

(defun slclj-who-specializes (symbol)
  "Show all known methods specialized on class SYMBOL."
  (interactive (list (slclj-read-symbol-name "Who specializes: " t)))
  (slclj-xref :specializes symbol))

(defun slclj-list-callers (symbol-name)
  "List the callers of SYMBOL-NAME in a xref window."
  (interactive (list (slclj-read-symbol-name "List callers: ")))
  (slclj-xref :callers symbol-name))

(defun slclj-list-callees (symbol-name)
  "List the callees of SYMBOL-NAME in a xref window."
  (interactive (list (slclj-read-symbol-name "List callees: ")))
  (slclj-xref :callees symbol-name))

(defun slclj-xref (type symbol &optional continuation)
  "Make an XREF request to Lisp."
  (slclj-eval-async
   `(swank:xref ',type ',symbol)
   (slclj-rcurry (lambda (result type symbol package cont)
                   (slclj-check-xref-implemented type result)
                   (let* ((xrefs (slclj-postprocess-xrefs result))
                          (file-alist (cadr (slclj-analyze-xrefs result))))
                     (funcall (or cont 'slclj-show-xrefs)
                              file-alist type symbol package)))
                 type 
                 symbol 
                 (slclj-current-package)
                 continuation)))

(defun slclj-check-xref-implemented (type xrefs)
  (when (eq xrefs :not-implemented)
    (error "%s is not implemented yet on %s." 
           (slclj-xref-type type)
           (slclj-lisp-implementation-name))))

(defun slclj-xref-type (type)
  (format "who-%s" (slclj-cl-symbol-name type)))

(defun slclj-xrefs (types symbol &optional continuation)
  "Make multiple XREF requests at once."
  (slclj-eval-async
   `(swank:xrefs ',types ',symbol)
   (slclj-rcurry (lambda (result types symbol package cont)
                   (funcall (or cont 'slclj-show-xrefs)
                            (slclj-map-alist #'slclj-xref-type 
                                             #'identity 
                                             result)
                            types symbol package))
                 types 
                 symbol 
                 (slclj-current-package)
                 continuation)))


;;;;; XREF navigation

(defun slclj-xref-location-at-point ()
  (save-excursion
    ;; When the end of the last line is at (point-max) we can't find
    ;; the text property there. Going to bol avoids this problem.
    (beginning-of-line 1)
    (or (get-text-property (point) 'slclj-location)
        (error "No reference at point."))))

(defun slclj-xref-dspec-at-point ()
  (save-excursion
    (beginning-of-line 1)
    (with-syntax-table lisp-mode-syntax-table
      (forward-sexp)                    ; skip initial whitespaces
      (backward-sexp)
      (slclj-sexp-at-point))))

(defun slclj-all-xrefs ()
  (let ((xrefs nil))
    (save-excursion
      (goto-char (point-min))
      (while (ignore-errors (slclj-next-line/not-add-newlines) t)
        (when-let (loc (get-text-property (point) 'slclj-location))
          (let* ((dspec (slclj-xref-dspec-at-point))
                 (xref  (make-slclj-xref :dspec dspec :location loc)))
            (push xref xrefs)))))
    (nreverse xrefs)))

(defun slclj-goto-xref ()
  "Goto the cross-referenced location at point."
  (interactive)
  (slclj-show-xref)
  (slclj-popup-buffer-quit))

(defun slclj-show-xref ()
  "Display the xref at point in the other window."
  (interactive)
  (let ((location (slclj-xref-location-at-point)))
    (slclj-show-source-location location)))

(defun slclj-goto-next-xref (&optional backward)
  "Goto the next cross-reference location."
  (if (not (buffer-live-p slclj-xref-last-buffer))
      (error "No XREF buffer alive.")
    (multiple-value-bind (location pos)
        (with-current-buffer slclj-xref-last-buffer
          (values (slclj-search-property 'slclj-location backward)
                  (point)))
      (cond ((slclj-location-p location)
             (slclj-pop-to-location location)
             ;; We do this here because changing the location can take
             ;; a while when Emacs needs to read a file from disk.
             (with-current-buffer slclj-xref-last-buffer
               (slclj-show-buffer-position pos)
               (slclj-highlight-line 0.35)))
            ((null location)
             (message (if backward "No previous xref" "No next xref.")))
            (t ; error location
             (slclj-goto-next-xref backward))))))

(defun slclj-goto-previous-xref ()
  "Goto the previous cross-reference location."
  (slclj-goto-next-xref t))

(defun slclj-search-property (prop &optional backward prop-value-fn)
  "Search the next text range where PROP is non-nil.
Return the value of PROP.
If BACKWARD is non-nil, search backward.
If PROP-VALUE-FN is non-nil use it to extract PROP's value."
  (let ((next-candidate (if backward 
                            #'previous-single-char-property-change
                            #'next-single-char-property-change))
        (prop-value-fn  (or prop-value-fn
                            (lambda ()
                              (get-text-property (point) prop))))
        (start (point))
        (prop-value))
    (while (progn 
             (goto-char (funcall next-candidate (point) prop))
             (not (or (setq prop-value (funcall prop-value-fn)) 
                      (eobp) 
                      (bobp)))))
    (cond (prop-value)
          (t (goto-char start) nil))))

(defun slclj-next-location ()
  "Go to the next location, depending on context.
When displaying XREF information, this goes to the next reference."
  (interactive)
  (when (null slclj-next-location-function)
    (error "No context for finding locations."))
  (funcall slclj-next-location-function))

(defun slclj-previous-location ()
  "Go to the previous location, depending on context.
When displaying XREF information, this goes to the previous reference."
  (interactive)
  (when (null slclj-previous-location-function)
    (error "No context for finding locations."))
  (funcall slclj-previous-location-function))

(defun slclj-recompile-xref (&optional raw-prefix-arg)
  (interactive "P")
  (let ((slclj-compilation-policy (slclj-compute-policy raw-prefix-arg)))
    (let ((location (slclj-xref-location-at-point))
          (dspec    (slclj-xref-dspec-at-point)))
      (slclj-recompile-locations 
       (list location)
       (slclj-rcurry #'slclj-xref-recompilation-cont
                     (list dspec) (current-buffer))))))

(defun slclj-recompile-all-xrefs (&optional raw-prefix-arg)
  (interactive "P")
  (let ((slclj-compilation-policy (slclj-compute-policy raw-prefix-arg)))
    (let ((dspecs) (locations))
      (dolist (xref (slclj-all-xrefs))
        (when (slclj-xref-has-location-p xref)
          (push (slclj-xref.dspec xref) dspecs)
          (push (slclj-xref.location xref) locations)))
      (slclj-recompile-locations 
       locations
       (slclj-rcurry #'slclj-xref-recompilation-cont
                     dspecs (current-buffer))))))

(defun slclj-xref-recompilation-cont (results dspecs buffer)
  ;; Extreme long-windedness to insert status of recompilation;
  ;; sometimes Elisp resembles more of an Ewwlisp.

  ;; FIXME: Should probably throw out the whole recompilation cruft
  ;; anyway.  -- helmut
  (with-current-buffer buffer
    (slclj-compilation-finished (slclj-aggregate-compilation-results results))
    (save-excursion
      (slclj-xref-insert-recompilation-flags 
       dspecs (loop for r in results collect
                    (or (slclj-compilation-result.successp r)
                        (and (slclj-compilation-result.notes r)
                             :complained)))))))

(defun slclj-aggregate-compilation-results (results)
  `(:compilation-result
    ,(reduce #'append (mapcar #'slclj-compilation-result.notes results))
    ,(every #'slclj-compilation-result.successp results)
    ,(reduce #'+ (mapcar #'slclj-compilation-result.duration results))))

(defun slclj-xref-insert-recompilation-flags (dspecs compilation-results)
  (let* ((buffer-read-only nil)
         (max-column (slclj-column-max)))
    (goto-char (point-min))
    (loop for dspec in dspecs
          for result in compilation-results
          do (save-excursion
               (loop for dspec-at-point = (progn (search-forward dspec)
                                                 (slclj-xref-dspec-at-point))
                     until (equal dspec-at-point dspec))
               (end-of-line) ; skip old status information.
               (dotimes (i (- max-column (current-column)))
                 (insert " "))
               (insert " ")
               (insert (format "[%s]"
                               (case result
                                 ((t)   :success)
                                 ((nil) :failure)
                                 (t     result))))))))


;;;; Macroexpansion

(define-minor-mode slclj-macroexpansion-minor-mode
    "SLCLJ mode for macroexpansion"
    nil
  " Macroexpand"
  '(("g" . slclj-macroexpand-again)))

(flet ((remap (from to)
         (dolist (mapping (where-is-internal from slclj-mode-map))
           (define-key slclj-macroexpansion-minor-mode-map mapping to))))
  (remap 'slclj-macroexpand-1 'slclj-macroexpand-1-inplace)
  (remap 'slclj-macroexpand-all 'slclj-macroexpand-all-inplace)
  (remap 'slclj-compiler-macroexpand-1 'slclj-compiler-macroexpand-1-inplace)
  (remap 'slclj-compiler-macroexpand 'slclj-compiler-macroexpand-inplace)
  (remap 'advertised-undo 'slclj-macroexpand-undo)
  (remap 'undo 'slclj-macroexpand-undo))

(defun slclj-macroexpand-undo (&optional arg)
  (interactive)
  (flet ((undo-only (arg)
           ;; Emacs 22.x introduced `undo-only' which works by binding
           ;; `undo-no-redo' to t. We do it this way so we don't break
           ;; prior Emacs versions.
           (let ((undo-no-redo t)) (undo arg))))
    (let ((inhibit-read-only t))
      (when (fboundp 'slclj-remove-edits)
        (slclj-remove-edits (point-min) (point-max)))
      (undo-only arg))))

(defun slclj-sexp-at-point-for-macroexpansion ()
  "`slclj-sexp-at-point' with special cases for LOOP."
  (let ((string (slclj-sexp-at-point-or-error))
        (bounds (bounds-of-thing-at-point 'sexp))
        (char-at-point (substring-no-properties (thing-at-point 'char))))
    ;; SLCLJ-SEXP-AT-POINT(-OR-ERROR) uses (THING-AT-POINT 'SEXP)
    ;; which is quite a bit botched: it returns "'(FOO BAR BAZ)" even
    ;; when point is placed _at the opening parenthesis_, and hence
    ;; "(FOO BAR BAZ)" wouldn't get expanded. Likewise for ",(...)",
    ;; ",@(...)" (would return "@(...)"!!), and "\"(...)".
    ;; So we better fix this up here:
    (when (string= char-at-point "(")
      (let ((char0 (elt string 0)))
        (when (member char0 '(?\' ?\, ?\" ?\@))
          (setf string (substring string 1))
          (incf (car bounds)))))
    (list string (cons (set-marker (make-marker) (car bounds))
                       (set-marker (make-marker) (cdr bounds))))))

(defvar slclj-eval-macroexpand-expression nil
  "Specifies the last macroexpansion preformed. 
This variable specifies both what was expanded and how.")

(defun slclj-eval-macroexpand (expander &optional string)
  (let ((string (or string
                    (car (slclj-sexp-at-point-for-macroexpansion)))))
    (setq slclj-eval-macroexpand-expression `(,expander ,string))
    (slclj-eval-async slclj-eval-macroexpand-expression
                      #'slclj-initialize-macroexpansion-buffer)))

(defun slclj-macroexpand-again ()
  "Reperform the last macroexpansion."
  (interactive)
  (slclj-eval-async slclj-eval-macroexpand-expression 
                    (slclj-rcurry #'slclj-initialize-macroexpansion-buffer
                                  (current-buffer))))

(defun slclj-initialize-macroexpansion-buffer (expansion &optional buffer)
  (pop-to-buffer (or buffer (slclj-create-macroexpansion-buffer)))
  (setq buffer-undo-list nil) ; Get rid of undo information from
                              ; previous expansions.
  (let ((inhibit-read-only t)
        (buffer-undo-list t)) ; Make the initial insertion not be undoable.
    (erase-buffer)
    (insert expansion)
    (goto-char (point-min))
    (indent-sexp)
    (font-lock-fontify-buffer)))

(defun slclj-create-macroexpansion-buffer ()
  (let ((name "*SLCLJ Macroexpansion*"))
    (slclj-with-popup-buffer (name t t)
      (lisp-mode)
      (slclj-mode 1)
      (slclj-macroexpansion-minor-mode 1)
      (setq font-lock-keywords-case-fold-search t)
      (current-buffer))))

(defun slclj-eval-macroexpand-inplace (expander)
  "Substitute the sexp at point with its macroexpansion.

NB: Does not affect slclj-eval-macroexpand-expression"
  (interactive)
  (destructuring-bind (string bounds)
      (slclj-sexp-at-point-for-macroexpansion)
    (lexical-let* ((start (car bounds))
                   (end (cdr bounds))
                   (point (point))
                   (package (slclj-current-package))
                   (buffer (current-buffer)))
      (slclj-eval-async 
       `(,expander ,string)
       (lambda (expansion)
         (with-current-buffer buffer
           (let ((buffer-read-only nil))
             (when (fboundp 'slclj-remove-edits)
               (slclj-remove-edits (point-min) (point-max)))
             (goto-char start)
             (delete-region start end)
             (insert expansion)
             (goto-char start)
             (indent-sexp)
             (goto-char point))))))))

(defun slclj-macroexpand-1 (&optional repeatedly)
  "Display the macro expansion of the form at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND."
  (interactive "P")
  (slclj-eval-macroexpand
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))

(defun slclj-macroexpand-1-inplace (&optional repeatedly)
  (interactive "P")
  (slclj-eval-macroexpand-inplace
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))

(defun slclj-macroexpand-all ()
  "Display the recursively macro expanded sexp at point."
  (interactive)
  (slclj-eval-macroexpand 'swank:swank-macroexpand-all))

(defun slclj-macroexpand-all-inplace ()
  "Display the recursively macro expanded sexp at point."
  (interactive)
  (slclj-eval-macroexpand-inplace 'swank:swank-macroexpand-all))

(defun slclj-compiler-macroexpand ()
  "Display the compiler-macro expansion of sexp at point."
  (interactive)
  (slclj-eval-macroexpand 'swank:swank-compiler-macroexpand))

(defun slclj-compiler-macroexpand-inplace ()
  "Display the compiler-macro expansion of sexp at point."
  (interactive)
  (slclj-eval-macroexpand-inplace 'swank:swank-compiler-macroexpand))

(defun slclj-compiler-macroexpand-1 ()
  "Display the compiler-macro expansion of sexp at point."
  (interactive)
  (slclj-eval-macroexpand 'swank:swank-compiler-macroexpand-1))

(defun slclj-compiler-macroexpand-1-inplace ()
  "Display the compiler-macro expansion of sexp at point."
  (interactive)
  (slclj-eval-macroexpand-inplace 'swank:swank-compiler-macroexpand-1))

(defun slclj-format-string-expand ()
  "Expand the format-string at point and display it."
  (interactive)
  (slclj-eval-macroexpand 'swank:swank-format-string-expand
                          (slclj-string-at-point-or-error)))


;;;; Subprocess control

(defun slclj-interrupt ()
  "Interrupt Lisp."
  (interactive)
  (cond ((slclj-use-sigint-for-interrupt) (slclj-send-sigint))
        (t (slclj-dispatch-event `(:emacs-interrupt ,slclj-current-thread)))))

(defun slclj-quit ()
  (error "Not implemented properly.  Use `slclj-interrupt' instead."))

(defun slclj-quit-lisp (&optional kill)
  "Quit lisp, kill the inferior process and associated buffers."
  (interactive "P")
  (slclj-quit-lisp-internal (slclj-connection) 'slclj-quit-sentinel kill))

(defun slclj-quit-lisp-internal (connection sentinel kill)
  (let ((slclj-dispatching-connection connection))
    (slclj-eval-async '(swank:quit-lisp))
    (let* ((process (slclj-inferior-process connection)))
      (set-process-filter connection  nil)
      (set-process-sentinel connection sentinel)
      (when (and kill process)
        (sleep-for 0.2)
        (unless (memq (process-status process) '(exit signal))
          (kill-process process))))))

(defun slclj-quit-sentinel (process message)
  (assert (process-status process) 'closed)
  (let* ((inferior (slclj-inferior-process process))
         (inferior-buffer (if inferior (process-buffer inferior))))
    (when inferior (delete-process inferior))
    (when inferior-buffer (kill-buffer inferior-buffer))
    (slclj-net-close process)
    (message "Connection closed.")))
	

;;;; Debugger (SLDB)

(defvar sldb-hook nil
  "Hook run on entry to the debugger.")

(defcustom sldb-initial-restart-limit 6
  "Maximum number of restarts to display initially."
  :group 'slclj-debugger
  :type 'integer)


;;;;; Local variables in the debugger buffer

;; Small helper.
(defun slclj-make-variables-buffer-local (&rest variables)
  (mapcar #'make-variable-buffer-local variables))

(slclj-make-variables-buffer-local
 (defvar sldb-condition nil
   "A list (DESCRIPTION TYPE) describing the condition being debugged.")

 (defvar sldb-restarts nil
   "List of (NAME DESCRIPTION) for each available restart.")

 (defvar sldb-level nil
   "Current debug level (recursion depth) displayed in buffer.")

 (defvar sldb-backtrace-start-marker nil
   "Marker placed at the first frame of the backtrace.")

 (defvar sldb-restart-list-start-marker nil
  "Marker placed at the first restart in the restart list.")

 (defvar sldb-continuations nil
   "List of ids for pending continuation."))

;;;;; SLDB macros

;; some macros that we need to define before the first use

;; FIXME: rename
(defmacro in-sldb-face (name string)
  "Return STRING propertised with face sldb-NAME-face."
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name))))
	(var (gensym "string")))
    `(let ((,var ,string))
      (slclj-add-face ',facename ,var)
      ,var)))

(put 'in-sldb-face 'lisp-indent-function 1)


;;;;; sldb-mode

(defvar sldb-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; We give < and > parenthesis syntax, so that #< ... > is treated
    ;; as a balanced expression.  This enables autodoc-mode to match
    ;; #<unreadable> actual arguments in the backtraces with formal
    ;; arguments of the function.  (For Lisp mode, this is not
    ;; desirable, since we do not wish to get a mismatched paren
    ;; highlighted everytime we type < or >.)
    (modify-syntax-entry ?< "(" table)
    (modify-syntax-entry ?> ")" table)
    table)
  "Syntax table for SLDB mode.")

(define-derived-mode sldb-mode fundamental-mode "sldb"
  "Superior lisp debugger mode.
In addition to ordinary SLCLJ commands, the following are
available:\\<sldb-mode-map>

Commands to examine the selected frame:
   \\[sldb-toggle-details]   - toggle details (local bindings, CATCH tags)
   \\[sldb-show-source]   - view source for the frame
   \\[sldb-eval-in-frame]   - eval in frame
   \\[sldb-pprint-eval-in-frame]   - eval in frame, pretty-print result
   \\[sldb-disassemble]   - disassemble
   \\[sldb-inspect-in-frame]   - inspect

Commands to invoke restarts:
   \\[sldb-quit]   - quit
   \\[sldb-abort]   - abort
   \\[sldb-continue]   - continue
   \\[sldb-invoke-restart-0]-\\[sldb-invoke-restart-9] - restart shortcuts

Commands to navigate frames:
   \\[sldb-down]   - down
   \\[sldb-up]   - up
   \\[sldb-details-down] - down, with details
   \\[sldb-details-up] - up, with details

Miscellaneous commands:
   \\[sldb-restart-frame]   - restart frame
   \\[sldb-return-from-frame]   - return from frame
   \\[sldb-step]   - step
   \\[sldb-break-with-default-debugger]   - switch to default debugger
   \\[slclj-interactive-eval]   - eval

Full list of commands:

\\{sldb-mode-map}"
  (erase-buffer)
  (set-syntax-table sldb-mode-syntax-table)
  (slclj-set-truncate-lines)
  ;; Make original slclj-connection "sticky" for SLDB commands in this buffer
  (setq slclj-buffer-connection (slclj-connection)))

(set-keymap-parent sldb-mode-map slclj-parent-map)

(slclj-define-keys sldb-mode-map

  ((kbd "RET") 'sldb-default-action)
  ("\C-m"      'sldb-default-action)
  ([return] 'sldb-default-action)
  ([mouse-2]  'sldb-default-action/mouse)
  ([follow-link] 'mouse-face)
  ("\C-i" 'sldb-cycle)
  ("h"    'describe-mode)
  ("v"    'sldb-show-source)
  ("e"    'sldb-eval-in-frame)
  ("d"    'sldb-pprint-eval-in-frame)
  ("D"    'sldb-disassemble)
  ("i"    'sldb-inspect-in-frame)
  ("n"    'sldb-down)
  ("p"    'sldb-up)
  ("\M-n" 'sldb-details-down)
  ("\M-p" 'sldb-details-up)
  ("<"    'sldb-beginning-of-backtrace)
  (">"    'sldb-end-of-backtrace)
  ("t"    'sldb-toggle-details)
  ("r"    'sldb-restart-frame)
  ("I"    'sldb-invoke-restart-by-name)
  ("R"    'sldb-return-from-frame)
  ("c"    'sldb-continue)
  ("s"    'sldb-step)
  ("x"    'sldb-next)
  ("o"    'sldb-out)
  ("b"    'sldb-break-on-return)
  ("a"    'sldb-abort)
  ("q"    'sldb-quit)
  ("A"    'sldb-break-with-system-debugger)
  ("B"    'sldb-break-with-default-debugger)
  ("P"    'sldb-print-condition)
  ("C"    'sldb-inspect-condition)
  (":"    'slclj-interactive-eval)
  ("\C-c\C-c" 'sldb-recompile-frame-source))

;; Keys 0-9 are shortcuts to invoke particular restarts.
(dotimes (number 10)
  (let ((fname (intern (format "sldb-invoke-restart-%S" number)))
        (docstring (format "Invoke restart numbered %S." number)))
    (eval `(defun ,fname ()
             ,docstring
             (interactive)
             (sldb-invoke-restart ,number)))
    (define-key sldb-mode-map (number-to-string number) fname)))


;;;;; SLDB buffer creation & update

(defun sldb-buffers (&optional connection)
  "Return a list of all sldb buffers (belonging to CONNECTION.)"
  (if connection
      (slclj-filter-buffers (lambda ()
                              (and (eq slclj-buffer-connection connection)
                                   (eq major-mode 'sldb-mode))))
      (slclj-filter-buffers (lambda () (eq major-mode 'sldb-mode)))))

(defun sldb-find-buffer (thread &optional connection)
  (let ((connection (or connection (slclj-connection))))
    (find-if (lambda (buffer)
               (with-current-buffer buffer
                 (and (eq slclj-buffer-connection connection)
                      (eq slclj-current-thread thread))))
             (sldb-buffers))))

(defun sldb-get-default-buffer ()
  "Get a sldb buffer.
The buffer is chosen more or less randomly."
  (car (sldb-buffers)))

(defun sldb-get-buffer (thread &optional connection)
  "Find or create a sldb-buffer for THREAD."
  (let ((connection (or connection (slclj-connection))))
    (or (sldb-find-buffer thread connection)
        (let ((name (format "*sldb %s/%s*" (slclj-connection-name) thread)))
          (with-current-buffer (generate-new-buffer name)
            (setq slclj-buffer-connection connection
                  slclj-current-thread thread)
            (current-buffer))))))

(defun sldb-debugged-continuations (connection)
  "Return the debugged continuations for CONNECTION."
  (lexical-let ((accu '()))
    (dolist (b (sldb-buffers))
      (with-current-buffer b
        (when (eq slclj-buffer-connection connection)
          (setq accu (append sldb-continuations accu)))))
    accu))

(defun sldb-setup (thread level condition restarts frames conts)
  "Setup a new SLDB buffer.
CONDITION is a string describing the condition to debug.
RESTARTS is a list of strings (NAME DESCRIPTION) for each available restart.
FRAMES is a list (NUMBER DESCRIPTION &optional PLIST) describing the initial
portion of the backtrace. Frames are numbered from 0.
CONTS is a list of pending Emacs continuations."
  (with-current-buffer (sldb-get-buffer thread)
    (unless (equal sldb-level level)
      (setq buffer-read-only nil)
      (slclj-save-local-variables (slclj-popup-restore-data)
        (sldb-mode))
      (setq slclj-current-thread thread)
      (setq sldb-level level)
      (setq mode-name (format "sldb[%d]" sldb-level))
      (setq sldb-condition condition)
      (setq sldb-restarts restarts)
      (setq sldb-continuations conts)
      (sldb-insert-condition condition)
      (insert "\n\n" (in-sldb-face section "Restarts:") "\n")
      (setq sldb-restart-list-start-marker (point-marker))
      (sldb-insert-restarts restarts 0 sldb-initial-restart-limit)
      (insert "\n" (in-sldb-face section "Backtrace:") "\n")
      (setq sldb-backtrace-start-marker (point-marker))
      (save-excursion
        (if frames 
            (sldb-insert-frames (sldb-prune-initial-frames frames) t)
          (insert "[No backtrace]")))
      (run-hooks 'sldb-hook)
      (set-syntax-table lisp-mode-syntax-table))
    (slclj-display-popup-buffer t)
    (sldb-recenter-region (point-min) (point))
    (setq buffer-read-only t)
    (when (and slclj-stack-eval-tags
               ;; (y-or-n-p "Enter recursive edit? ")
               )
      (message "Entering recursive edit..")
      (recursive-edit))))

(defun sldb-activate (thread level select)
  "Display the debugger buffer for THREAD.
If LEVEL isn't the same as in the buffer reinitialize the buffer."
  (or (let ((buffer (sldb-find-buffer thread)))
        (when buffer
          (with-current-buffer buffer
            (when (equal sldb-level level)
              (when select (pop-to-buffer (current-buffer)))
              t))))
      (sldb-reinitialize thread level)))

(defun sldb-reinitialize (thread level)
  (slclj-rex (thread level)
      ('(swank:debugger-info-for-emacs 0 10)
       nil thread)
    ((:ok result)
     (apply #'sldb-setup thread level result))))

(defun sldb-exit (thread level &optional stepping)
  "Exit from the debug level LEVEL."
  (when-let (sldb (sldb-find-buffer thread))
    (with-current-buffer sldb
      (cond (stepping
             (setq sldb-level nil))
            (t
             (slclj-popup-buffer-quit t))))))


;;;;;; SLDB buffer insertion

(defun sldb-insert-condition (condition)
  "Insert the text for CONDITION.
CONDITION should be a list (MESSAGE TYPE EXTRAS).
EXTRAS is currently used for the stepper."
  (destructuring-bind (message type extras) condition
    (when (> (length message) 70)
      (add-text-properties 0 (length message) (list 'help-echo message)
                           message))
    (slclj-insert-propertized '(sldb-default-action sldb-inspect-condition)
                              (in-sldb-face topline message)
                              "\n"
                              (in-sldb-face condition type))
    (sldb-dispatch-extras extras)))

(defvar sldb-extras-hooks)

(defun sldb-dispatch-extras (extras)
  ;; this is (mis-)used for the stepper
  (dolist (extra extras)
    (destructure-case extra
      ((:show-frame-source n)
       (sldb-show-frame-source n))
      (t
       (or (run-hook-with-args-until-success 'sldb-extras-hooks extra)
           ;;(error "Unhandled extra element:" extra)
           )))))

(defun sldb-insert-restarts (restarts start count)
  "Insert RESTARTS and add the needed text props
RESTARTS should be a list ((NAME DESCRIPTION) ...)."
  (let* ((len (length restarts))
         (end (if count (min (+ start count) len) len)))
    (loop for (name string) in (subseq restarts start end)
          for number from start  
          do (slclj-insert-propertized
               `(,@nil restart ,number
                       sldb-default-action sldb-invoke-restart
                       mouse-face highlight)
               " " (in-sldb-face restart-number (number-to-string number))
               ": ["  (in-sldb-face restart-type name) "] "
               (in-sldb-face restart string))
             (insert "\n"))
    (when (< end len)
      (let ((pos (point)))
        (slclj-insert-propertized
         (list 'sldb-default-action 
               (slclj-rcurry #'sldb-insert-more-restarts restarts pos end))
         " --more--\n")))))

(defun sldb-insert-more-restarts (restarts position start)
  (goto-char position)
  (let ((inhibit-read-only t))
    (delete-region position (1+ (line-end-position)))
    (sldb-insert-restarts restarts start nil)))

(defun sldb-frame.string (frame)
  (destructuring-bind (_ str &optional _) frame str))

(defun sldb-frame.number (frame)
  (destructuring-bind (n _ &optional _) frame n))

(defun sldb-frame.plist (frame)
  (destructuring-bind (_ _ &optional plist) frame plist))

(defun sldb-frame-restartable-p (frame)
  (and (plist-get (sldb-frame.plist frame) :restartable) t))

(defun sldb-prune-initial-frames (frames)
  "Return the prefix of FRAMES to initially present to the user.
Regexp heuristics are used to avoid showing SWANK-internal frames."
  (let* ((case-fold-search t)
         (rx "^\\([() ]\\|lambda\\)*swank\\>"))
    (or (loop for frame in frames
              until (string-match rx (sldb-frame.string frame))
              collect frame)
        frames)))

(defun sldb-insert-frames (frames more)
  "Insert FRAMES into buffer.
If MORE is non-nil, more frames are on the Lisp stack."
  (mapc #'sldb-insert-frame frames)
  (when more
    (slclj-insert-propertized
     `(,@nil sldb-default-action sldb-fetch-more-frames
             sldb-previous-frame-number 
             ,(sldb-frame.number (first (last frames)))
             point-entered sldb-fetch-more-frames
             start-open t
             face sldb-section-face
             mouse-face highlight)
     " --more--")
    (insert "\n")))

(defun sldb-compute-frame-face (frame)
  (if (sldb-frame-restartable-p frame)
      'sldb-restartable-frame-line-face
      'sldb-frame-line-face))

(defun sldb-insert-frame (frame &optional face)
  "Insert FRAME with FACE at point.
If FACE is nil, `sldb-compute-frame-face' is used to determine the face."
  (setq face (or face (sldb-compute-frame-face frame)))
  (let ((number (sldb-frame.number frame))
        (string (sldb-frame.string frame))
        (props `(frame ,frame sldb-default-action sldb-toggle-details)))
    (slclj-propertize-region props
      (slclj-propertize-region '(mouse-face highlight)
        (insert " " (in-sldb-face frame-label (format "%2d:" number)) " ")
        (slclj-insert-indented
         (slclj-add-face face string)))
      (insert "\n"))))

(defun sldb-fetch-more-frames (&rest ignore)
  "Fetch more backtrace frames.
Called on the `point-entered' text-property hook."
  (let ((inhibit-point-motion-hooks t)
        (inhibit-read-only t)
        (prev (get-text-property (point) 'sldb-previous-frame-number)))
    ;; we may be called twice, PREV is nil the second time
    (when prev
      (let* ((count 40)
             (from (1+ prev))
             (to (+ from count))
             (frames (slclj-eval `(swank:backtrace ,from ,to)))
             (more (slclj-length= frames count))
             (pos (point)))
        (delete-region (line-beginning-position) (point-max))
        (sldb-insert-frames frames more)
        (goto-char pos)))))


;;;;;; SLDB examining text props

(defun sldb-restart-at-point ()
  (or (get-text-property (point) 'restart)
      (error "No restart at point")))

(defun sldb-frame-number-at-point ()
  (let ((frame (get-text-property (point) 'frame)))
    (cond (frame (car frame))
	  (t (error "No frame at point")))))

(defun sldb-var-number-at-point ()
  (let ((var (get-text-property (point) 'var)))
    (cond (var var)
	  (t (error "No variable at point")))))

(defun sldb-previous-frame-number ()
  (save-excursion
    (sldb-backward-frame)
    (sldb-frame-number-at-point)))

(defun sldb-frame-details-visible-p ()
  (and (get-text-property (point) 'frame)
       (get-text-property (point) 'details-visible-p)))

(defun sldb-frame-region ()
  (slclj-property-bounds 'frame))

(defun sldb-forward-frame ()
  (goto-char (next-single-char-property-change (point) 'frame)))

(defun sldb-backward-frame ()
  (when (> (point) sldb-backtrace-start-marker)
    (goto-char (previous-single-char-property-change
                (if (get-text-property (point) 'frame)
                    (car (sldb-frame-region))
                    (point))
                'frame
                nil sldb-backtrace-start-marker))))

(defun sldb-goto-last-frame ()
  (goto-char (point-max))
  (while (not (get-text-property (point) 'frame))
    (goto-char (previous-single-property-change (point) 'frame))))

(defun sldb-beginning-of-backtrace ()
  "Goto the first frame."
  (interactive)
  (goto-char sldb-backtrace-start-marker))


;;;;;; SLDB recenter & redisplay

;; FIXME: these functions need factorization

(defun slclj-show-buffer-position (position &optional recenter)
  "Ensure sure that the POSITION in the current buffer is visible."
  (let ((window (display-buffer (current-buffer) t)))
    (save-selected-window
      (select-window window)
      (goto-char position)
      (ecase recenter
        (top    (recenter 0))
        (center (recenter))
        ((nil)
         (unless (pos-visible-in-window-p)
           (cond ((= (current-column) 0) (recenter 1))
                 (t (recenter)))))))))

(defun sldb-recenter-region (start end &optional center)
  "Make the region from START to END visible.
Avoid point motions, if possible.
Minimize scrolling, if CENTER is nil.
If CENTER is true, scroll enough to center the region in the window."
  (let ((pos (point))  (lines (count-screen-lines start end t)))
    (assert (and (<= start pos) (<= pos end)))
    ;;(sit-for 0)
    (cond ((and (pos-visible-in-window-p start)
                (pos-visible-in-window-p end)))
          ((< lines (window-height))
           (cond (center (recenter (+ (/ (- (window-height) 1 lines)
                                         2)
                                      (slclj-count-lines start pos))))
                 (t (recenter (+ (- (window-height) 1 lines)
                                 (slclj-count-lines start pos))))))
          (t
           (goto-char start)
           (recenter 0)
           (cond ((pos-visible-in-window-p pos)
                  (goto-char pos))
                 (t
                  (goto-char start)
                  (unless noninteractive ; for running the test suite
                    (forward-line (- (window-height) 2)))))))))

;; not sure yet, whether this is a good idea.
(defmacro slclj-save-coordinates (origin &rest body)
  "Restore line and column relative to ORIGIN, after executing BODY.

This is useful if BODY deletes and inserts some text but we want to
preserve the current row and column as closely as possible."
  (let ((base (make-symbol "base"))
        (goal (make-symbol "goal"))
        (mark (make-symbol "mark")))
    `(let* ((,base ,origin)
            (,goal (slclj-coordinates ,base))
            (,mark (point-marker)))
       (set-marker-insertion-type ,mark t)
       (prog1 (save-excursion ,@body)
         (slclj-restore-coordinate ,base ,goal ,mark)))))

(put 'slclj-save-coordinates 'lisp-indent-function 1)

(defun slclj-coordinates (origin)
  ;; Return a pair (X . Y) for the column and line distance to ORIGIN.
  (let ((y (slclj-count-lines origin (point)))
        (x (save-excursion
             (- (current-column)
                (progn (goto-char origin) (current-column))))))
    (cons x y)))

(defun slclj-restore-coordinate (base goal limit)
  ;; Move point to GOAL. Coordinates are relative to BASE.
  ;; Don't move beyond LIMIT.
  (save-restriction
    (narrow-to-region base limit)
    (goto-char (point-min))
    (let ((col (current-column)))
      (forward-line (cdr goal))
      (when (and (eobp) (bolp) (not (bobp)))
        (backward-char))
      (move-to-column (+ col (car goal))))))

(defun slclj-count-lines (start end)
  "Return the number of lines between START and END.
This is 0 if START and END at the same line."
  (- (count-lines start end)
     (if (save-excursion (goto-char end) (bolp)) 0 1)))


;;;;; SLDB commands

(defun sldb-default-action ()
  "Invoke the action at point."
  (interactive)
  (let ((fn (get-text-property (point) 'sldb-default-action)))
    (if fn (funcall fn))))

(defun sldb-default-action/mouse (event)
  "Invoke the action pointed at by the mouse."
  (interactive "e")
  (destructuring-bind (mouse-1 (w pos &rest _)) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point) 'sldb-default-action)))
	(if fn (funcall fn))))))

(defun sldb-cycle ()
  "Cycle between restart list and backtrace."
  (interactive)
  (let ((pt (point)))
    (cond ((< pt sldb-restart-list-start-marker)
           (goto-char sldb-restart-list-start-marker))
          ((< pt sldb-backtrace-start-marker)
           (goto-char sldb-backtrace-start-marker))
          (t
           (goto-char sldb-restart-list-start-marker)))))

(defun sldb-end-of-backtrace ()
  "Fetch the entire backtrace and go to the last frame."
  (interactive)
  (sldb-fetch-all-frames)
  (sldb-goto-last-frame))

(defun sldb-fetch-all-frames ()
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (sldb-goto-last-frame)
    (let ((last (sldb-frame-number-at-point)))
      (goto-char (next-single-char-property-change (point) 'frame))
      (delete-region (point) (point-max))
      (save-excursion
        (sldb-insert-frames (slclj-eval `(swank:backtrace ,(1+ last) nil))
                            nil)))))


;;;;;; SLDB show source

(defun sldb-show-source ()
  "Highlight the frame at point's expression in a source code buffer."
  (interactive)
  (sldb-show-frame-source (sldb-frame-number-at-point)))

(defun sldb-show-frame-source (frame-number)
  (slclj-eval-async
   `(swank:frame-source-location ,frame-number)
   (lambda (source-location)
     (destructure-case source-location
       ((:error message)
        (message "%s" message)
        (ding))
       (t
        (slclj-show-source-location source-location))))))

(defun slclj-show-source-location (source-location &optional no-highlight-p)
  (save-selected-window   ; show the location, but don't hijack focus.
    (slclj-goto-source-location source-location)
    (unless no-highlight-p (slclj-highlight-sexp))
    (slclj-show-buffer-position (point))))

(defun slclj-highlight-sexp (&optional start end)
  "Highlight the first sexp after point."
  (let ((start (or start (point)))
	(end (or end (save-excursion (ignore-errors (forward-sexp)) (point)))))
    (slclj-flash-region start end)))

(defun slclj-highlight-line (&optional timeout)
  (slclj-flash-region (+ (line-beginning-position) (current-indentation)) 
                      (line-end-position)
                      timeout))


;;;;;; SLDB toggle details

(defun sldb-toggle-details (&optional on)
  "Toggle display of details for the current frame.
The details include local variable bindings and CATCH-tags."
  (interactive)
  (assert (sldb-frame-number-at-point))
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (if (or on (not (sldb-frame-details-visible-p)))
	(sldb-show-frame-details)
      (sldb-hide-frame-details))))

(defun sldb-show-frame-details ()
  ;; fetch and display info about local variables and catch tags
  (destructuring-bind (start end frame locals catches) (sldb-frame-details)
    (slclj-save-coordinates start
      (delete-region start end)
      (slclj-propertize-region `(frame ,frame details-visible-p t)
        (sldb-insert-frame frame (if (sldb-frame-restartable-p frame)
                                     'sldb-restartable-frame-line-face
                                     ;; FIXME: can we somehow merge the two?
                                     'sldb-detailed-frame-line-face))
        (let ((indent1 "      ")
              (indent2 "        "))
          (insert indent1 (in-sldb-face section
                            (if locals "Locals:" "[No Locals]")) "\n")
          (sldb-insert-locals locals indent2 frame)
          (when catches
            (insert indent1 (in-sldb-face section "Catch-tags:") "\n")
            (dolist (tag catches)
              (slclj-propertize-region `(catch-tag ,tag)
                (insert indent2 (in-sldb-face catch-tag (format "%s" tag))
                        "\n"))))
          (setq end (point)))))
    (sldb-recenter-region start end)))

(defun sldb-frame-details ()
  ;; Return a list (START END FRAME LOCALS CATCHES) for frame at point.
  (let* ((frame (get-text-property (point) 'frame))
         (num (car frame)))
    (destructuring-bind (start end) (sldb-frame-region)
      (list* start end frame 
             (slclj-eval `(swank:frame-locals-and-catch-tags ,num))))))

(defvar sldb-insert-frame-variable-value-function 
  'sldb-insert-frame-variable-value)

(defun sldb-insert-locals (vars prefix frame)
  "Insert VARS and add PREFIX at the beginning of each inserted line.
VAR should be a plist with the keys :name, :id, and :value."
  (loop for i from 0
        for var in vars do
        (destructuring-bind (&key name id value) var
          (slclj-propertize-region (list 'sldb-default-action 'sldb-inspect-var
                                         'var i)
            (insert prefix
                    (in-sldb-face local-name
                      (concat name (if (zerop id) "" (format "#%d" id))))
                    " = ")
            (funcall sldb-insert-frame-variable-value-function value frame i)
            (insert "\n")))))

(defun sldb-insert-frame-variable-value (value frame index)
  (insert (in-sldb-face local-value value)))  

(defun sldb-hide-frame-details ()
  ;; delete locals and catch tags, but keep the function name and args.
  (destructuring-bind (start end) (sldb-frame-region)
    (let ((frame (get-text-property (point) 'frame)))
      (slclj-save-coordinates start
        (delete-region start end)
        (slclj-propertize-region '(details-visible-p nil)
          (sldb-insert-frame frame))))))

(defun sldb-disassemble ()
  "Disassemble the code for the current frame."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slclj-eval-async `(swank:sldb-disassemble ,frame)
                      (lambda (result)
			(slclj-show-description result nil)))))


;;;;;; SLDB eval and inspect

(defun sldb-eval-in-frame (string)
  "Prompt for an expression and evaluate it in the selected frame."
  (interactive (list (slclj-read-from-minibuffer "Eval in frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slclj-eval-async `(swank:eval-string-in-frame ,string ,number)
                      (if current-prefix-arg
                          'slclj-write-string
                        'slclj-display-eval-result))))

(defun sldb-pprint-eval-in-frame (string)
  "Prompt for an expression, evaluate in selected frame, pretty-print result."
  (interactive (list (slclj-read-from-minibuffer "Eval in frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slclj-eval-async `(swank:pprint-eval-string-in-frame ,string ,number)
		      (lambda (result)
			(slclj-show-description result nil)))))

(defun sldb-inspect-in-frame (string)
  "Prompt for an expression and inspect it in the selected frame."
  (interactive (list (slclj-read-from-minibuffer 
                      "Inspect in frame (evaluated): " 
                      (slclj-sexp-at-point))))
  (let ((number (sldb-frame-number-at-point)))
    (slclj-eval-async `(swank:inspect-in-frame ,string ,number)
                      'slclj-open-inspector)))

(defun sldb-inspect-var ()
  (let ((frame (sldb-frame-number-at-point))
        (var (sldb-var-number-at-point)))
    (slclj-eval-async `(swank:inspect-frame-var ,frame ,var) 
                      'slclj-open-inspector)))

(defun sldb-inspect-condition ()
  "Inspect the current debugger condition."
  (interactive)
  (slclj-eval-async '(swank:inspect-current-condition)
                    'slclj-open-inspector))


;;;;;; SLDB movement

(defun sldb-down ()
  "Select next frame."
  (interactive)
  (sldb-forward-frame))

(defun sldb-up ()
  "Select previous frame."
  (interactive)
  (sldb-backward-frame)
  (when (= (point) sldb-backtrace-start-marker)
    (recenter (1+ (count-lines (point-min) (point))))))

(defun sldb-sugar-move (move-fn)
  (let ((inhibit-read-only t))
    (when (sldb-frame-details-visible-p) (sldb-hide-frame-details))
    (funcall move-fn)
    (sldb-show-source)
    (sldb-toggle-details t)))

(defun sldb-details-up ()
  "Select previous frame and show details."
  (interactive)
  (sldb-sugar-move 'sldb-up))

(defun sldb-details-down ()
  "Select next frame and show details."
  (interactive)
  (sldb-sugar-move 'sldb-down))


;;;;;; SLDB restarts

(defun sldb-quit ()
  "Quit to toplevel."
  (interactive)
  (assert sldb-restarts () "sldb-quit called outside of sldb buffer")
  (slclj-rex () ('(swank:throw-to-toplevel))
    ((:ok x) (error "sldb-quit returned [%s]" x))
    ((:abort))))

(defun sldb-continue ()
  "Invoke the \"continue\" restart."
  (interactive)
  (assert sldb-restarts () "sldb-continue called outside of sldb buffer")
  (slclj-rex ()
      ('(swank:sldb-continue))
    ((:ok _)
     (message "No restart named continue")
     (ding))
    ((:abort))))

(defun sldb-abort ()
  "Invoke the \"abort\" restart."
  (interactive)
  (slclj-eval-async '(swank:sldb-abort)
                    (lambda (v) (message "Restart returned: %S" v))))

(defun sldb-invoke-restart (&optional number)
  "Invoke a restart.
Optional NUMBER (index into `sldb-restarts') specifies the
restart to invoke, otherwise use the restart at point."
  (interactive)
  (let ((restart (or number (sldb-restart-at-point))))
    (slclj-rex ()
        ((list 'swank:invoke-nth-restart-for-emacs sldb-level restart))
      ((:ok value) (message "Restart returned: %s" value))
      ((:abort)))))

(defun sldb-invoke-restart-by-name (restart-name)
  (interactive (list (let ((completion-ignore-case t))
                       (completing-read "Restart: " sldb-restarts nil t
                                        ""
                                        'sldb-invoke-restart-by-name))))
  (sldb-invoke-restart (position restart-name sldb-restarts 
                                 :test 'string= :key 'first)))

(defun sldb-break-with-default-debugger (&optional dont-unwind)
  "Enter default debugger."
  (interactive "P")
  (slclj-rex ()
      ((list 'swank:sldb-break-with-default-debugger 
             (not (not dont-unwind)))
       nil slclj-current-thread)
    ((:abort))))

(defun sldb-break-with-system-debugger (&optional lightweight)
  "Enter system debugger (gdb)."
  (interactive "P")
  (slclj-attach-gdb slclj-buffer-connection lightweight))

(defun slclj-attach-gdb (connection &optional lightweight)
  "Run `gud-gdb'on the connection with PID `pid'. 

If `lightweight' is given, do not send any request to the
inferior Lisp (e.g. to obtain default gdb config) but only
operate from the Emacs side; intended for cases where the Lisp is
truly screwed up."
  (interactive
   (list (slclj-read-connection "Attach gdb to: " (slclj-connection)) "P"))
  (let ((pid  (slclj-pid connection))
        (file (slclj-lisp-implementation-program connection))
        (commands (unless lightweight
                    (let ((slclj-dispatching-connection connection))
                      (slclj-eval `(swank:gdb-initial-commands))))))
    (gud-gdb (format "gdb -p %d %s" pid (or file "")))
    (with-current-buffer gud-comint-buffer
      (dolist (cmd commands)
        ;; First wait until gdb was initialized, then wait until current
        ;; command was processed.
        (while (not (looking-back comint-prompt-regexp))
          (sit-for 0.01))
        ;; We do not use `gud-call' because we want the initial commands
        ;; to be displayed by the user so he knows what he's got.
        (insert cmd)
        (comint-send-input)))))


(defun sldb-step ()
  "Step to next basic-block boundary."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slclj-eval-async `(swank:sldb-step ,frame))))

(defun sldb-next ()
  "Step over call."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slclj-eval-async `(swank:sldb-next ,frame))))

(defun sldb-out ()
  "Resume stepping after returning from this function."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slclj-eval-async `(swank:sldb-out ,frame))))

(defun sldb-break-on-return ()
  "Set a breakpoint at the current frame.
The debugger is entered when the frame exits."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slclj-eval-async `(swank:sldb-break-on-return ,frame)
                      (lambda (msg) (message "%s" msg)))))

(defun sldb-break (name)
  "Set a breakpoint at the start of the function NAME."
  (interactive (list (slclj-read-symbol-name "Function: " t)))
  (slclj-eval-async `(swank:sldb-break ,name)
                    (lambda (msg) (message "%s" msg))))

(defun sldb-return-from-frame (string)
  "Reads an expression in the minibuffer and causes the function to
return that value, evaluated in the context of the frame."
  (interactive (list (slclj-read-from-minibuffer "Return from frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slclj-rex ()
        ((list 'swank:sldb-return-from-frame number string))
      ((:ok value) (message "%s" value))
      ((:abort)))))

(defun sldb-restart-frame ()
  "Causes the frame to restart execution with the same arguments as it
was called originally."
  (interactive)
  (let* ((number (sldb-frame-number-at-point)))
    (slclj-rex ()
        ((list 'swank:restart-frame number))
      ((:ok value) (message "%s" value))
      ((:abort)))))


;;;;;; SLDB recompilation commands

(defun sldb-recompile-frame-source (&optional raw-prefix-arg)
  (interactive "P")
  (slclj-eval-async
   `(swank:frame-source-location ,(sldb-frame-number-at-point))
   (lexical-let ((policy (slclj-compute-policy raw-prefix-arg)))
     (lambda (source-location)
       (destructure-case source-location
         ((:error message)
          (message "%s" message)
          (ding))
         (t
          (let ((slclj-compilation-policy policy))
            (slclj-recompile-location source-location))))))))


;;;; Thread control panel

(defvar slclj-threads-buffer-name "*SLCLJ Threads*")

(defun slclj-list-threads ()
  "Display a list of threads."
  (interactive)
  (let ((name slclj-threads-buffer-name))
    (slclj-with-popup-buffer (name nil t)
      (slclj-thread-control-mode)
      (slclj-update-threads-buffer)
      (setq slclj-popup-buffer-quit-function 'slclj-quit-threads-buffer))))

(defun slclj-longest-lines (list-of-lines)
  (let ((lengths (make-list (length (car list-of-lines)) 0)))
    (flet ((process-line (line)
             (loop for element in line
                   for length on lengths
                   do (setf (car length)
                            (max (length (prin1-to-string element t))
                                 (car length))))))
      (mapc 'process-line list-of-lines)
      lengths)))

(defun slclj-quit-threads-buffer (&optional _)
  (slclj-eval-async `(swank:quit-thread-browser))
  (slclj-popup-buffer-quit t))

(defun slclj-update-threads-buffer ()
  (interactive)
  (let ((threads (slclj-eval '(swank:list-threads))))
    (with-current-buffer slclj-threads-buffer-name
      (let ((inhibit-read-only t))
        (erase-buffer)
        (slclj-insert-threads threads)
        (goto-char (point-min))))))

(defvar *slclj-threads-table-properties*
  '(nil (face bold)))

(defun slclj-format-threads-labels (threads)
  (let ((labels (mapcar (lambda (x)
                          (capitalize (substring (symbol-name x) 1)))
                        (car threads))))
    (cons labels (cdr threads))))

(defun slclj-insert-thread (thread longest-lines)
  (unless (bolp) (insert "\n"))
  (loop for i from 0
        for align in longest-lines
        for element in thread
        for string = (prin1-to-string element t)
        for property = (nth i *slclj-threads-table-properties*)
        do
        (if property
            (slclj-insert-propertized property string)
            (insert string))
        (insert-char ?\  (- align (length string) -3))))

(defun slclj-insert-threads (threads)
  (let* ((threads (slclj-format-threads-labels threads))
         (longest-lines (slclj-longest-lines threads)))
    (setq header-line-format
          (concat (propertize " " 'display '((space :align-to 0)))
                  (let (*slclj-threads-table-properties*)
                    (with-temp-buffer
                      (slclj-insert-thread (car threads) longest-lines)
                      (buffer-string)))))
    (loop for thread-id from 0
          for thread in (cdr threads)
          do
          (slclj-propertize-region `(thread-id ,thread-id)
            (slclj-insert-thread thread longest-lines)))))


;;;;; Major mode

(define-derived-mode slclj-thread-control-mode fundamental-mode
  "Threads"
  "SLCLJ Thread Control Panel Mode.

\\{slclj-thread-control-mode-map}
\\{slclj-popup-buffer-mode-map}"
  (when slclj-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(slclj-define-keys slclj-thread-control-mode-map
  ("a" 'slclj-thread-attach)
  ("d" 'slclj-thread-debug)
  ("g" 'slclj-update-threads-buffer)
  ("k" 'slclj-thread-kill))

(defun slclj-thread-kill ()
  (interactive)
  (slclj-eval `(cl:mapc 'swank:kill-nth-thread
                        ',(slclj-get-properties 'thread-id)))
  (call-interactively 'slclj-list-threads))

(defun slclj-get-region-properties (prop start end)
  (loop for position = (if (get-text-property start prop)
                           start
                           (next-single-property-change start prop))
        then (next-single-property-change position prop)
        while (<= position end)
        collect (get-text-property position prop)))

(defun slclj-get-properties (prop)
  (if (use-region-p)
      (slclj-get-region-properties prop
                                   (region-beginning)
                                   (region-end))
      (let ((value (get-text-property (point) prop)))
        (when value
          (list value)))))

(defun slclj-thread-attach ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id))
        (file (slclj-swank-port-file)))
    (slclj-eval-async `(swank:start-swank-server-in-thread ,id ,file)))
  (slclj-read-port-and-connect nil nil))

(defun slclj-thread-debug ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id)))
    (slclj-eval-async `(swank:debug-nth-thread ,id))))


;;;;; Connection listing

(define-derived-mode slclj-connection-list-mode fundamental-mode
  "Slclj-Connections"
  "SLCLJ Connection List Mode.

\\{slclj-connection-list-mode-map}
\\{slclj-popup-buffer-mode-map}"
  (when slclj-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(slclj-define-keys slclj-connection-list-mode-map
  ("d"         'slclj-connection-list-make-default)
  ("g"         'slclj-update-connection-list)
  ((kbd "C-k") 'slclj-quit-connection-at-point)
  ("R"         'slclj-restart-connection-at-point))

(defun slclj-connection-at-point ()
  (or (get-text-property (point) 'slclj-connection)
      (error "No connection at point")))

(defun slclj-quit-connection-at-point (connection)
  (interactive (list (slclj-connection-at-point)))
  (let ((slclj-dispatching-connection connection)
        (end (time-add (current-time) (seconds-to-time 3))))
    (slclj-quit-lisp t)
    (while (memq connection slclj-net-processes)
      (when (time-less-p end (current-time))
        (message "Quit timeout expired.  Disconnecting.")
        (delete-process connection))
      (sit-for 0 100)))
  (slclj-update-connection-list))

(defun slclj-restart-connection-at-point (connection)
  (interactive (list (slclj-connection-at-point)))
  (let ((slclj-dispatching-connection connection))
    (slclj-restart-inferior-lisp)))
  
(defun slclj-connection-list-make-default ()
  "Make the connection at point the default connection."
  (interactive)
  (slclj-select-connection (slclj-connection-at-point))
  (slclj-update-connection-list))

(defvar slclj-connections-buffer-name "*SLCLJ Connections*")

(defun slclj-list-connections ()
  "Display a list of all connections."
  (interactive)
  (slclj-with-popup-buffer (slclj-connections-buffer-name)
    (slclj-connection-list-mode)
    (slclj-draw-connection-list)))

(defun slclj-update-connection-list ()
 "Display a list of all connections."
 (interactive)
 (let ((pos (point))
       (inhibit-read-only t))
   (erase-buffer)
   (slclj-draw-connection-list)
   (goto-char pos)))

(defun slclj-draw-connection-list ()
  (let ((default-pos nil)
        (default slclj-default-connection)
        (fstring "%s%2s  %-10s  %-17s  %-7s %-s\n"))
    (insert (format fstring " " "Nr" "Name" "Port" "Pid" "Type")
            (format fstring " " "--" "----" "----" "---" "----"))
    (dolist (p (reverse slclj-net-processes))
      (when (eq default p) (setf default-pos (point)))
      (slclj-insert-propertized 
       (list 'slclj-connection p)
       (format fstring
               (if (eq default p) "*" " ")
               (slclj-connection-number p)
               (slclj-connection-name p)
               (or (process-id p) (process-contact p))
               (slclj-pid p)
               (slclj-lisp-implementation-type p))))
    (when default 
      (goto-char default-pos))))


;;;; Inspector

(defgroup slclj-inspector nil
  "Inspector faces."
  :prefix "slclj-inspector-"
  :group 'slclj)

(defface slclj-inspector-topline-face
  '((t ()))
  "Face for top line describing object."
  :group 'slclj-inspector)

(defface slclj-inspector-label-face
  '((t (:inherit font-lock-constant-face)))
  "Face for labels in the inspector."
  :group 'slclj-inspector)

(defface slclj-inspector-value-face
  (if (slclj-face-inheritance-possible-p)
      '((t (:inherit font-lock-builtin-face)))
    '((((background light)) (:foreground "MediumBlue" :bold t))
      (((background dark)) (:foreground "LightGray" :bold t))))
  "Face for things which can themselves be inspected."
  :group 'slclj-inspector)

(defface slclj-inspector-action-face
  (if (slclj-face-inheritance-possible-p)
      '((t (:inherit font-lock-warning-face)))
    '((t (:foreground "OrangeRed"))))
  "Face for labels of inspector actions."
  :group 'slclj-inspector)

(defface slclj-inspector-type-face
    '((t (:inherit font-lock-type-face)))
  "Face for type description in inspector."
  :group 'slclj-inspector)

(defvar slclj-inspector-mark-stack '())
(defvar slclj-saved-window-config)

(defun slclj-inspect (string)
  "Eval an expression and inspect the result."
  (interactive 
   (list (slclj-read-from-minibuffer "Inspect value (evaluated): "
				     (slclj-sexp-at-point))))
  (slclj-eval-async `(swank:init-inspector ,string) 'slclj-open-inspector))

(define-derived-mode slclj-inspector-mode fundamental-mode "Slclj-Inspector"
  (set-syntax-table lisp-mode-syntax-table)
  (slclj-set-truncate-lines)
  (setq buffer-read-only t))

(defun slclj-inspector-buffer ()
  (or (get-buffer "*Slclj Inspector*")
      (with-current-buffer (get-buffer-create "*Slclj Inspector*")
	(setq slclj-inspector-mark-stack '())
        (buffer-disable-undo)
        (slclj-mode t)
	(slclj-inspector-mode)
        (make-local-variable 'slclj-saved-window-config)
        (setq slclj-saved-window-config (current-window-configuration))
	(current-buffer))))

(defmacro slclj-inspector-fontify (face string)
  `(slclj-add-face ',(intern (format "slclj-inspector-%s-face" face)) ,string))

(defvar slclj-inspector-insert-ispec-function 'slclj-inspector-insert-ispec)

(defun slclj-open-inspector (inspected-parts &optional point hook)
  "Display INSPECTED-PARTS in a new inspector window.
Optionally set point to POINT. If HOOK is provided, it is added to local
KILL-BUFFER hooks for the inspector buffer."
  (with-current-buffer (slclj-inspector-buffer)
    (when hook
      (add-hook 'kill-buffer-hook hook t t))
    (setq slclj-buffer-connection (slclj-current-connection))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (destructuring-bind (&key id title content) inspected-parts
        (macrolet ((fontify (face string) 
                            `(slclj-inspector-fontify ,face ,string)))
          (slclj-propertize-region
              (list 'slclj-part-number id 
                 'mouse-face 'highlight
                 'face 'slclj-inspector-value-face)
            (insert title))
          (while (eq (char-before) ?\n)
            (backward-delete-char 1))
          (insert "\n" (fontify label "--------------------") "\n")
          (save-excursion
            (slclj-inspector-insert-content content))
          (pop-to-buffer (current-buffer))
          (when point
            (check-type point cons)
            (ignore-errors 
              (goto-line (car point))
              (move-to-column (cdr point)))))))))

(defvar slclj-inspector-limit 500)

(defun slclj-inspector-insert-content (content)
  (slclj-inspector-fetch-chunk
   content nil 
   (lambda (chunk)
     (let ((inhibit-read-only t))
       (slclj-inspector-insert-chunk chunk t t)))))

(defun slclj-inspector-insert-chunk (chunk prev next)
  "Insert CHUNK at point.
If PREV resp. NEXT are true insert more-buttons as needed."
  (destructuring-bind (ispecs len start end) chunk
    (when (and prev (> start 0))
      (slclj-inspector-insert-more-button start t))
    (mapc #'slclj-inspector-insert-ispec ispecs)
    (when (and next (< end len))
      (slclj-inspector-insert-more-button end nil))))

(defun slclj-inspector-insert-ispec (ispec)
  (if (stringp ispec)
      (insert ispec)
    (destructure-case ispec
      ((:value string id)
       (slclj-propertize-region 
           (list 'slclj-part-number id 
                 'mouse-face 'highlight
                 'face 'slclj-inspector-value-face)
         (insert string)))
      ((:action string id)
       (slclj-insert-propertized (list 'slclj-action-number id
                                       'mouse-face 'highlight
                                       'face 'slclj-inspector-action-face)
                                 string)))))

(defun slclj-inspector-position ()
  "Return a pair (Y-POSITION X-POSITION) representing the
position of point in the current buffer."
  ;; We make sure we return absolute coordinates even if the user has
  ;; narrowed the buffer.
  ;; FIXME: why would somebody narrow the buffer?
  (save-restriction
    (widen)
    (cons (line-number-at-pos)
          (current-column))))

(defun slclj-inspector-operate-on-point ()
  "Invoke the command for the text at point.
1. If point is on a value then recursivly call the inspector on
that value.  
2. If point is on an action then call that action.
3. If point is on a range-button fetch and insert the range."
  (interactive)
  (let ((part-number (get-text-property (point) 'slclj-part-number))
        (range-button (get-text-property (point) 'slclj-range-button))
        (action-number (get-text-property (point) 'slclj-action-number))
        (opener (lexical-let ((point (slclj-inspector-position)))
                  (lambda (parts)
                    (when parts
                      (slclj-open-inspector parts point))))))
    (cond (part-number
           (slclj-eval-async `(swank:inspect-nth-part ,part-number)
                             opener)
           (push (slclj-inspector-position) slclj-inspector-mark-stack))
          (range-button
           (slclj-inspector-fetch-more range-button))
          (action-number 
           (slclj-eval-async `(swank::inspector-call-nth-action ,action-number)
                             opener))
          (t (error "No object at point")))))

(defun slclj-inspector-operate-on-click (event)
  "Move to events' position and operate the part."
  (interactive "@e")
  (let ((point (posn-point (event-end event))))
    (cond ((and point
                (or (get-text-property point 'slclj-part-number)
                    (get-text-property point 'slclj-range-button)
                    (get-text-property point 'slclj-action-number)))
           (goto-char point)
           (slclj-inspector-operate-on-point))
          (t
           (error "No clickable part here")))))

(defun slclj-inspector-pop ()
  "Reinspect the previous object."
  (interactive)
  (slclj-eval-async 
   `(swank:inspector-pop)
   (lambda (result)
     (cond (result
	    (slclj-open-inspector result (pop slclj-inspector-mark-stack)))
	   (t 
	    (message "No previous object")
	    (ding))))))

(defun slclj-inspector-next ()
  "Inspect the next object in the history."
  (interactive)
  (let ((result (slclj-eval `(swank:inspector-next))))
    (cond (result 
	   (push (slclj-inspector-position) slclj-inspector-mark-stack)
	   (slclj-open-inspector result))
	  (t (message "No next object")
	     (ding)))))
  
(defun slclj-inspector-quit ()
  "Quit the inspector and kill the buffer."
  (interactive)
  (slclj-eval-async `(swank:quit-inspector))
  (set-window-configuration slclj-saved-window-config)
  (kill-buffer (current-buffer)))

;; FIXME: first return value is just point.
;; FIXME: could probably use slclj-search-property.
(defun slclj-find-inspectable-object (direction limit)
  "Find the next/previous inspectable object.
DIRECTION can be either 'next or 'prev.  
LIMIT is the maximum or minimum position in the current buffer.

Return a list of two values: If an object could be found, the
starting position of the found object and T is returned;
otherwise LIMIT and NIL is returned."
  (let ((finder (ecase direction
                  (next 'next-single-property-change)
                  (prev 'previous-single-property-change))))
    (let ((prop nil) (curpos (point)))
      (while (and (not prop) (not (= curpos limit)))
        (let ((newpos (funcall finder curpos 'slclj-part-number nil limit)))
          (setq prop (get-text-property newpos 'slclj-part-number))
          (setq curpos newpos)))
      (list curpos (and prop t)))))

(defun slclj-inspector-next-inspectable-object (arg)
  "Move point to the next inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move backwards."
  (interactive "p")
  (let ((maxpos (point-max)) (minpos (point-min))
        (previously-wrapped-p nil))
    ;; Forward.
    (while (> arg 0)
      (destructuring-bind (pos foundp)
          (slclj-find-inspectable-object 'next maxpos)
        (if foundp
            (progn (goto-char pos) (setq arg (1- arg))
                   (setq previously-wrapped-p nil))
            (if (not previously-wrapped-p) ; cycle detection
                (progn (goto-char minpos) (setq previously-wrapped-p t))
                (error "No inspectable objects")))))
    ;; Backward.
    (while (< arg 0)
      (destructuring-bind (pos foundp)
          (slclj-find-inspectable-object 'prev minpos)
        ;; SLCLJ-OPEN-INSPECTOR inserts the title of an inspector page
        ;; as a presentation at the beginning of the buffer; skip
        ;; that.  (Notice how this problem can not arise in ``Forward.'')
        (if (and foundp (/= pos minpos))
            (progn (goto-char pos) (setq arg (1+ arg))
                   (setq previously-wrapped-p nil))
            (if (not previously-wrapped-p) ; cycle detection
                (progn (goto-char maxpos) (setq previously-wrapped-p t))
                (error "No inspectable objects")))))))

(defun slclj-inspector-previous-inspectable-object (arg)
  "Move point to the previous inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move forwards."
  (interactive "p")
  (slclj-inspector-next-inspectable-object (- arg)))
  
(defun slclj-inspector-describe ()
  (interactive)
  (slclj-eval-describe `(swank:describe-inspectee)))

(defun slclj-inspector-pprint (part)
  (interactive (list (or (get-text-property (point) 'slclj-part-number)
                         (error "No part at point"))))
  (slclj-eval-describe `(swank:pprint-inspector-part ,part)))

(defun slclj-inspector-eval (string)
  "Eval an expression in the context of the inspected object."
  (interactive (list (slclj-read-from-minibuffer "Inspector eval: ")))
  (slclj-eval-with-transcript `(swank:inspector-eval ,string)))

(defun slclj-inspector-history ()
  "Show the previously inspected objects."
  (interactive)
  (slclj-eval-describe `(swank:inspector-history)))

(defun slclj-inspector-show-source (part)
  (interactive (list (or (get-text-property (point) 'slclj-part-number)
                         (error "No part at point"))))
  (slclj-eval-async 
   `(swank:find-source-location-for-emacs '(:inspector ,part))
   #'slclj-show-source-location))
  
(defun slclj-inspector-reinspect ()
  (interactive)
  (slclj-eval-async `(swank:inspector-reinspect)
                    (lexical-let ((point (slclj-inspector-position)))
                      (lambda (parts)
                        (slclj-open-inspector parts point)))))

(defun slclj-inspector-toggle-verbose ()
  (interactive)
  (slclj-eval-async `(swank:inspector-toggle-verbose)
                    (lexical-let ((point (slclj-inspector-position)))
                      (lambda (parts)
                        (slclj-open-inspector parts point)))))

(defun slclj-inspector-insert-more-button (index previous)
  (slclj-insert-propertized 
   (list 'slclj-range-button (list index previous)
         'mouse-face 'highlight
         'face 'slclj-inspector-action-face)
   (if previous " [--more--]\n" " [--more--]")))

(defun slclj-inspector-fetch-all ()
  "Fetch all inspector contents and go to the end."
  (interactive)
  (goto-char (1- (point-max)))
  (let ((button (get-text-property (point) 'slclj-range-button)))
    (when button
      (let (slclj-inspector-limit)
        (slclj-inspector-fetch-more button)))))

(defun slclj-inspector-fetch-more (button)
  (destructuring-bind (index prev) button
    (slclj-inspector-fetch-chunk 
     (list '() (1+ index) index index) prev
     (slclj-rcurry 
      (lambda (chunk prev)
        (let ((inhibit-read-only t))
          (apply #'delete-region (slclj-property-bounds 'slclj-range-button))
          (slclj-inspector-insert-chunk chunk prev (not prev))))
      prev))))

(defun slclj-inspector-fetch-chunk (chunk prev cont)
  (slclj-inspector-fetch chunk slclj-inspector-limit prev cont))

(defun slclj-inspector-fetch (chunk limit prev cont)
  (destructuring-bind (from to) (slclj-inspector-next-range chunk limit prev)
    (cond ((and from to)
           (slclj-eval-async 
            `(swank:inspector-range ,from ,to)
            (slclj-rcurry (lambda (chunk2 chunk1 limit prev cont)
                            (slclj-inspector-fetch 
                             (slclj-inspector-join-chunks chunk1 chunk2)
                             limit prev cont))
                          chunk limit prev cont)))
          (t (funcall cont chunk)))))

(defun slclj-inspector-next-range (chunk limit prev)
  (destructuring-bind (_ len start end) chunk
    (let ((count (- end start)))
      (cond ((and prev (< 0 start) (or (not limit) (< count limit)))
             (list (if limit (max (- end limit) 0) 0) start))
            ((and (not prev) (< end len) (or (not limit) (< count limit)))
             (list end (if limit (+ start limit) most-positive-fixnum)))
            (t '(nil nil))))))

(defun slclj-inspector-join-chunks (chunk1 chunk2)
  (destructuring-bind (i1 l1 s1 e1) chunk1
    (destructuring-bind (i2 l2 s2 e2) chunk2
      (cond ((= e1 s2)
             (list (append i1 i2) l2 s1 e2))
            ((= e2 s1)
             (list (append i2 i1) l2 s2 e1))
            (t (error "Invalid chunks"))))))

(set-keymap-parent slclj-inspector-mode-map slclj-parent-map)

(slclj-define-keys slclj-inspector-mode-map
  ([return] 'slclj-inspector-operate-on-point)
  ("\C-m"   'slclj-inspector-operate-on-point)
  ([mouse-2] 'slclj-inspector-operate-on-click)
  ("l" 'slclj-inspector-pop)
  ("n" 'slclj-inspector-next)
  (" " 'slclj-inspector-next)
  ("d" 'slclj-inspector-describe)
  ("p" 'slclj-inspector-pprint)
  ("e" 'slclj-inspector-eval)
  ("h" 'slclj-inspector-history)
  ("q" 'slclj-inspector-quit)
  ("g" 'slclj-inspector-reinspect)
  ("v" 'slclj-inspector-toggle-verbose)
  ("\C-i" 'slclj-inspector-next-inspectable-object)
  ([(shift tab)] 'slclj-inspector-previous-inspectable-object) ; Emacs translates S-TAB
  ([backtab]     'slclj-inspector-previous-inspectable-object) ; to BACKTAB on X.
  ("." 'slclj-inspector-show-source)
  (">" 'slclj-inspector-fetch-all))


;;;; Buffer selector

(defvar slclj-selector-methods nil
  "List of buffer-selection methods for the `slclj-select' command.
Each element is a list (KEY DESCRIPTION FUNCTION).
DESCRIPTION is a one-line description of what the key selects.")

(defvar slclj-selector-other-window nil
  "If non-nil use switch-to-buffer-other-window.")

(defun slclj-selector (&optional other-window)
  "Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer. The `?' character describes the
available methods.

See `def-slclj-selector-method' for defining new methods."
  (interactive)
  (message "Select [%s]: " 
           (apply #'string (mapcar #'car slclj-selector-methods)))
  (let* ((slclj-selector-other-window other-window)
         (ch (save-window-excursion
               (select-window (minibuffer-window))
               (read-char)))
         (method (find ch slclj-selector-methods :key #'car)))
    (cond (method 
           (funcall (third method)))
          (t
           (message "No method for character: ?\\%c" ch)
           (ding)
           (sleep-for 1)
           (discard-input)
           (slclj-selector)))))

(defmacro def-slclj-selector-method (key description &rest body)
  "Define a new `slclj-select' buffer selection method.

KEY is the key the user will enter to choose this method.

DESCRIPTION is a one-line sentence describing how the method
selects a buffer.

BODY is a series of forms which are evaluated when the selector
is chosen. The returned buffer is selected with
switch-to-buffer."
  (let ((method `(lambda () 
                   (let ((buffer (progn ,@body)))
                     (cond ((not (get-buffer buffer))
                            (message "No such buffer: %S" buffer)
                            (ding))
                           ((get-buffer-window buffer)
                            (select-window (get-buffer-window buffer)))
                           (slclj-selector-other-window
                            (switch-to-buffer-other-window buffer))
                           (t
                            (switch-to-buffer buffer)))))))
    `(setq slclj-selector-methods
           (sort* (cons (list ,key ,description ,method)
                        (remove* ,key slclj-selector-methods :key #'car))
                  #'< :key #'car))))

(def-slclj-selector-method ?? "Selector help buffer."
  (ignore-errors (kill-buffer "*Select Help*"))
  (with-current-buffer (get-buffer-create "*Select Help*")
    (insert "Select Methods:\n\n")
    (loop for (key line function) in slclj-selector-methods
          do (insert (format "%c:\t%s\n" key line)))
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (slclj-selector)
  (current-buffer))

(pushnew (list ?4 "Select in other window" (lambda () (slclj-selector t)))
         slclj-selector-methods :key #'car)

(def-slclj-selector-method ?q "Abort."
  (top-level))

(def-slclj-selector-method ?i
  "*inferior-lisp* buffer."
  (cond ((and (slclj-connected-p) (slclj-process))
         (process-buffer (slclj-process)))
        (t
         "*inferior-lisp*")))

(def-slclj-selector-method ?v
  "*slclj-events* buffer."
  slclj-event-buffer-name)

(def-slclj-selector-method ?l
  "most recently visited lisp-mode buffer."
  (slclj-recently-visited-buffer 'lisp-mode))

(def-slclj-selector-method ?d
  "*sldb* buffer for the current connection."
  (or (sldb-get-default-buffer)
      (error "No debugger buffer")))

(def-slclj-selector-method ?e
  "most recently visited emacs-lisp-mode buffer."
  (slclj-recently-visited-buffer 'emacs-lisp-mode))

(def-slclj-selector-method ?c
  "SLCLJ connections buffer."
  (slclj-list-connections)
  slclj-connections-buffer-name)

(def-slclj-selector-method ?n
  "Cycle to the next Lisp connection."
  (slclj-cycle-connections)
  (concat "*slclj-repl "
          (slclj-connection-name (slclj-current-connection))
          "*"))

(def-slclj-selector-method ?t
  "SLCLJ threads buffer."
  (slclj-list-threads)
  slclj-threads-buffer-name)

(defun slclj-recently-visited-buffer (mode)
  "Return the most recently visited buffer whose major-mode is MODE.
Only considers buffers that are not already visible."
  (loop for buffer in (buffer-list)
        when (and (with-current-buffer buffer (eq major-mode mode))
                  (not (string-match "^ " (buffer-name buffer)))
                  (null (get-buffer-window buffer 'visible)))
        return buffer
        finally (error "Can't find unshown buffer in %S" mode)))


;;;; Indentation

(defun slclj-update-indentation ()
  "Update indentation for all macros defined in the Lisp system."
  (interactive)
  (slclj-eval-async '(swank:update-indentation-information)))

(defvar slclj-indentation-update-hooks)

(defun slclj-handle-indentation-update (alist)
  "Update Lisp indent information.

ALIST is a list of (SYMBOL-NAME . INDENT-SPEC) of proposed indentation
settings for `common-lisp-indent-function'. The appropriate property
is setup, unless the user already set one explicitly."
  (dolist (info alist)
    (let ((symbol (intern (car info)))
          (indent (cdr info)))
      ;; Does the symbol have an indentation value that we set?
      (when (equal (get symbol 'common-lisp-indent-function)
                   (get symbol 'slclj-indent))
        (put symbol 'common-lisp-indent-function indent)
        (put symbol 'slclj-indent indent))
      (run-hook-with-args 'slclj-indentation-update-hooks symbol indent))))


;;;; Contrib modules

(defvar slclj-required-modules '())

(defun slclj-require (module)
  (assert (keywordp module))
  (pushnew module slclj-required-modules)
  (when (slclj-connected-p)
    (slclj-load-contribs)))

(defun slclj-load-contribs ()
  (let ((needed (remove-if (lambda (s) 
                             (member (subseq (symbol-name s) 1)
                                     (mapcar #'downcase (slclj-lisp-modules))))
                           slclj-required-modules)))
    (when needed
      ;; No asynchronous request because with :SPAWN that could result
      ;; in the attempt to load modules concurrently which may not be
      ;; supported by the host Lisp.
      (setf (slclj-lisp-modules) 
            (slclj-eval `(swank:swank-require ',needed))))))


;;;;; Pull-down menu

(defvar slclj-easy-menu
  (let ((C '(slclj-connected-p)))
    `("SLCLJ"
      [ "Edit Definition..."       slclj-edit-definition ,C ]
      [ "Return From Definition"   slclj-pop-find-definition-stack ,C ]
      [ "Complete Symbol"          slclj-complete-symbol ,C ]
      "--"
      ("Evaluation"
       [ "Eval Defun"              slclj-eval-defun ,C ]
       [ "Eval Last Expression"    slclj-eval-last-expression ,C ]
       [ "Eval And Pretty-Print"   slclj-pprint-eval-last-expression ,C ]
       [ "Eval Region"             slclj-eval-region ,C ]
       [ "Interactive Eval..."     slclj-interactive-eval ,C ]
       [ "Edit Lisp Value..."      slclj-edit-value ,C ]
       [ "Call Defun"              slclj-call-defun ,C ])
      ("Debugging"
       [ "Macroexpand Once..."     slclj-macroexpand-1 ,C ]
       [ "Macroexpand All..."      slclj-macroexpand-all ,C ]
       [ "Create Trace Buffer"     slclj-redirect-trace-output ,C ]
       [ "Toggle Trace..."         slclj-toggle-trace-fdefinition ,C ]
       [ "Untrace All"             slclj-untrace-all ,C]
       [ "Disassemble..."          slclj-disassemble-symbol ,C ]
       [ "Inspect..."              slclj-inspect ,C ])
      ("Compilation"
       [ "Compile Defun"           slclj-compile-defun ,C ]
       [ "Compile/Load File"       slclj-compile-and-load-file ,C ]
       [ "Compile File"            slclj-compile-file ,C ]
       [ "Compile Region"          slclj-compile-region ,C ]
       "--"
       [ "Next Note"               slclj-next-note t ]
       [ "Previous Note"           slclj-previous-note t ]
       [ "Remove Notes"            slclj-remove-notes t ]
       [ "List Notes"              slclj-list-compiler-notes ,C ])
      ("Cross Reference"
       [ "Who Calls..."            slclj-who-calls ,C ]
       [ "Who References... "      slclj-who-references ,C ]
       [ "Who Sets..."             slclj-who-sets ,C ]
       [ "Who Binds..."            slclj-who-binds ,C ]
       [ "Who Macroexpands..."     slclj-who-macroexpands ,C ]
       [ "Who Specializes..."      slclj-who-specializes ,C ]
       [ "List Callers..."         slclj-list-callers ,C ]
       [ "List Callees..."         slclj-list-callees ,C ]
       [ "Next Location"           slclj-next-location t ])
      ("Editing"
       [ "Check Parens"            check-parens t]
       [ "Update Indentation"      slclj-update-indentation ,C]
       [ "Select Buffer"           slclj-selector t])
      ("Profiling"
       [ "Toggle Profiling..."     slclj-toggle-profile-fdefinition ,C ]
       [ "Profile Package"         slclj-profile-package ,C]
       [ "Profile by Substring"    slclj-profile-by-substring ,C ]
       [ "Unprofile All"           slclj-unprofile-all ,C ]
       [ "Show Profiled"           slclj-profiled-functions ,C ]
       "--"
       [ "Report"                  slclj-profile-report ,C ]
       [ "Reset Counters"          slclj-profile-reset ,C ])
      ("Documentation"
       [ "Describe Symbol..."      slclj-describe-symbol ,C ]
       [ "Lookup Documentation..." slclj-documentation-lookup t ]
       [ "Apropos..."              slclj-apropos ,C ]
       [ "Apropos all..."          slclj-apropos-all ,C ]
       [ "Apropos Package..."      slclj-apropos-package ,C ]
       [ "Hyperspec..."            slclj-hyperspec-lookup t ])
      "--"
      [ "Interrupt Command"        slclj-interrupt ,C ]
      [ "Abort Async. Command"     slclj-quit ,C ]
      [ "Sync Package & Directory" slclj-sync-package-and-default-directory ,C]
      )))

(defvar slclj-sldb-easy-menu
  (let ((C '(slclj-connected-p)))
    `("SLDB"
      [ "Next Frame" sldb-down t ]
      [ "Previous Frame" sldb-up t ]
      [ "Toggle Frame Details" sldb-toggle-details t ]
      [ "Next Frame (Details)" sldb-details-down t ]
      [ "Previous Frame (Details)" sldb-details-up t ]
      "--"
      [ "Eval Expression..." slclj-interactive-eval ,C ]
      [ "Eval in Frame..." sldb-eval-in-frame ,C ]
      [ "Eval in Frame (pretty print)..." sldb-pprint-eval-in-frame ,C ]
      [ "Inspect In Frame..." sldb-inspect-in-frame ,C ]
      [ "Inspect Condition Object" sldb-inspect-condition ,C ]
      "--"
      [ "Restart Frame" sldb-restart-frame ,C ]
      [ "Return from Frame..." sldb-return-from-frame ,C ]
      ("Invoke Restart"
       [ "Continue" sldb-continue ,C ]
       [ "Abort"    sldb-abort ,C ]
       [ "Step"      sldb-step ,C ]
       [ "Step next" sldb-next ,C ]
       [ "Step out"  sldb-out ,C ]
       )
      "--"
      [ "Quit (throw)" sldb-quit ,C ]
      [ "Break With Default Debugger" sldb-break-with-default-debugger ,C ])))

(easy-menu-define menubar-slclj slclj-mode-map "SLCLJ" slclj-easy-menu)

(defun slclj-add-easy-menu ()
  (easy-menu-add slclj-easy-menu 'slclj-mode-map))

(add-hook 'slclj-mode-hook 'slclj-add-easy-menu)

(defun slclj-sldb-add-easy-menu ()
  (easy-menu-define menubar-slclj-sldb 
    sldb-mode-map "SLDB" slclj-sldb-easy-menu)
  (easy-menu-add slclj-sldb-easy-menu 'sldb-mode-map))

(add-hook 'sldb-mode-hook 'slclj-sldb-add-easy-menu)


;;;; Cheat Sheet

(defvar slclj-cheat-sheet-table
  '((:title "Editing lisp code"
     :map slclj-mode-map
     :bindings ((slclj-eval-defun "Evaluate current top level form")
                (slclj-compile-defun "Compile current top level form")
                (slclj-interactive-eval "Prompt for form and eval it")
                (slclj-compile-and-load-file "Compile and load current file")
                (slclj-sync-package-and-default-directory "Synch default package and directory with current buffer")
                (slclj-next-note "Next compiler note")
                (slclj-previous-note "Previous compiler note")
                (slclj-remove-notes "Remove notes")
                slclj-documentation-lookup))
    (:title "Completion"
     :map slclj-mode-map
     :bindings (slclj-indent-and-complete-symbol
                slclj-fuzzy-complete-symbol))
    (:title "Within SLDB buffers" 
     :map sldb-mode-map
     :bindings ((sldb-default-action "Do 'whatever' with thing at point")
                (sldb-toggle-details "Toggle frame details visualization")
                (sldb-quit "Quit to REPL")
                (sldb-abort "Invoke ABORT restart")
                (sldb-continue "Invoke CONTINUE restart (if available)")
                (sldb-show-source "Jump to frame's source code")
                (sldb-eval-in-frame "Evaluate in frame at point")
                (sldb-inspect-in-frame "Evaluate in frame at point and inspect result")))
    (:title "Within the Inspector" 
     :map slclj-inspector-mode-map
     :bindings ((slclj-inspector-next-inspectable-object "Jump to next inspectable object")
                (slclj-inspector-operate-on-point "Inspect object or execute action at point")
                (slclj-inspector-reinspect "Reinspect current object")
                (slclj-inspector-pop "Return to previous object")
                ;;(slclj-inspector-copy-down "Send object at point to REPL")
                (slclj-inspector-toggle-verbose "Toggle verbose mode")
                (slclj-inspector-quit "Quit")))
    (:title "Finding Definitions"
     :map slclj-mode-map
     :bindings (slclj-edit-definition
                slclj-pop-find-definition-stack))))

(defun slclj-cheat-sheet ()
  (interactive)
  (switch-to-buffer-other-frame (get-buffer-create "*SLCLJ Cheat Sheet*"))
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (goto-char (point-min))
  (insert "SLCLJ: The Superior Lisp Interaction Mode for Emacs (minor-mode).\n\n")
  (dolist (mode slclj-cheat-sheet-table)
    (let ((title (getf mode :title))
          (mode-map (getf mode :map))
          (mode-keys (getf mode :bindings)))
      (insert title)
      (insert ":\n")
      (insert (make-string (1+ (length title)) ?-))
      (insert "\n")
      (let ((keys '())
            (descriptions '()))
        (dolist (func mode-keys)
          ;; func is eithor the function name or a list (NAME DESCRIPTION)
          (push (if (symbolp func)
                    (prin1-to-string func)
                    (second func))
                descriptions)
          (let ((all-bindings (where-is-internal (if (symbolp func)
                                                     func
                                                     (first func))
                                                 (symbol-value mode-map)))
                (key-bindings '()))
            (dolist (binding all-bindings)
              (when (and (vectorp binding)
                         (integerp (aref binding 0)))
                (push binding key-bindings)))
            (push (mapconcat 'key-description key-bindings " or ") keys)))
        (loop
           with key-length = (apply 'max (mapcar 'length keys))
           with desc-length = (apply 'max (mapcar 'length descriptions))
           for key in (nreverse keys)
           for desc in (nreverse descriptions)
           do (insert desc)
           do (insert (make-string (- desc-length (length desc)) ? ))
           do (insert " => ")
           do (insert (if (string= "" key)
                          "<not on any key>"
                          key))
           do (insert "\n")
           finally do (insert "\n")))))
  (setq buffer-read-only t)
  (goto-char (point-min)))


;;;; Test suite

(defstruct (slclj-test (:conc-name slclj-test.))
  name fname args doc inputs fails-for style)
  
(defvar slclj-tests '()
  "Names of test functions.")

(defvar slclj-test-debug-on-error nil
  "*When non-nil debug errors in test cases.")

(defvar slclj-total-tests nil
  "Total number of tests executed during a test run.")

(defvar slclj-failed-tests nil
  "Total number of failed tests during a test run.")

(defvar slclj-skipped-tests nil
  "Total number of skipped tests during a test run.")

(defvar slclj-expected-failures nil
  "Total number of expected failures during a test run")

(defvar slclj-test-buffer-name "*Tests*"
  "The name of the buffer used to display test results.")

(defvar slclj-lisp-under-test nil
  "The name of Lisp currently executing the tests.")

(defvar slclj-randomize-test-order t
  "*If t execute tests in random order.
If nil, execute them in definition order.")

;; dynamically bound during a single test
(defvar slclj-current-test)
(defvar slclj-unexpected-failures)


;;;;; Execution engine

(defun slclj-run-tests ()
  "Run the test suite.
The results are presented in an outline-mode buffer, with the tests
that succeeded initially folded away."
  (interactive)
  (assert (slclj-at-top-level-p) () "Pending RPCs or open debuggers.")
  (slclj-create-test-results-buffer)
  (unwind-protect
      (let ((slclj-repl-history-file 
             (expand-file-name "slclj-repl-history" (slclj-temp-directory)))
            (slclj-tests (if slclj-randomize-test-order
                             (slclj-shuffle-list slclj-tests)
                           slclj-tests)))
        (slclj-execute-tests))
    (pop-to-buffer slclj-test-buffer-name)
    (goto-char (point-min))
    (hide-body)
    ;; Expose failed tests
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (overlay-get o 'slclj-failed-test)
        (goto-char (overlay-start o))
        (show-subtree)))))

(defun slclj-run-one-test (name)
  "Ask for the name of a test and then execute the test."
  (interactive (list (slclj-read-test-name)))
  (let ((test (find name slclj-tests :key #'slclj-test.name)))
    (assert test)
    (let ((slclj-tests (list test)))
      (slclj-run-tests))))

(defun slclj-read-test-name ()
  (let ((alist (mapcar (lambda (test) 
                         (list (symbol-name (slclj-test.name test))))
                       slclj-tests)))
    (read (completing-read "Test: " alist nil t))))

(defun slclj-test-should-fail-p ()
  (member slclj-lisp-under-test (slclj-test.fails-for slclj-current-test)))

(defun slclj-shuffle-list (list)
  (let* ((len (length list))
         (taken (make-vector len nil))
         (result (make-vector len nil)))
    (dolist (e list)
      (while (let ((i (random len)))
               (cond ((aref taken i))
                     (t (aset taken i t)
                        (aset result i e)
                        nil)))))
    (append result '())))

(defun slclj-execute-tests ()
  "Execute each test case with each input.
Return the number of failed tests."
  (save-window-excursion
    (let ((slclj-total-tests 0)
          (slclj-skipped-tests 0)
          (slclj-expected-passes 0)
          (slclj-unexpected-failures 0)
          (slclj-expected-failures 0)
          (slclj-lisp-under-test (slclj-lisp-implementation-name)))
      (dolist (slclj-current-test slclj-tests)
        (with-struct (slclj-test. name (function fname) inputs style) 
            slclj-current-test
          (if (and style (not (memq (slclj-communication-style) style)))
              (incf slclj-skipped-tests)  
            (slclj-test-heading 1 "%s" name)
            (dolist (input inputs)
              (incf slclj-total-tests)
              (message "%s: %s" name input)
              (slclj-test-heading 2 "input: %s" input)
              (if slclj-test-debug-on-error
                  (let ((debug-on-error t)
                        (debug-on-quit t))
                    (catch 'skip
                      (apply function input)))
                  (condition-case err
                      (apply function input)
                    (error
                     (cond ((slclj-test-should-fail-p)
                            (incf slclj-expected-failures)
                            (slclj-test-failure "ERROR (expected)"
                                                (format "%S" err)))
                           (t
                            (incf slclj-unexpected-failures)
                            (slclj-print-check-error err))))))))))
      (let ((summary 
             (concat (if (and (zerop slclj-expected-failures)
                              (zerop slclj-unexpected-failures))
                         (format "All %d tests completed successfully."
                                 slclj-total-tests)
                         (format "Failed on %d (%d expected) of %d tests."
                                 (+ slclj-expected-failures
                                    slclj-unexpected-failures)
                                 slclj-expected-failures
                                 slclj-total-tests))
                     (if (zerop slclj-skipped-tests)
                         ""
                         (format " Skipped %d tests." slclj-skipped-tests)))))
        (save-excursion
          (with-current-buffer slclj-test-buffer-name
            (goto-char (point-min))
            (insert summary "\n\n")))
        (message "%s" summary)
        slclj-unexpected-failures))))

(defun slclj-batch-test (results-file &optional test-name randomize)
  "Run the test suite in batch-mode.
Exits Emacs when finished. The exit code is the number of failed tests."
  (let ((slclj-test-debug-on-error nil))
    (slclj)
    ;; Block until we are up and running.
    (let* ((timeout 30)
           (cell (cons nil nil))
           (timer (run-with-timer timeout nil (lambda (cell) 
                                                (setcar cell t))
                                  cell)))
      (while (not (slclj-connected-p))
        (sit-for 1)
        (when (car cell)
          (with-temp-file results-file 
            (insert (format "TIMEOUT: Failed to connect within %s seconds."
                            timeout)))
          (kill-emacs 252))))
    (slclj-sync-to-top-level 5)
    (switch-to-buffer "*scratch*")
    (let* ((slclj-randomize-test-order (when randomize (random t) t))
           (failed-tests (cond (test-name (slclj-run-one-test test-name))
                               (t (slclj-run-tests)))))
      (with-current-buffer slclj-test-buffer-name
        (slclj-delete-hidden-outline-text)
        (goto-char (point-min))
        (insert "-*- outline -*-\n\n")
        (write-file results-file))
      (kill-emacs failed-tests))))


;;;;; Results buffer creation and output

(defun slclj-create-test-results-buffer ()
  "Create and initialize the buffer for test suite results."
  (ignore-errors (kill-buffer slclj-test-buffer-name))
  (with-current-buffer (get-buffer-create slclj-test-buffer-name)
    (erase-buffer)
    (outline-mode)
    (set (make-local-variable 'outline-regexp) "\\*+")
    (slclj-set-truncate-lines)))

(defun slclj-delete-hidden-outline-text ()
  "Delete the hidden parts of an outline-mode buffer."
  (loop do (when (eq (get-char-property (point) 'invisible) 'outline)
             (delete-region (point)
                            (next-single-char-property-change (point)
                                                              'invisible)))
        until (eobp)
        do (goto-char (next-single-char-property-change (point) 'invisible))))

(defun slclj-test-heading (level format &rest args)
  "Output a test suite heading.
LEVEL gives the depth of nesting: 1 for top-level, 2 for a subheading, etc."
  (with-current-buffer slclj-test-buffer-name
    (goto-char (point-max))
    (insert (make-string level ?*)
            " "
            (apply 'format format args)
            "\n")))

(defun slclj-test-failure (keyword string)
  "Output a failure message from the test suite.
KEYWORD names the type of failure and STRING describes the reason."
  (with-current-buffer slclj-test-buffer-name
    (goto-char (point-max))
    (let ((start (point)))
      (insert keyword ": ")
      (let ((overlay (make-overlay start (point))))
        (overlay-put overlay 'slclj-failed-test t)
        (overlay-put overlay 'face 'bold)))
    (insert string "\n")))

(defun slclj-test-message (string)
  "Output a message from the test suite."
  (with-current-buffer slclj-test-buffer-name
    (goto-char (point-max))
    (insert string "\n")))


;;;;; Macros for defining test cases

(defmacro def-slclj-test (name args doc inputs &rest body)
  "Define a test case.
NAME    ::= SYMBOL | (SYMBOL OPTION*) is a symbol naming the test.
OPTION  ::= (:fails-for IMPLEMENTATION*) | (:style COMMUNICATION-STYLE*)
ARGS is a lambda-list.
DOC is a docstring.
INPUTS is a list of argument lists, each tested separately.
BODY is the test case. The body can use `slclj-check' to test
conditions (assertions)."
  (multiple-value-bind (name fails-for style)
      (etypecase name
        (symbol (values name nil nil))
        (cons   (let* ((opts  (rest name))
                       (name  (first name))
                       (fails-for (cdr (assq :fails-for opts)))
                       (style (cdr (assq :style opts))))
                  ;; :style and :fails-for only options, given no more than one time?
                  (assert (null (remove* :style (remove* :fails-for opts :key #'car)
                                         :key #'car)))
                  (values name fails-for style))))
    (let ((fname (intern (format "slclj-test-%s" name))))
      `(progn
         (defun ,fname ,args
           ,doc
           (slclj-sync-to-top-level 0.3)
           ,@body
           (slclj-sync-to-top-level 0.3))
         (setq slclj-tests 
               (append (remove* ',name slclj-tests :key 'slclj-test.name)
                       (list (make-slclj-test :name ',name :fname ',fname
                                              :fails-for ',fails-for
                                              :style ',style
                                              :inputs ,inputs))))))))

(put 'def-slclj-test 'lisp-indent-function 4)

(defmacro slclj-check (test-name &rest body)
  "Check a condition (assertion.)
TEST-NAME can be a symbol, a string, or a (FORMAT-STRING . ARGS) list.
BODY returns true if the check succeeds."
  (let ((check-name (gensym "check-name-")))
    `(let ((,check-name ,(typecase test-name
                           (symbol (symbol-name test-name))
                           (string test-name)
                           (cons `(format ,@test-name)))))
       (if (progn ,@body)
           (slclj-print-check-ok ,check-name)
         (cond ((slclj-test-should-fail-p)
                (incf slclj-expected-failures)
                (slclj-test-failure "FAIL (expected)" ,check-name))
               (t
                (incf slclj-unexpected-failures)
                (slclj-print-check-failed ,check-name)))
         (when slclj-test-debug-on-error
           (debug (format "Check failed: %S" ,check-name)))))))

(defun slclj-print-check-ok (test-name)
  (slclj-test-message (concat "OK: " test-name)))

(defun slclj-print-check-failed (test-name)
  (slclj-test-failure "FAILED" test-name))

(defun slclj-print-check-error (reason)
  (slclj-test-failure "ERROR" (format "%S" reason)))

(put 'slclj-check 'lisp-indent-function 1)


;;;;; Test case definitions

;; Clear out old tests.
(setq slclj-tests nil)

(defun slclj-check-top-level (&optional test-name)
  (slclj-accept-process-output nil 0.001)
  (slclj-check "At the top level (no debugging or pending RPCs)"
    (slclj-at-top-level-p)))

(defun slclj-at-top-level-p ()
  (and (not (sldb-get-default-buffer))
       (null (slclj-rex-continuations))))

(defun slclj-wait-condition (name predicate timeout)
  (let ((end (time-add (current-time) (seconds-to-time timeout))))
    (while (not (funcall predicate))
      (let ((now (current-time)))
        (message "waiting for condition: %s [%s.%06d]" name
                 (format-time-string "%H:%M:%S" now) (third now)))
      (cond ((time-less-p end (current-time))
             (error "Timeout waiting for condition: %S" name))
            (t
             ;; XXX if a process-filter enters a recursive-edit, we
             ;; hang forever
             (slclj-accept-process-output nil 0.1))))))

(defun slclj-sync-to-top-level (timeout)
  (slclj-wait-condition "top-level" #'slclj-at-top-level-p timeout))

;; XXX: unused function
(defun slclj-check-sldb-level (expected)
  (let ((sldb-level (when-let (sldb (sldb-get-default-buffer))
                      (with-current-buffer sldb
                        sldb-level))))
    (slclj-check ("SLDB level (%S) is %S" expected sldb-level)
      (equal expected sldb-level))))

(defun slclj-test-expect (name expected actual &optional test)
  (when (stringp expected) (setq expected (substring-no-properties expected)))
  (when (stringp actual)   (setq actual (substring-no-properties actual)))
  (slclj-check ("%s:\nexpected: [%S]\n  actual: [%S]" name expected actual)
    (funcall (or test #'equal) expected actual)))

(defun sldb-level ()
  (when-let (sldb (sldb-get-default-buffer))
    (with-current-buffer sldb
      sldb-level)))

(defun slclj-sldb-level= (level)
  (equal level (sldb-level)))

(defvar slclj-test-symbols
  '(("foobar") ("foo@bar") ("@foobar") ("foobar@") ("\\@foobar")
    ("|asdf||foo||bar|")
    ("\\#<Foo@Bar>")
    ("\\(setf\\ car\\)")))

(defun slclj-check-symbol-at-point (prefix symbol suffix)
  ;; We test that `slclj-symbol-at-point' works at every
  ;; character of the symbol name.
  (with-temp-buffer
    (lisp-mode)
    (insert prefix)
    (let ((start (point)))
      (insert symbol suffix)
      (dotimes (i (length symbol))
        (goto-char (+ start i))
        (slclj-test-expect (format "Check `%s' (at %d)..."
                                   (buffer-string) (point))
                           symbol
                           (slclj-symbol-at-point)
                           #'equal)))))

(def-slclj-test symbol-at-point.1 (sym)
    "Check that we can cope with idiosyncratic symbol names."
    slclj-test-symbols
  (slclj-check-symbol-at-point "" sym ""))

(def-slclj-test symbol-at-point.2 (sym)
  "fancy symbol-name _not_ at BOB/EOB"
  slclj-test-symbols
  (slclj-check-symbol-at-point "(foo " sym " bar)"))

(def-slclj-test symbol-at-point.3 (sym)
  "fancy symbol-name with leading ,"
  (remove-if (lambda (s) (eq (aref (car s) 0) ?@)) slclj-test-symbols)
  (slclj-check-symbol-at-point "," sym ""))

(def-slclj-test symbol-at-point.4 (sym)
  "fancy symbol-name with leading ,@"
  slclj-test-symbols
  (slclj-check-symbol-at-point ",@" sym ""))

(def-slclj-test symbol-at-point.5 (sym)
  "fancy symbol-name with leading `"
  slclj-test-symbols
  (slclj-check-symbol-at-point "`" sym ""))

(def-slclj-test symbol-at-point.6 (sym)
  "fancy symbol-name wrapped in ()"
  slclj-test-symbols
  (slclj-check-symbol-at-point "(" sym ")"))

(def-slclj-test symbol-at-point.7 (sym)
  "fancy symbol-name wrapped in #< {DEADBEEF}>"
  slclj-test-symbols
  (slclj-check-symbol-at-point "#<" sym " {DEADBEEF}>"))

;;(def-slclj-test symbol-at-point.8 (sym)
;;  "fancy symbol-name wrapped in #<>"
;;  slclj-test-symbols
;;  (slclj-check-symbol-at-point "#<" sym ">"))

(def-slclj-test symbol-at-point.9 (sym)
  "fancy symbol-name wrapped in #| ... |#"
  slclj-test-symbols
  (slclj-check-symbol-at-point "#|\n" sym "\n|#"))

(def-slclj-test symbol-at-point.10 (sym)
  "fancy symbol-name after #| )))(( |# (1)"
  slclj-test-symbols
  (slclj-check-symbol-at-point "#| )))(( #|\n" sym ""))

(def-slclj-test symbol-at-point.11 (sym)
  "fancy symbol-name after #| )))(( |# (2)"
  slclj-test-symbols
  (slclj-check-symbol-at-point "#| )))(( #|" sym ""))

(def-slclj-test symbol-at-point.12 (sym)
  "fancy symbol-name wrapped in \"...\""
  slclj-test-symbols
  (slclj-check-symbol-at-point "\"\n" sym "\"\n"))

(def-slclj-test symbol-at-point.13 (sym)
  "fancy symbol-name wrapped in \" )))(( \" (1)"
  slclj-test-symbols
  (slclj-check-symbol-at-point "\" )))(( \"\n" sym ""))

(def-slclj-test symbol-at-point.14 (sym)
  "fancy symbol-name wrapped in \" )))(( \" (1)"
  slclj-test-symbols
  (slclj-check-symbol-at-point "\" )))(( \"" sym ""))

(def-slclj-test symbol-at-point.15 (sym)
  "symbol-at-point after #."
  slclj-test-symbols
  (slclj-check-symbol-at-point "#." sym ""))

(def-slclj-test symbol-at-point.16 (sym)
  "symbol-at-point after #+"
  slclj-test-symbols
  (slclj-check-symbol-at-point "#+" sym ""))


(def-slclj-test sexp-at-point.1 (string)
  "symbol-at-point after #'"
  '(("foo")
    ("#:foo")
    ("#'foo")
    ("#'(lambda (x) x)"))
  (with-temp-buffer
    (lisp-mode)
    (insert string)
    (goto-char (point-min))
    (slclj-test-expect (format "Check sexp `%s' (at %d)..."
                               (buffer-string) (point))
                       string
                       (slclj-sexp-at-point)
                       #'equal)))

(def-slclj-test narrowing ()
    "Check that narrowing is properly sustained."
    '()
  (slclj-check-top-level)
  (let ((random-buffer-name (symbol-name (gensym)))
        (defun-pos) (tmpbuffer))
    (with-temp-buffer
      (dotimes (i 100) (insert (format ";;; %d. line\n" i)))
      (setq tmpbuffer (current-buffer))
      (setq defun-pos (point))
      (insert (concat "(defun __foo__ (x y)" "\n"
                      "  'nothing)"          "\n"))
      (dotimes (i 100) (insert (format ";;; %d. line\n" (+ 100 i))))
      (slclj-check "Checking that newly created buffer is not narrowed."
        (not (slclj-buffer-narrowed-p)))

      (goto-char defun-pos)
      (narrow-to-defun)
      (slclj-check "Checking that narrowing succeeded."
       (slclj-buffer-narrowed-p))

      (slclj-with-popup-buffer (random-buffer-name)
        (slclj-check ("Checking that we're in Slclj's temp buffer `%s'" random-buffer-name)
          (equal (buffer-name (current-buffer)) random-buffer-name)))
      (with-current-buffer random-buffer-name
        ;; Notice that we cannot quit the buffer within the the extent
        ;; of slclj-with-output-to-temp-buffer.
        (slclj-popup-buffer-quit t)) 
      (slclj-check ("Checking that we've got back from `%s'" random-buffer-name)
        (and (eq (current-buffer) tmpbuffer)
             (= (point) defun-pos)))
      
      (slclj-check "Checking that narrowing sustained after quitting Slclj's temp buffer."
        (slclj-buffer-narrowed-p))

      (let ((slclj-buffer-package "SWANK")
            (symbol '*buffer-package*))
        (slclj-edit-definition (symbol-name symbol))
        (slclj-check ("Checking that we've got M-. into swank.lisp. %S" symbol)
          (string= (file-name-nondirectory (buffer-file-name))
                   "swank.lisp"))
        (slclj-pop-find-definition-stack)
        (slclj-check ("Checking that we've got back.")
          (and (eq (current-buffer) tmpbuffer)
               (= (point) defun-pos)))

        (slclj-check "Checking that narrowing sustained after M-,"
          (slclj-buffer-narrowed-p)))
      )) 
  (slclj-check-top-level))

(def-slclj-test find-definition
    (name buffer-package snippet)
    "Find the definition of a function or macro in swank.lisp."
    '(("start-server" "SWANK" "(defun start-server ")
      ("swank::start-server" "CL-USER" "(defun start-server ")
      ("swank:start-server" "CL-USER" "(defun start-server "))
  (switch-to-buffer "*scratch*")        ; not buffer of definition
  (slclj-check-top-level)
  (let ((orig-buffer (current-buffer))
        (orig-pos (point))
        (enable-local-variables nil)    ; don't get stuck on -*- eval: -*-
        (slclj-buffer-package buffer-package))
    (slclj-edit-definition name)
    ;; Postconditions
    (slclj-check ("Definition of `%S' is in swank.lisp." name)
      (string= (file-name-nondirectory (buffer-file-name)) "swank.lisp"))
    (slclj-check "Definition now at point." (looking-at snippet))
    (slclj-pop-find-definition-stack)
    (slclj-check "Returning from definition restores original buffer/position."
      (and (eq orig-buffer (current-buffer))
           (= orig-pos (point)))))
    (slclj-check-top-level))

(def-slclj-test (find-definition.2 (:fails-for "allegro" "lispworks"))
    (buffer-content buffer-package snippet)
    "Check that we're able to find definitions even when
confronted with nasty #.-fu."
    '(("#.(prog1 nil (defvar *foobar* 42))

       (defun .foo. (x)
         (+ x #.*foobar*))

       #.(prog1 nil (makunbound '*foobar*))
       "
       "SWANK"
       "[ \t]*(defun .foo. "
       ))
  (let ((slclj-buffer-package buffer-package))
    (with-temp-buffer
      (insert buffer-content)
      (slclj-check-top-level)
      (slclj-eval 
       `(swank:compile-string-for-emacs
         ,buffer-content
         ,(buffer-name)
         ,0
         ,nil
         ,nil))
      (let ((bufname (buffer-name)))
        (slclj-edit-definition ".foo.")
        (slclj-check ("Definition of `.foo.' is in buffer `%s'." bufname)
          (string= (buffer-name) bufname))
        (slclj-check "Definition now at point." (looking-at snippet)))
      )))

(def-slclj-test complete-symbol
    (prefix expected-completions)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" (("cl:compile" "cl:compile-file" "cl:compile-file-pathname"
                      "cl:compiled-function" "cl:compiled-function-p" 
                      "cl:compiler-macro" "cl:compiler-macro-function")
                     "cl:compile"))
      ("cl:foobar" (nil ""))
      ("swank::compile-file" (("swank::compile-file" 
                               "swank::compile-file-for-emacs"
                               "swank::compile-file-if-needed"
                               "swank::compile-file-output"
                               "swank::compile-file-pathname")
                              "swank::compile-file"))
      ("cl:m-v-l" (nil "")))
  (let ((completions (slclj-simple-completions prefix)))
    (slclj-test-expect "Completion set" expected-completions completions)))

(def-slclj-test arglist
    ;; N.B. Allegro apparently doesn't return the default values of
    ;; optional parameters. Thus the regexp in the start-server
    ;; expected value. In a perfect world we'd find a way to smooth
    ;; over this difference between implementations--perhaps by
    ;; convincing Franz to provide a function that does what we want.
    (function-name expected-arglist)
    "Lookup the argument list for FUNCTION-NAME.
Confirm that EXPECTED-ARGLIST is displayed."
    '(("swank::operator-arglist" "(swank::operator-arglist name package)")
      ("swank::create-socket" "(swank::create-socket host port)")
      ("swank::emacs-connected" "(swank::emacs-connected)")
      ("swank::compile-string-for-emacs"
       "(swank::compile-string-for-emacs string buffer position filename policy)")
      ("swank::connection.socket-io"
       "(swank::connection.socket-io \\(struct\\(ure\\)?\\|object\\|instance\\|x\\))")
      ("cl:lisp-implementation-type" "(cl:lisp-implementation-type)")
      ("cl:class-name" 
       "(cl:class-name \\(class\\|object\\|instance\\|structure\\))"))
  (let ((arglist (slclj-eval `(swank:operator-arglist ,function-name 
                                                      "swank"))))
    (slclj-test-expect "Argument list is as expected"
                       expected-arglist (and arglist (downcase arglist))
                       (lambda (pattern arglist)
                         (and arglist (string-match pattern arglist))))))

(def-slclj-test (compile-defun (:fails-for "allegro" "lispworks" "clisp" "ccl"))
    (program subform)
    "Compile PROGRAM containing errors.
Confirm that SUBFORM is correctly located."
    '(("(defun cl-user::foo () (cl-user::bar))" (cl-user::bar))
      ("(defun cl-user::foo () 
          #\\space
          ;;Sdf              
          (cl-user::bar))"
       (cl-user::bar))
      ("(defun cl-user::foo () 
             #+(or)skipped
             #| #||#
                #||# |#
             (cl-user::bar))"
       (cl-user::bar))
      ("(defun cl-user::foo () 
           (list `(1 ,(random 10) 2 ,@(random 10) 3 ,(cl-user::bar))))"
       (cl-user::bar))
      ("(defun cl-user::foo ()
          \"\\\" bla bla \\\"\"
          (cl-user::bar))"
       (cl-user::bar))
      ("(defun cl-user::foo ()
          #.*log-events*
          (cl-user::bar))"
       (cl-user::bar))
      ("#.'(defun x () (/ 1 0))
        (defun foo () 
           (cl-user::bar))
        
        "
       (cl-user::bar))
      ("(defun foo ()
          #+#.'(:and) (/ 1 0))"
       (/ 1 0))
      )
  (slclj-check-top-level)    
  (with-temp-buffer
    (lisp-mode)
    (insert program)
    (let ((font-lock-verbose nil))
      (setq slclj-buffer-package ":swank")
      (slclj-compile-string (buffer-string) 1)
      (setq slclj-buffer-package ":cl-user")
      (slclj-sync-to-top-level 5)
      (goto-char (point-max))
      (slclj-previous-note)
      (slclj-check error-location-correct
        (equal (read (current-buffer)) subform))))
  (slclj-check-top-level))

(def-slclj-test (compile-file (:fails-for "allegro" "lispworks" "clisp"))
    (string)
    "Insert STRING in a file, and compile it."
    `((,(pp-to-string '(defun foo () nil))))
  (let ((filename "/tmp/slclj-tmp-file.lisp"))
    (with-temp-file filename
      (insert string))
    (let ((cell (cons nil nil)))
      (slclj-eval-async
       `(swank:compile-file-for-emacs ,filename nil)
       (slclj-rcurry (lambda (result cell)
                       (setcar cell t)
                       (setcdr cell result))
                     cell))
      (slclj-wait-condition "Compilation finished" (lambda () (car cell))
                            0.5)
      (let ((result (cdr cell)))
        (slclj-check "Compilation successfull" 
          (eq (slclj-compilation-result.successp result) t))))))

(def-slclj-test async-eval-debugging (depth)
  "Test recursive debugging of asynchronous evaluation requests."
  '((1) (2) (3))
  (lexical-let ((depth depth)
                (debug-hook-max-depth 0))
    (let ((debug-hook
           (lambda ()
             (with-current-buffer (sldb-get-default-buffer)
               (when (> sldb-level debug-hook-max-depth)
                 (setq debug-hook-max-depth sldb-level)
                 (if (= sldb-level depth)
                     ;; We're at maximum recursion - time to unwind
                     (sldb-quit)
                   ;; Going down - enter another recursive debug
                   ;; Recursively debug.
                   (slclj-eval-async '(error))))))))
      (let ((sldb-hook (cons debug-hook sldb-hook)))
        (slclj-eval-async '(error))
        (slclj-sync-to-top-level 5)
        (slclj-check ("Maximum depth reached (%S) is %S."
                      debug-hook-max-depth depth)
          (= debug-hook-max-depth depth))))))

(def-slclj-test unwind-to-previous-sldb-level (level2 level1)
  "Test recursive debugging and returning to lower SLDB levels."
  '((2 1) (4 2))
  (slclj-check-top-level)
  (lexical-let ((level2 level2)
                (level1 level1)
                (state 'enter)
                (max-depth 0))
    (let ((debug-hook
           (lambda ()
             (with-current-buffer (sldb-get-default-buffer)
               (setq max-depth (max sldb-level max-depth))
               (ecase state
                 (enter
                  (cond ((= sldb-level level2)
                         (setq state 'leave)
                         (sldb-invoke-restart (sldb-first-abort-restart)))
                        (t
                         (slclj-eval-async `(cl:aref cl:nil ,sldb-level)))))
                 (leave
                  (cond ((= sldb-level level1)
                         (setq state 'ok)
                         (sldb-quit))
                        (t
                         (sldb-invoke-restart (sldb-first-abort-restart))
                         ))))))))
      (let ((sldb-hook (cons debug-hook sldb-hook)))
        (slclj-eval-async `(cl:aref cl:nil 0))
        (slclj-sync-to-top-level 15)
        (slclj-check-top-level)
        (slclj-check ("Maximum depth reached (%S) is %S." max-depth level2)
          (= max-depth level2))
        (slclj-check ("Final state reached.")
          (eq state 'ok))))))

(defun sldb-first-abort-restart ()
  (let ((case-fold-search t))
    (position-if (lambda (x) (string-match "abort" (car x))) sldb-restarts)))

(def-slclj-test loop-interrupt-quit
    ()
    "Test interrupting a loop."
    '(())
  (slclj-check-top-level)
  (slclj-eval-async '(cl:loop) (lambda (_) ) "CL-USER")
  (slclj-accept-process-output nil 1)
  (slclj-check "In eval state." (slclj-busy-p))
  (slclj-interrupt)
  (slclj-wait-condition "First interrupt" (lambda () (slclj-sldb-level= 1)) 5)
  (with-current-buffer (sldb-get-default-buffer) 
    (sldb-quit))
  (slclj-sync-to-top-level 5)
  (slclj-check-top-level))

(def-slclj-test loop-interrupt-continue-interrupt-quit
    ()
    "Test interrupting a previously interrupted but continued loop."
    '(())
  (slclj-check-top-level)
  (slclj-eval-async '(cl:loop) (lambda (_) ) "CL-USER")
  (sleep-for 1)
  (slclj-wait-condition "running" #'slclj-busy-p 5)
  (slclj-interrupt)
  (slclj-wait-condition "First interrupt" (lambda () (slclj-sldb-level= 1)) 5)
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-continue))
  (slclj-wait-condition "running" (lambda () 
                                    (and (slclj-busy-p)
                                         (not (sldb-get-default-buffer)))) 5)
  (slclj-interrupt)
  (slclj-wait-condition "Second interrupt" (lambda () (slclj-sldb-level= 1)) 5)
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-quit))
  (slclj-sync-to-top-level 5)
  (slclj-check-top-level))
 
(def-slclj-test interactive-eval 
    ()
    "Test interactive eval and continuing from the debugger."
    '(())
  (slclj-check-top-level)
  (lexical-let ((done nil))
    (let ((sldb-hook (lambda () (sldb-continue) (setq done t))))
      (slclj-interactive-eval 
       "(progn(cerror \"foo\" \"restart\")(cerror \"bar\" \"restart\")(+ 1 2))")
      (while (not done) (slclj-accept-process-output))
      (slclj-sync-to-top-level 5)
      (slclj-check-top-level)
      (unless noninteractive
        (let ((message (current-message)))
          (slclj-check "Minibuffer contains: \"3\""
            (equal "=> 3 (#x3, #o3, #b11)" message)))))))

(def-slclj-test interrupt-bubbling-idiot 
    ()
    "Test interrupting a loop that sends a lot of output to Emacs."
    '(())
  (slclj-accept-process-output nil 1)
  (slclj-check-top-level)
  (slclj-eval-async '(cl:loop :for i :from 0 :do (cl:progn (cl:print i) 
                                                           (cl:finish-output)))
                    (lambda (_) ) 
                    "CL-USER")
  (sleep-for 1)
  (slclj-interrupt)
  (slclj-wait-condition "Debugger visible" 
                        (lambda () 
                          (and (slclj-sldb-level= 1)
                               (get-buffer-window (sldb-get-default-buffer))))
                        30)
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-quit))
  (slclj-sync-to-top-level 5))

(def-slclj-test inspector
    (exp)
    "Test basic inspector workingness."
    '(((let ((h (make-hash-table)))
         (loop for i below 10 do (setf (gethash i h) i))
         h))
      ((make-array 10))
      ((make-list 10))
      ('cons)
      (#'cons))
  (slclj-inspect (prin1-to-string exp))
  (assert (not (slclj-inspector-visible-p)))
  (slclj-wait-condition "Inspector visible" #'slclj-inspector-visible-p 5)
  (with-current-buffer (window-buffer (selected-window))
    (slclj-inspector-quit))
  (slclj-wait-condition "Inspector closed" 
                        (lambda () (not (slclj-inspector-visible-p)))
                        5)
  (slclj-sync-to-top-level 1))

(defun slclj-buffer-visible-p (name)
  (let ((buffer (window-buffer (selected-window))))
    (string-match name (buffer-name buffer))))

(defun slclj-inspector-visible-p ()
  (slclj-buffer-visible-p "\\*Slclj Inspector\\*" ))

(defun slclj-execute-as-command (name)
  "Execute `name' as if it was done by the user through the
Command Loop. Similiar to `call-interactively' but also pushes on
the buffer's undo-list."
  (undo-boundary)
  (call-interactively name))

(def-slclj-test macroexpand 
    (macro-defs bufcontent expansion1 search-str expansion2)
    "foo"
    '((("(defmacro qwertz (&body body) `(list :qwertz ',body))"
        "(defmacro yxcv (&body body) `(list :yxcv (qwertz ,@body)))")
       "(yxcv :A :B :C)"
       "(list :yxcv (qwertz :a :b :c))"
       "(qwertz"
       "(list :yxcv (list :qwertz '(:a :b :c)))"))
  (slclj-check-top-level)
  (setq slclj-buffer-package ":swank")
  (with-temp-buffer
    (lisp-mode)
    (dolist (def macro-defs) 
      (slclj-compile-string def 0)
      (slclj-sync-to-top-level 5))
    (insert bufcontent)
    (goto-char (point-min))
    (slclj-execute-as-command 'slclj-macroexpand-1)
    (slclj-wait-condition "Macroexpansion buffer visible" 
                          (lambda () 
                            (slclj-buffer-visible-p "*SLCLJ Macroexpansion*"))
                          5)
    (with-current-buffer (get-buffer "*SLCLJ Macroexpansion*")
      (slclj-test-expect "Initial macroexpansion is correct"
                         expansion1 
                         (downcase (buffer-string)))
      (search-forward search-str)
      (backward-up-list)
      (slclj-execute-as-command 'slclj-macroexpand-1-inplace)
      (slclj-sync-to-top-level 3)
      (slclj-test-expect "In-place macroexpansion is correct"
                         expansion2 
                         (downcase (buffer-string)))
      (slclj-execute-as-command 'slclj-macroexpand-undo)
      (slclj-test-expect "Expansion after undo is correct"
                         expansion1
                         (downcase (buffer-string)))))
    (setq slclj-buffer-package ":cl-user"))

(def-slclj-test indentation (buffer-content point-markers)
        "Check indentation update to work correctly."
    '(("
\(in-package :swank)

\(defmacro with-lolipop (&body body)
  `(progn ,@body))

\(defmacro lolipop (&body body)
  `(progn ,@body))

\(with-lolipop
  1
  2
  42)

\(lolipop
  1
  2
  23)
"
       ("23" "42")))
  (with-temp-buffer
    (lisp-mode)
    (slclj-lisp-mode-hook)
    (insert buffer-content)
    (slclj-compile-region (point-min) (point-max))
    (slclj-sync-to-top-level 3)
    (slclj-update-indentation)
    (slclj-sync-to-top-level 3)
    (dolist (marker point-markers)
      (search-backward marker)
      (beginning-of-defun)
      (indent-sexp))
    (slclj-test-expect "Correct buffer content"
                       buffer-content
                       (substring-no-properties (buffer-string)))))

(def-slclj-test break
    (times exp)
    "Test whether BREAK invokes SLDB."
    (let ((exp1 '(break)))
      `((1 ,exp1) (2 ,exp1) (3 ,exp1)))
  (slclj-accept-process-output nil 0.2)
  (slclj-check-top-level)
  (slclj-eval-async 
   `(cl:eval (cl:read-from-string 
              ,(prin1-to-string `(dotimes (i ,times) 
                                   ,exp 
                                   (swank::sleep-for 0.2))))))
  (dotimes (i times)
    (slclj-wait-condition "Debugger visible" 
                          (lambda () 
                            (and (slclj-sldb-level= 1)
                                 (get-buffer-window 
                                  (sldb-get-default-buffer))))
                          1)
    (with-current-buffer (sldb-get-default-buffer)
      (sldb-continue))
    (slclj-wait-condition "sldb closed" 
                          (lambda () (not (sldb-get-default-buffer)))
                          0.2))
  (slclj-sync-to-top-level 1))

(def-slclj-test (break2 (:fails-for "cmucl" "allegro" "ccl"))
    (times exp)
    "Backends should arguably make sure that BREAK does not depend
on *DEBUGGER-HOOK*."
    (let ((exp2 
           '(block outta
              (let ((*debugger-hook* (lambda (c h) (return-from outta 42))))
                (break)))))
      `((1 ,exp2) (2 ,exp2) (3 ,exp2)))
  (slclj-test-break times exp))

(def-slclj-test locally-bound-debugger-hook
    ()
    "Test that binding *DEBUGGER-HOOK* locally works properly."
    '(())
  (slclj-accept-process-output nil 1)
  (slclj-check-top-level)
  (slclj-compile-string
   (prin1-to-string `(defun cl-user::quux ()
                       (block outta
                         (let ((*debugger-hook*
                                (lambda (c hook)
                                  (declare (ignore c hook))
                                  (return-from outta 42))))
                           (error "FOO")))))
   0)
  (slclj-sync-to-top-level 2)
  (slclj-eval-async '(cl-user::quux))
  ;; FIXME: slclj-wait-condition returns immediately if the test returns true
  (slclj-wait-condition "Checking that Debugger does not popup" 
                        (lambda () 
                          (not (sldb-get-default-buffer)))
                        3)
  (slclj-sync-to-top-level 5))


(def-slclj-test interrupt-at-toplevel
    ()
    "Let's see what happens if we send a user interrupt at toplevel."
    '(())
  (slclj-check-top-level)
  (unless (and (eq (slclj-communication-style) :spawn)
               (not (featurep 'slclj-repl)))
    (slclj-interrupt)
    (slclj-wait-condition 
     "Debugger visible" 
     (lambda () 
       (and (slclj-sldb-level= 1)
            (get-buffer-window (sldb-get-default-buffer))))
     5)
    (with-current-buffer (sldb-get-default-buffer)
      (sldb-quit))
    (slclj-sync-to-top-level 5)))

(def-slclj-test interrupt-in-debugger (interrupts continues)
    "Let's see what happens if we interrupt the debugger.
INTERRUPTS ... number of nested interrupts
CONTINUES  ... how often the continue restart should be invoked"
    '((1 0) (2 1) (4 2))
  (slclj-check "No debugger" (not (sldb-get-default-buffer)))
  (when (and (eq (slclj-communication-style) :spawn)
             (not (featurep 'slclj-repl)))
    (slclj-eval-async '(swank::without-slclj-interrupts
                        (swank::receive)))
    (sit-for 0.2))
  (dotimes (i interrupts)
    (slclj-interrupt)
    (let ((level (1+ i)))
      (slclj-wait-condition (format "Debug level %d reachend" level)
                            (lambda () (equal (sldb-level) level))
                            2)))
  (dotimes (i continues)
    (with-current-buffer (sldb-get-default-buffer)
      (sldb-continue))
    (let ((level (- interrupts (1+ i))))
      (slclj-wait-condition (format "Return to debug level %d" level)
                            (lambda () (equal (sldb-level) level))
                            2)))
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-quit))
  (slclj-sync-to-top-level 1))

;;; FIXME: reconnection is broken since the recent io-redirection changes.    
(def-slclj-test (disconnect-one-connection (:style :spawn)) ()
    "`slclj-disconnect' should disconnect only the current connection"
    '(())
  (let ((connection-count (length slclj-net-processes))
        (old-connection slclj-default-connection)
        (slclj-connected-hook nil))
    (unwind-protect
         (let ((slclj-dispatching-connection 
                (slclj-connect "localhost" 
                               ;; Here we assume that the request will
                               ;; be evaluated in its own thread.
                               (slclj-eval `(swank:create-server 
                                             :port 0 ; use random port
                                             :style :spawn
                                             :dont-close nil)))))
           (slclj-sync-to-top-level 3)
           (slclj-disconnect)
           (slclj-test-expect "Number of connections must remane the same"
                              connection-count
                              (length slclj-net-processes)))
      (slclj-select-connection old-connection))))

(def-slclj-test disconnect-and-reconnect
    ()
    "Close the connetion.
Confirm that the subprocess continues gracefully.
Reconnect afterwards."
    '(())
  (slclj-check-top-level)
  (let* ((c (slclj-connection))
         (p (slclj-inferior-process c)))
    (with-current-buffer (process-buffer p)
      (erase-buffer))
    (delete-process c)
    (assert (equal (process-status c) 'closed) nil "Connection not closed")
    (slclj-accept-process-output nil 0.1)
    (assert (equal (process-status p) 'run) nil "Subprocess not running")
    (with-current-buffer (process-buffer p)
      (assert (< (buffer-size) 500) nil "Unusual output"))
    (slclj-inferior-connect p (slclj-inferior-lisp-args p))
    (lexical-let ((hook nil) (p p))
      (setq hook (lambda ()
                   (slclj-test-expect 
                    "We are connected again" p (slclj-inferior-process))
                   (remove-hook 'slclj-connected-hook hook)))
      (add-hook 'slclj-connected-hook hook)
      (slclj-wait-condition "Lisp restarted" 
                            (lambda () 
                              (not (member hook slclj-connected-hook)))
                            5))))
    

;;;; Utilities (no not Paul Graham style)

;;;; List frobbing

;; FIXME: Seems uncommon and less readable than loop.
(defun slclj-map-alist (car-fn cdr-fn alist)
  "Map over ALIST, calling CAR-FN on the car, and CDR-FN on the
cdr of each entry."
  (mapcar (lambda (entry)
            (cons (funcall car-fn (car entry))
                  (funcall cdr-fn (cdr entry))))
          alist))

;; XXX: unused function
(defun slclj-intersperse (element list)
  "Intersperse ELEMENT between each element of LIST."
  (if (null list) 
      '()
    (cons (car list)
          (mapcan (lambda (x) (list element x)) (cdr list)))))

;;; FIXME: this looks almost slclj `slclj-alistify', perhaps the two
;;;        functions can be merged.
(defun slclj-group-similar (similar-p list)
  "Return the list of lists of 'similar' adjacent elements of LIST.
The function SIMILAR-P is used to test for similarity.
The order of the input list is preserved."
  (if (null list)
      nil
    (let ((accumulator (list (list (car list)))))
      (dolist (x (cdr list))
        (if (funcall similar-p x (caar accumulator))
            (push x (car accumulator))
          (push (list x) accumulator)))
      (reverse (mapcar #'reverse accumulator)))))

(defun slclj-alistify (list key test)
  "Partition the elements of LIST into an alist.
KEY extracts the key from an element and TEST is used to compare
keys."
  (declare (type function key))
  (let ((alist '()))
    (dolist (e list)
      (let* ((k (funcall key e))
	     (probe (assoc* k alist :test test)))
	(if probe
	    (push e (cdr probe))
            (push (cons k (list e)) alist))))
    ;; Put them back in order.
    (loop for (key . value) in (reverse alist)
          collect (cons key (reverse value)))))

;;;;; Misc.

(defun slclj-length= (seq n)
  "Return (= (length SEQ) N)."
  (etypecase seq
    (list
     (cond ((zerop n) (null seq))
           ((let ((tail (nthcdr (1- n) seq)))
              (and tail (null (cdr tail)))))))
    (sequence
     (= (length seq) n))))

(defun slclj-length> (seq n)
  "Return (> (length SEQ) N)."
  (etypecase seq
    (list (nthcdr n seq))
    (sequence (> (length seq) n))))

(defun slclj-trim-whitespace (str)
  (save-match-data
    (string-match "^\\s-*\\(.*?\\)\\s-*$" str)
    (match-string 1 str)))

;;;;; Buffer related

(defun slclj-buffer-narrowed-p (&optional buffer)
  "Returns T if BUFFER (or the current buffer respectively) is narrowed."
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

(defun slclj-column-max ()
  (save-excursion
    (goto-char (point-min))
    (loop for column = (prog2 (end-of-line) (current-column) (forward-line))
          until (= (point) (point-max))
          maximizing column)))

(defun slclj-inside-string-p ()
  (nth 3 (slclj-current-parser-state)))

(defun slclj-inside-comment-p ()
  (nth 4 (slclj-current-parser-state)))

(defun slclj-inside-string-or-comment-p ()
  (let ((state (slclj-current-parser-state)))
    (or (nth 3 state) (nth 4 state))))



;;;;; CL symbols vs. Elisp symbols.

(defun slclj-cl-symbol-name (symbol)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match ":\\([^:]*\\)$" n)
	(let ((symbol-part (match-string 1 n)))
          (if (string-match "^|\\(.*\\)|$" symbol-part)
              (match-string 1 symbol-part)
              symbol-part))
      n)))

(defun slclj-cl-symbol-package (symbol &optional default)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match "^\\([^:]*\\):" n)
	(match-string 1 n)
      default)))

(defun slclj-qualify-cl-symbol-name (symbol-or-name)
  "Return a package-qualified string for SYMBOL-OR-NAME.
If SYMBOL-OR-NAME doesn't already have a package prefix the
current package is used."
  (let ((s (if (stringp symbol-or-name)
               symbol-or-name
             (symbol-name symbol-or-name))))
    (if (slclj-cl-symbol-package s)
        s
      (format "%s::%s"
              (let* ((package (slclj-current-package)))
                ;; package is a string like ":cl-user" or "CL-USER", or "\"CL-USER\"".
                (if package
                    (slclj-pretty-package-name package)
                  "CL-USER"))
              (slclj-cl-symbol-name s)))))

;;;;; Moving, CL idiosyncracies aware (reader conditionals &c.)

(defmacro slclj-point-moves-p (&rest body)
  "Execute BODY and return true if the current buffer's point moved."
  (let ((pointvar (gensym "point-")))
    `(let ((,pointvar (point)))
       (save-current-buffer ,@body)
       (/= ,pointvar (point)))))

(put 'slclj-point-moves-p 'lisp-indent-function 0)

(defun slclj-forward-sexp (&optional count)
  "Like `forward-sexp', but understands reader-conditionals (#- and #+),
and skips comments."
  (dotimes (i (or count 1))
    (slclj-forward-cruft)
    (forward-sexp)))

(defconst slclj-reader-conditionals-regexp
  ;; #!+, #!- are SBCL specific reader-conditional syntax.
  ;; We need this for the source files of SBCL itself.
  (regexp-opt '("#+" "#-" "#!+" "#!-")))
 
(defun slclj-forward-reader-conditional ()
  "Move past any reader conditional (#+ or #-) at point."
  (when (looking-at slclj-reader-conditionals-regexp)
    (goto-char (match-end 0))
    (let* ((plus-conditional-p (eq (char-before) ?+))
           (result (slclj-eval-feature-expression 
                    (condition-case e
                        (read (current-buffer))
                      (invalid-read-syntax 
                       (signal 'slclj-unknown-feature-expression (cdr e)))))))
      (unless (if plus-conditional-p result (not result))
        ;; skip this sexp
        (slclj-forward-sexp)))))

(defun slclj-forward-cruft ()
  "Move forward over whitespace, comments, reader conditionals."
  (while (slclj-point-moves-p (skip-chars-forward "[:space:]")
                              (forward-comment (buffer-size))
                              (inline (slclj-forward-reader-conditional)))))

(defun slclj-keywordify (symbol)
  "Make a keyword out of the symbol SYMBOL."
  (let ((name (downcase (symbol-name symbol))))
    (intern (if (eq ?: (aref name 0)) 
                name 
              (concat ":" name)))))

(put 'slclj-incorrect-feature-expression
     'error-conditions '(slclj-incorrect-feature-expression error))

(put 'slclj-unknown-feature-expression
     'error-conditions '(slclj-unknown-feature-expression 
                         slclj-incorrect-feature-expression
                         error))

;; FIXME: let it crash
;; FIXME: the length=1 constraint is bogus
(defun slclj-eval-feature-expression (e)
  "Interpret a reader conditional expression."
  (cond ((symbolp e)
         (memq (slclj-keywordify e) (slclj-lisp-features)))
        ((and (consp e) (symbolp (car e)))
         (funcall (let ((head (slclj-keywordify (car e))))
                    (case head
                      (:and #'every)
                      (:or #'some)
                      (:not 
                         (lexical-let ((feature-expression e))
                           (lambda (f l) 
                             (cond 
                               ((slclj-length= l 0) t)
                               ((slclj-length= l 1) (not (apply f l)))
                               (t (signal 'slclj-incorrect-feature-expression 
                                          feature-expression))))))
                      (t (signal 'slclj-unknown-feature-expression head))))
                  #'slclj-eval-feature-expression
                  (cdr e)))
        (t (signal 'slclj-incorrect-feature-expression e))))

;;;;; Extracting Lisp forms from the buffer or user

(defun slclj-defun-at-point ()
  "Return the text of the defun at point."
  (apply #'buffer-substring-no-properties
         (slclj-region-for-defun-at-point)))

(defun slclj-region-for-defun-at-point ()
  "Return the start and end position of defun at point."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))

(defun slclj-beginning-of-symbol ()
  "Move to the beginning of the CL-style symbol at point."
  (while (re-search-backward "\\(\\sw\\|\\s_\\|\\s\\.\\|\\s\\\\|[#@|]\\)\\=" 
                             (when (> (point) 2000) (- (point) 2000))
                             t))
  (re-search-forward "\\=#[-+.<|]" nil t)
  (when (and (looking-at "@") (eq (char-before) ?\,))
    (forward-char)))

(defun slclj-end-of-symbol ()
  "Move to the end of the CL-style symbol at point."
  (re-search-forward "\\=\\(\\sw\\|\\s_\\|\\s\\.\\|#:\\|[@|]\\)*"))

(put 'slclj-symbol 'end-op 'slclj-end-of-symbol)
(put 'slclj-symbol 'beginning-op 'slclj-beginning-of-symbol)

(defun slclj-symbol-start-pos ()
  "Return the starting position of the symbol under point.
The result is unspecified if there isn't a symbol under the point."
  (save-excursion (slclj-beginning-of-symbol) (point)))

(defun slclj-symbol-end-pos ()
  (save-excursion (slclj-end-of-symbol) (point)))

(defun slclj-symbol-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  ;; (thing-at-point 'symbol) returns "" in empty buffers
  (let ((string (thing-at-point 'slclj-symbol)))
    (and string
         (not (equal string "")) 
         (substring-no-properties string))))

(defun slclj-sexp-at-point ()
  "Return the sexp at point as a string, otherwise nil."
  (or (slclj-symbol-at-point)
      (let ((string (thing-at-point 'sexp)))
        (if string (substring-no-properties string) nil))))

(defun slclj-sexp-at-point-or-error ()
  "Return the sexp at point as a string, othwise signal an error."
  (or (slclj-sexp-at-point) (error "No expression at point.")))

(defun slclj-string-at-point ()
  "Returns the string at point as a string, otherwise nil."
  (let ((sexp (slclj-sexp-at-point)))
    (if (eql (char-syntax (aref sexp 0)) ?\")
        sexp
        nil)))

(defun slclj-string-at-point-or-error ()
  "Return the sexp at point as a string, othwise signal an error."
  (or (slclj-string-at-point) (error "No string at point.")))

(defun slclj-input-complete-p (start end)
  "Return t if the region from START to END contains a complete sexp."
  (save-excursion
    (goto-char start)
    (cond ((looking-at "\\s *['`#]?[(\"]")
           (ignore-errors
             (save-restriction
               (narrow-to-region start end)
               ;; Keep stepping over blanks and sexps until the end of
               ;; buffer is reached or an error occurs. Tolerate extra
               ;; close parens.
               (loop do (skip-chars-forward " \t\r\n)")
                     until (eobp)
                     do (forward-sexp))
               t)))
          (t t))))


;;;; Portability library

(when (featurep 'xemacs)
  (require 'overlay))

(defun slclj-emacs-21-p ()
  (and (not (featurep 'xemacs))
       (= emacs-major-version 21)))

;; FIXME: not used here; move it away
(if (and (featurep 'emacs) (>= emacs-major-version 22))
    ;;  N.B. The 2nd, and 6th return value cannot be relied upon.
    (defsubst slclj-current-parser-state ()
      ;; `syntax-ppss' does not save match data as it invokes
      ;; `beginning-of-defun' implicitly which does not save match
      ;; data. This issue has been reported to the Emacs maintainer on
      ;; Feb27.
      (syntax-ppss))
    (defsubst slclj-current-parser-state ()
      (let ((original-pos (point)))
        (save-excursion
          (beginning-of-defun)
          (parse-partial-sexp (point) original-pos)))))

;;; `getf', `get', `symbol-plist' do not work on malformed plists
;;; on Emacs21. On later versions they do.
(when (slclj-emacs-21-p)
  ;; Perhaps we should rather introduce a new `slclj-getf' than
  ;; redefining. But what about (setf getf)? (A redefinition is not
  ;; necessary, except for consistency.)
  (defun getf (plist property &optional default)
    (loop for (prop . val) on plist 
          when (eq prop property) return (car val)
          finally (return default))))


(defun slclj-split-string (string &optional separators omit-nulls)
  "This is like `split-string' in Emacs22, but also works in 21."
  (let ((splits (split-string string separators)))
    (if omit-nulls
        (setq splits (remove "" splits))
      ;; SPLIT-STRING in Emacs before 22.x automatically removed nulls
      ;; at beginning and end, so we gotta add them here again.
      (when (slclj-emacs-21-p)
        (when (find (elt string 0) separators)
          (push "" splits))
        (when (find (elt string (1- (length string))) separators)
          (setq splits (append splits (list ""))))))
    splits))

(defun slclj-delete-and-extract-region (start end)
  "Like `delete-and-extract-region' except that it is guaranteed
to return a string. At least Emacs 21.3.50 returned `nil' on
\(delete-and-extract-region (point) (point)), this function
will return \"\"."
  (let ((result (delete-and-extract-region start end)))
    (if (null result)
        ""
      (assert (stringp result))
      result)))

(defmacro slclj-DEFUN-if-undefined (name &rest rest)
  ;; We can't decide at compile time whether NAME is properly
  ;; bound. So we delay the decision to runtime to ensure some
  ;; definition
  `(unless (fboundp ',name)
     (defun ,name ,@rest)))

(put 'slclj-DEFUN-if-undefined 'lisp-indent-function 2)
(put 'slclj-indulge-pretty-colors 'slclj-DEFUN-if-undefined t)

;; FIXME: defining macros here is probably too late for the compiler
(defmacro slclj-DEFMACRO-if-undefined (name &rest rest)
  `(unless (fboundp ',name)
     (defmacro ,name ,@rest)))

(put 'slclj-DEFMACRO-if-undefined 'lisp-indent-function 2)
(put 'slclj-indulge-pretty-colors 'slclj-DEFMACRO-if-undefined t)


(defvar slclj-accept-process-output-supports-floats 
  (ignore-errors (accept-process-output nil 0.0) t))

(defun slclj-accept-process-output (&optional process timeout)
  "Like `accept-process-output' but the TIMEOUT argument can be a float."
  (cond (slclj-accept-process-output-supports-floats
         (accept-process-output process timeout))
        (t
         (accept-process-output process 
                                (if timeout (truncate timeout))
                                ;; Emacs 21 uses microsecs; Emacs 22 millisecs
                                (if timeout (truncate (* timeout 1000000)))))))

(defun slclj-pop-to-buffer (buffer &optional other-window)
  "Select buffer BUFFER in some window.
This is like `pop-to-buffer' but also sets the input focus
for (somewhat) better multiframe support."
  (set-buffer buffer)
  (let ((old-frame (selected-frame))
        (window (display-buffer buffer other-window)))
    (select-window window)
    ;; select-window doesn't set the input focus
    (when (and (not (featurep 'xemacs))
               (>= emacs-major-version 22)
               (not (eq old-frame (selected-frame))))
      (select-frame-set-input-focus (window-frame window))))
  buffer)

(defun slclj-add-local-hook (hook function &optional append)
  (cond ((featurep 'xemacs) (add-local-hook hook function append))
        (t (add-hook hook function append t))))

(defun slclj-run-mode-hooks (&rest hooks)
  (if (fboundp 'run-mode-hooks) 
      (apply #'run-mode-hooks hooks)
    (apply #'run-hooks hooks)))

(if (featurep 'xemacs)
  (slclj-DEFUN-if-undefined line-number-at-pos (&optional pos)
     (line-number pos))
  (slclj-DEFUN-if-undefined line-number-at-pos (&optional pos)
     (save-excursion
       (when pos (goto-char pos))
       (1+ (count-lines 1 (point-at-bol))))))

(defun slclj-local-variable-p (var &optional buffer)
  (local-variable-p var (or buffer (current-buffer)))) ; XEmacs

(slclj-DEFUN-if-undefined region-active-p ()
  (and transient-mark-mode mark-active))

(if (featurep 'xemacs)
    (slclj-DEFUN-if-undefined use-region-p ()
      (region-active-p))
    (slclj-DEFUN-if-undefined use-region-p ()
      (and transient-mark-mode mark-active)))

(slclj-DEFUN-if-undefined next-single-char-property-change
    (position prop &optional object limit)
  (let ((limit (typecase limit
		 (null nil)
		 (marker (marker-position limit))
		 (t limit))))
    (if (stringp object)
	(or (next-single-property-change position prop object limit)
	    limit 
	    (length object))
      (with-current-buffer (or object (current-buffer))
	(let ((initial-value (get-char-property position prop object))
	      (limit (or limit (point-max))))
	  (loop for pos = position then 
                (next-single-property-change pos prop object limit)
		if (>= pos limit) return limit
		if (not (eq initial-value 
			    (get-char-property pos prop object))) 
		return pos))))))

(slclj-DEFUN-if-undefined previous-single-char-property-change 
    (position prop &optional object limit)
  (let ((limit (typecase limit
		 (null nil)
		 (marker (marker-position limit))
		 (t limit))))
    (if (stringp object)
	(or (previous-single-property-change position prop object limit)
	    limit 
	    (length object))
      (with-current-buffer (or object (current-buffer))
	(let ((limit (or limit (point-min))))
	  (if (<= position limit)
	      limit
            (let ((initial-value (get-char-property (1- position)
                                                    prop object)))
              (loop for pos = position then 
                    (previous-single-property-change pos prop object limit)
                    if (<= pos limit) return limit
                    if (not (eq initial-value 
                                (get-char-property (1- pos) prop object))) 
                    return pos))))))))

(slclj-DEFUN-if-undefined next-char-property-change (position &optional limit)
  (let ((tmp (next-overlay-change position)))
    (when tmp
      (setq tmp (min tmp limit)))
    (next-property-change position nil tmp)))

(slclj-DEFUN-if-undefined previous-char-property-change 
    (position &optional limit)
  (let ((tmp (previous-overlay-change position)))
    (when tmp
      (setq tmp (max tmp limit)))
    (previous-property-change position nil tmp)))
        
(slclj-DEFUN-if-undefined substring-no-properties (string &optional start end)
  (let* ((start (or start 0))
	 (end (or end (length string)))
	 (string (substring string start end)))
    (set-text-properties 0 (- end start) nil string)
    string))

(slclj-DEFUN-if-undefined match-string-no-properties (num &optional string)
  (if (match-beginning num)
      (if string
	  (substring-no-properties string (match-beginning num)
				   (match-end num))
	(buffer-substring-no-properties (match-beginning num)
                                        (match-end num)))))

(slclj-DEFUN-if-undefined set-window-text-height (window height)
  (let ((delta (- height (window-text-height window))))
    (unless (zerop delta)
      (let ((window-min-height 1))
	(if (and window (not (eq window (selected-window))))
	    (save-selected-window
	      (select-window window)
	      (enlarge-window delta))
	  (enlarge-window delta))))))

(slclj-DEFUN-if-undefined window-text-height (&optional window)
  (1- (window-height window)))

(slclj-DEFUN-if-undefined subst-char-in-string (fromchar tochar string 
						   &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))
                          
(slclj-DEFUN-if-undefined count-screen-lines 
  (&optional beg end count-final-newline window)
  (unless beg
    (setq beg (point-min)))
  (unless end
    (setq end (point-max)))
  (if (= beg end)
      0
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-region (min beg end)
                          (if (and (not count-final-newline)
                                   (= ?\n (char-before (max beg end))))
                              (1- (max beg end))
                            (max beg end)))
        (goto-char (point-min))
        ;; XXX make this xemacs compatible
        (1+ (vertical-motion (buffer-size) window))))))

(slclj-DEFUN-if-undefined seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to a time value."
  (list (floor seconds 65536)
	(floor (mod seconds 65536))
	(floor (* (- seconds (ffloor seconds)) 1000000))))

(slclj-DEFUN-if-undefined time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(slclj-DEFUN-if-undefined time-add (t1 t2)
  "Add two time values.  One should represent a time difference."
  (let ((high (car t1))
	(low (if (consp (cdr t1)) (nth 1 t1) (cdr t1)))
	(micro (if (numberp (car-safe (cdr-safe (cdr t1))))
		   (nth 2 t1)
		 0))
	(high2 (car t2))
	(low2 (if (consp (cdr t2)) (nth 1 t2) (cdr t2)))
	(micro2 (if (numberp (car-safe (cdr-safe (cdr t2))))
		    (nth 2 t2)
		  0)))
    ;; Add
    (setq micro (+ micro micro2))
    (setq low (+ low low2))
    (setq high (+ high high2))

    ;; Normalize
    ;; `/' rounds towards zero while `mod' returns a positive number,
    ;; so we can't rely on (= a (+ (* 100 (/ a 100)) (mod a 100))).
    (setq low (+ low (/ micro 1000000) (if (< micro 0) -1 0)))
    (setq micro (mod micro 1000000))
    (setq high (+ high (/ low 65536) (if (< low 0) -1 0)))
    (setq low (logand low 65535))

    (list high low micro)))

(slclj-DEFUN-if-undefined line-beginning-position (&optional n)
  (save-excursion
    (beginning-of-line n)
    (point)))

(slclj-DEFUN-if-undefined line-end-position (&optional n)
  (save-excursion
    (end-of-line n)
    (point)))

(slclj-DEFUN-if-undefined check-parens ()
    "Verify that parentheses in the current buffer are balanced.
If they are not, position point at the first syntax error found."
    (interactive)
    (let ((saved-point (point))
	  (state (parse-partial-sexp (point-min) (point-max) -1)))
      (destructuring-bind (depth innermost-start last-terminated-start
				 in-string in-comment after-quote 
				 minimum-depth comment-style 
				 comment-or-string-start &rest _) state
	(cond ((and (zerop depth) 
		    (not in-string) 
		    (or (not in-comment) 
			(and (eq comment-style nil) 
			     (eobp)))
		    (not after-quote))
	       (goto-char saved-point)
	       (message "All parentheses appear to be balanced."))
	      ((plusp depth)
	       (goto-char innermost-start)
	       (error "Missing )"))
	      ((minusp depth)
	       (error "Extra )"))
	      (in-string
	       (goto-char comment-or-string-start)
	       (error "String not terminated"))
	      (in-comment
	       (goto-char comment-or-string-start)
	       (error "Comment not terminated"))
	      (after-quote
	       (error "After quote"))
	      (t (error "Shouldn't happen: parsing state: %S" state))))))

(slclj-DEFUN-if-undefined read-directory-name (prompt 
                                               &optional dir default-dirname
                                               mustmatch initial)
  (unless dir
    (setq dir default-directory))
  (unless default-dirname
    (setq default-dirname
	  (if initial (concat dir initial) default-directory)))
  (let ((file (read-file-name prompt dir default-dirname mustmatch initial)))
    (setq file (file-name-as-directory (expand-file-name file)))
    (cond ((file-directory-p file)
           file)
          (t 
           (error "Not a directory: %s" file)))))

(slclj-DEFUN-if-undefined check-coding-system (coding-system)
  (or (eq coding-system 'binary)
      (error "No such coding system: %S" coding-system)))

(slclj-DEFUN-if-undefined process-coding-system (process)
  '(binary . binary))

(slclj-DEFUN-if-undefined set-process-coding-system 
    (process &optional decoding encoding))

;; For Emacs 21
(slclj-DEFUN-if-undefined display-warning
    (type message &optional level buffer-name)
  (with-output-to-temp-buffer "*Warnings*"
    (princ (format "Warning (%s %s): %s" type level message))))

(unless (boundp 'temporary-file-directory)
  (defvar temporary-file-directory
    (file-name-as-directory
     (cond ((memq system-type '(ms-dos windows-nt))
            (or (getenv "TEMP") (getenv "TMPDIR") (getenv "TMP") "c:/temp"))
           ((memq system-type '(vax-vms axp-vms))
            (or (getenv "TMPDIR") (getenv "TMP") 
                (getenv "TEMP") "SYS$SCRATCH:"))
           (t
            (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp"))))
    "The directory for writing temporary files."))

(slclj-DEFMACRO-if-undefined with-temp-message (message &rest body)
  (let ((current-message (make-symbol "current-message"))
        (temp-message (make-symbol "with-temp-message")))
    `(let ((,temp-message ,message)
           (,current-message))
       (unwind-protect
            (progn
              (when ,temp-message
                (setq ,current-message (current-message))
                (message "%s" ,temp-message))
              ,@body)
         (and ,temp-message ,current-message
              (message "%s" ,current-message))))))

(slclj-DEFMACRO-if-undefined with-selected-window (window &rest body)
  `(save-selected-window
     (select-window ,window)
     ,@body))


(when (featurep 'xemacs)
  (add-hook 'sldb-hook 'sldb-xemacs-emulate-point-entered-hook))

(defun sldb-xemacs-emulate-point-entered-hook ()
  (add-hook (make-local-variable 'post-command-hook)
            'sldb-xemacs-post-command-hook))

(defun sldb-xemacs-post-command-hook ()
  (when (get-text-property (point) 'point-entered)
    (funcall (get-text-property (point) 'point-entered))))

(when (slclj-emacs-21-p)
  ;; ?\@ is a prefix char from 22 onward, and
  ;; `slclj-symbol-at-point' was written with that assumption.
  (modify-syntax-entry ?\@ "'   " lisp-mode-syntax-table))


;;;; slclj.el in pretty colors

;;; You can use (put 'slclj-indulge-pretty-colors 'slclj-def-foo t) to
;;; have `slclj-def-foo' be fontified like `defun'.

(defun slclj-indulge-pretty-colors (def-foo-symbol)
  (let ((regexp (format "(\\(%S\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
                        def-foo-symbol)))
    (font-lock-add-keywords
     'emacs-lisp-mode
     `((,regexp (1 font-lock-keyword-face)
                (2 font-lock-variable-name-face))))))
 
(unless (featurep 'xemacs)
  (loop for (symbol flag) on (symbol-plist 'slclj-indulge-pretty-colors) by 'cddr
        when (eq flag 't) do (slclj-indulge-pretty-colors symbol)))
 
;;;; Finishing up

(require 'bytecomp)
(let ((byte-compile-warnings '()))
  (mapc #'byte-compile
        '(slclj-alistify
          slclj-log-event
          slclj-events-buffer
          ;;slclj-write-string 
          ;;slclj-repl-emit
          ;;slclj-output-buffer
          ;;slclj-connection-output-buffer
          ;;slclj-output-filter
          ;;slclj-repl-show-maximum-output
          slclj-process-available-input 
          slclj-dispatch-event 
          slclj-net-filter 
          slclj-net-have-input-p
          slclj-net-decode-length
          slclj-net-read
          slclj-print-apropos
          slclj-insert-propertized
          slclj-tree-insert
          slclj-symbol-constituent-at
          slclj-beginning-of-symbol
          slclj-end-of-symbol
          ;; Used implicitly during fontification:
          slclj-current-parser-state
          slclj-eval-feature-expression
          slclj-forward-sexp
          slclj-forward-cruft
          slclj-forward-reader-conditional
          )))

(provide 'slclj)
(run-hooks 'slclj-load-hook)

;; Local Variables: 
;; outline-regexp: ";;;;+"
;; indent-tabs-mode: nil
;; coding: latin-1-unix
;; compile-command: "emacs -batch -L . -eval '(byte-compile-file \"slclj.el\")' ; rm -v slclj.elc"
;; End:
;;; slclj.el ends here
