;;; slclj-references.el --- Clickable references to documentation (SBCL only)
;;
;; Authors: Christophe Rhodes  <csr21@cantab.net>
;;          Luke Gorrie  <luke@bluetail.com>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;
;;;

(defcustom slclj-sbcl-manual-root "http://www.sbcl.org/manual/"
  "*The base URL of the SBCL manual, for documentation lookup."
  :type 'string
  :group 'slclj-mode)

(defface sldb-reference-face 
  (list (list t '(:underline t)))
  "Face for references."
  :group 'slclj-debugger)


;;;;; SBCL-style references 

(defvar slclj-references-local-keymap
  (let ((map (make-sparse-keymap "local keymap for slclj references")))
    (define-key map [mouse-2] 'slclj-lookup-reference-at-mouse)
    (define-key map [return] 'slclj-lookup-reference-at-point)
    map))

(defun slclj-reference-properties (reference)
  "Return the properties for a reference.
Only add clickability to properties we actually know how to lookup."
  (destructuring-bind (where type what) reference
    (if (or (and (eq where :sbcl) (eq type :node))
            (and (eq where :ansi-cl)
                 (memq type '(:function :special-operator :macro
			      :section :glossary :issue))))
        `(slclj-reference ,reference
          font-lock-face sldb-reference-face
          follow-link t
          mouse-face highlight
          help-echo "mouse-2: visit documentation."
          keymap ,slclj-references-local-keymap))))

(defun slclj-insert-reference (reference)
  "Insert documentation reference from a condition.
See SWANK-BACKEND:CONDITION-REFERENCES for the datatype."
  (destructuring-bind (where type what) reference
    (insert "\n" (slclj-format-reference-source where) ", ")
    (slclj-insert-propertized (slclj-reference-properties reference)
                              (slclj-format-reference-node what))
    (insert (format " [%s]" type))))

(defun slclj-insert-references (references)
  (when references
    (insert "\nSee also:")
    (slclj-with-rigid-indentation 2
      (mapc #'slclj-insert-reference references))))

(defun slclj-format-reference-source (where)
  (case where
    (:amop    "The Art of the Metaobject Protocol")
    (:ansi-cl "Common Lisp Hyperspec")
    (:sbcl    "SBCL Manual")
    (t        (format "%S" where))))

(defun slclj-format-reference-node (what)
  (if (listp what)
      (mapconcat #'prin1-to-string what ".")
    what))

(defun slclj-lookup-reference-at-point ()
  "Browse the documentation reference at point."
  (interactive)
  (let ((refs (get-text-property (point) 'slclj-reference)))
    (if (null refs)
        (error "No references at point")
        (destructuring-bind (where type what) refs
          (case where
            (:ansi-cl
               (case type
                 (:section
                    (browse-url (funcall common-lisp-hyperspec-section-fun what)))
                 (:glossary
                    (browse-url (funcall common-lisp-glossary-fun what)))
                 (:issue
                    (browse-url (funcall 'common-lisp-issuex what)))
                 (t
                    (hyperspec-lookup what))))
            (t
               (let ((url (format "%s%s.html" slclj-sbcl-manual-root
                                  (subst-char-in-string ?\  ?\- what))))
                 (browse-url url))))))))

(defun slclj-lookup-reference-at-mouse (event)
  "Invoke the action pointed at by the mouse."
  (interactive "e")
  (destructuring-bind (mouse-1 (w pos . _) . _) event
    (save-excursion
      (goto-char pos)
      (slclj-lookup-reference-at-point))))

;;;;; Hook into *SLCLJ COMPILATION*

(defun slclj-note.references (note)
  (plist-get note :references))

;;; FIXME: `compilation-mode' will swallow the `mouse-face'
;;; etc. properties.
(defadvice slclj-note.message (after slclj-note.message+references)
  (setq ad-return-value 
        (concat ad-return-value
                (with-temp-buffer
                  (slclj-insert-references 
                   (slclj-note.references (ad-get-arg 0)))
                  (buffer-string)))))

;;;;; Hook into slclj-compiler-notes-tree

(defun slclj-tree-print-with-references (tree)
  ;; for SBCL-style references
  (slclj-tree-default-printer tree)
  (when-let (note (plist-get (slclj-tree.plist tree) 'note))
    (when-let (references (slclj-note.references note))
      (terpri (current-buffer))
      (slclj-insert-references references))))

;;;;; Hook into SLDB

(defun sldb-maybe-insert-references (extra)
  (destructure-case extra
    ((:references references) (slclj-insert-references references) t)
    (t nil)))


;;; Initialization

(defun slclj-references-init ()
  (ad-enable-advice 'slclj-note.message 'after 'slclj-note.message+references)
  (ad-activate 'slclj-note.message)
  (setq slclj-tree-printer 'slclj-tree-print-with-references)
  (add-hook 'sldb-extras-hooks 'sldb-maybe-insert-references))

(defun slclj-references-unload ()
  (ad-disable-advice 'slclj-note.message 'after 'slclj-note.message+references)
  (ad-deactivate 'slclj-note.message)
  (setq slclj-tree-printer 'slclj-tree-default-printer)
  (remove-hook 'sldb-extras-hooks 'sldb-maybe-insert-references))
  
(provide 'slclj-references)
