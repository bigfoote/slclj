;; slclj-compiler-notes-tree.el --- Display compiler messages in tree layout.
;;
;; Author: Helmut Eller
;; License: GNU GPL (same license as Emacs)
;;
;;; Commentary:
;;
;; M-x slclj-list-compiler-notes display the compiler notes in a tree
;; grouped by severity.
;;
;; `slclj-maybe-list-compiler-notes' can be used as
;; `slclj-compilation-finished-hook'.

(defun slclj-maybe-list-compiler-notes (notes)
  "Show the compiler notes if appropriate."
  ;; don't pop up a buffer if all notes are already annotated in the
  ;; buffer itself
  (unless (every #'slclj-note-has-location-p notes)
    (slclj-list-compiler-notes notes)))

(defun slclj-list-compiler-notes (notes)
  "Show the compiler notes NOTES in tree view."
  (interactive (list (slclj-compiler-notes)))
  (with-temp-message "Preparing compiler note tree..."
    (slclj-with-popup-buffer ("*SLCLJ Compiler-Notes*")
      (erase-buffer)
      (slclj-compiler-notes-mode)
      (when (null notes)
        (insert "[no notes]"))
      (let ((collapsed-p))
        (dolist (tree (slclj-compiler-notes-to-tree notes))
          (when (slclj-tree.collapsed-p tree) (setf collapsed-p t))
          (slclj-tree-insert tree "")
          (insert "\n"))
        (goto-char (point-min))))))

(defvar slclj-tree-printer 'slclj-tree-default-printer)

(defun slclj-tree-for-note (note)
  (make-slclj-tree :item (slclj-note.message note)
                   :plist (list 'note note)
                   :print-fn slclj-tree-printer))

(defun slclj-tree-for-severity (severity notes collapsed-p)
  (make-slclj-tree :item (format "%s (%d)" 
                                 (slclj-severity-label severity)
                                 (length notes))
                   :kids (mapcar #'slclj-tree-for-note notes)
                   :collapsed-p collapsed-p))

(defun slclj-compiler-notes-to-tree (notes)
  (let* ((alist (slclj-alistify notes #'slclj-note.severity #'eq))
         (collapsed-p (slclj-length> alist 1)))
    (loop for (severity . notes) in alist
          collect (slclj-tree-for-severity severity notes 
                                           collapsed-p))))

(defvar slclj-compiler-notes-mode-map)

(define-derived-mode slclj-compiler-notes-mode fundamental-mode 
  "Compiler-Notes"
  "\\<slclj-compiler-notes-mode-map>\
\\{slclj-compiler-notes-mode-map}
\\{slclj-popup-buffer-mode-map}
"
  (slclj-set-truncate-lines))

(slclj-define-keys slclj-compiler-notes-mode-map
  ((kbd "RET") 'slclj-compiler-notes-default-action-or-show-details)
  ([return] 'slclj-compiler-notes-default-action-or-show-details)
  ([mouse-2] 'slclj-compiler-notes-default-action-or-show-details/mouse))

(defun slclj-compiler-notes-default-action-or-show-details/mouse (event)
  "Invoke the action pointed at by the mouse, or show details."
  (interactive "e")
  (destructuring-bind (mouse-2 (w pos &rest _) &rest __) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point) 
                                   'slclj-compiler-notes-default-action)))
	(if fn (funcall fn) (slclj-compiler-notes-show-details))))))

(defun slclj-compiler-notes-default-action-or-show-details ()
  "Invoke the action at point, or show details."
  (interactive)
  (let ((fn (get-text-property (point) 'slclj-compiler-notes-default-action)))
    (if fn (funcall fn) (slclj-compiler-notes-show-details))))

(defun slclj-compiler-notes-show-details ()
  (interactive)
  (let* ((tree (slclj-tree-at-point))
         (note (plist-get (slclj-tree.plist tree) 'note))
         (inhibit-read-only t))
    (cond ((not (slclj-tree-leaf-p tree))
           (slclj-tree-toggle tree))
          (t
           (slclj-show-source-location (slclj-note.location note) t)))))


;;;;;; Tree Widget

(defstruct (slclj-tree (:conc-name slclj-tree.))
  item
  (print-fn #'slclj-tree-default-printer :type function)
  (kids '() :type list)
  (collapsed-p t :type boolean)
  (prefix "" :type string)
  (start-mark nil)
  (end-mark nil)
  (plist '() :type list))

(defun slclj-tree-leaf-p (tree)
  (not (slclj-tree.kids tree)))

(defun slclj-tree-default-printer (tree)
  (princ (slclj-tree.item tree) (current-buffer)))

(defun slclj-tree-decoration (tree)
  (cond ((slclj-tree-leaf-p tree) "-- ")
	((slclj-tree.collapsed-p tree) "[+] ")
	(t "-+  ")))

(defun slclj-tree-insert-list (list prefix)
  "Insert a list of trees."
  (loop for (elt . rest) on list 
	do (cond (rest
		  (insert prefix " |")
		  (slclj-tree-insert elt (concat prefix " |"))
                  (insert "\n"))
		 (t
		  (insert prefix " `")
		  (slclj-tree-insert elt (concat prefix "  "))))))

(defun slclj-tree-insert-decoration (tree)
  (insert (slclj-tree-decoration tree)))

(defun slclj-tree-indent-item (start end prefix)
  "Insert PREFIX at the beginning of each but the first line.
This is used for labels spanning multiple lines."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (while (< start (point))
      (insert-before-markers prefix)
      (forward-line -1))))

(defun slclj-tree-insert (tree prefix)
  "Insert TREE prefixed with PREFIX at point."
  (with-struct (slclj-tree. print-fn kids collapsed-p start-mark end-mark) tree
    (let ((line-start (line-beginning-position)))
      (setf start-mark (point-marker))
      (slclj-tree-insert-decoration tree)
      (funcall print-fn tree)
      (slclj-tree-indent-item start-mark (point) (concat prefix "   "))
      (add-text-properties line-start (point) (list 'slclj-tree tree))
      (set-marker-insertion-type start-mark t)
      (when (and kids (not collapsed-p))
        (terpri (current-buffer))
        (slclj-tree-insert-list kids prefix))
      (setf (slclj-tree.prefix tree) prefix)
      (setf end-mark (point-marker)))))

(defun slclj-tree-at-point ()
  (cond ((get-text-property (point) 'slclj-tree))
        (t (error "No tree at point"))))

(defun slclj-tree-delete (tree)
  "Delete the region for TREE."
  (delete-region (slclj-tree.start-mark tree)
                 (slclj-tree.end-mark tree)))

(defun slclj-tree-toggle (tree)
  "Toggle the visibility of TREE's children."
  (with-struct (slclj-tree. collapsed-p start-mark end-mark prefix) tree
    (setf collapsed-p (not collapsed-p))
    (slclj-tree-delete tree)
    (insert-before-markers " ") ; move parent's end-mark
    (backward-char 1)
    (slclj-tree-insert tree prefix)
    (delete-char 1)
    (goto-char start-mark)))

(provide 'slclj-compiler-notes-tree)
