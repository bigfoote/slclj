;;; slclj-sprof.el --- Integration with SBCL's sb-sprof
;;;
;;; Authors: Juho Snellman
;;;
;;; License: MIT
;;;
;;; Installation
;;
;; Add this to your .emacs: 
;;
;;   (slclj-setup '(... slclj-sprof))

(defvar slclj-sprof-exclude-swank nil
  "*Display swank functions in the report.")

(define-derived-mode slclj-sprof-browser-mode fundamental-mode
  "slprof"
  "Mode for browsing profiler data\
\\<slclj-sprof-browser-mode-map>\
\\{slclj-sprof-browser-mode-map}"
  :syntax-table lisp-mode-syntax-table
  (setq buffer-read-only t))

(set-keymap-parent slclj-sprof-browser-mode-map slclj-parent-map)

(slclj-define-keys slclj-sprof-browser-mode-map
  ("h" 'describe-mode)
  ("q" 'bury-buffer)
  ("d" 'slclj-sprof-browser-disassemble-function)
  ("g" 'slclj-sprof-browser-go-to)
  ("v" 'slclj-sprof-browser-view-source)
  ("s" 'slclj-sprof-toggle-swank-exclusion)
  ((kbd "RET") 'slclj-sprof-browser-toggle))

;; Start / stop profiling

(defun slclj-sprof-start ()
  (interactive)
  (slclj-eval `(swank:swank-sprof-start)))

(defun slclj-sprof-stop ()
  (interactive)
  (slclj-eval `(swank:swank-sprof-stop)))

;; Reporting

(defun slclj-sprof-format (graph)
  (with-current-buffer (slclj-sprof-browser-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%4s %-54s %6s %6s %6s\n"
                      "Rank"
                      "Name"
                      "Self%"
                      "Cumul%"
                      "Total%"))
      (dolist (data graph)
        (slclj-sprof-browser-insert-line data 54))))
  (goto-line 2))

(defun* slclj-sprof-update (&optional (exclude-swank slclj-sprof-exclude-swank))
  (slclj-eval-async `(swank:swank-sprof-get-call-graph
                      :exclude-swank ,exclude-swank)
                    'slclj-sprof-format))

(defun slclj-sprof-browser ()
  (interactive)
  (switch-to-buffer (slclj-sprof-browser-buffer))
  (slclj-sprof-update))

(defun slclj-sprof-browser-buffer ()
  (if (get-buffer "*slclj-sprof-browser*")
      (get-buffer "*slclj-sprof-browser*")
      (let ((connection (slclj-connection)))
        (with-current-buffer (get-buffer-create "*slclj-sprof-browser*")
          (slclj-sprof-browser-mode)
          (setq slclj-buffer-connection connection)
          (current-buffer)))))

(defun slclj-sprof-toggle-swank-exclusion ()
  (interactive)
  (setq slclj-sprof-exclude-swank
        (not slclj-sprof-exclude-swank))
  (slclj-sprof-update))

(defun slclj-sprof-browser-insert-line (data name-length)
  (destructuring-bind (index name self cumul total)
      data
    (if index
        (insert (format "%-4d " index))
        (insert "     "))
    (slclj-insert-propertized
     (slclj-sprof-browser-name-properties)
     (format (format "%%-%ds " name-length)
             (abbreviate-name name name-length)))
    (insert (format "%6.2f " self))
    (when cumul
      (insert (format "%6.2f " cumul))
      (when total
        (insert (format "%6.2f" total))))
    (when index
      (slclj-sprof-browser-add-line-text-properties
       `(profile-index ,index expanded nil)))
    (insert "\n")))

(defun abbreviate-name (name max-length)
  (lexical-let ((length (min (length name) max-length)))
    (subseq name 0 length)))

;; Expanding / collapsing

(defun slclj-sprof-browser-toggle ()
  (interactive)
  (let ((index (get-text-property (point) 'profile-index)))
    (when index
      (save-excursion
        (if (slclj-sprof-browser-line-expanded-p)
            (slclj-sprof-browser-collapse)
            (slclj-sprof-browser-expand))))))

(defun slclj-sprof-browser-collapse ()
  (let ((inhibit-read-only t))
    (slclj-sprof-browser-add-line-text-properties '(expanded nil))
    (forward-line)
    (loop until (or (eobp)
                    (get-text-property (point) 'profile-index))
          do
          (delete-region (point-at-bol) (point-at-eol))
          (unless (eobp)
            (delete-char 1)))))

(defun slclj-sprof-browser-expand ()
  (lexical-let* ((buffer (current-buffer))
                 (point (point))
                 (index (get-text-property point 'profile-index)))
    (slclj-eval-async `(swank:swank-sprof-expand-node ,index)
                      (lambda (data)
                        (with-current-buffer buffer
                          (save-excursion 
                            (destructuring-bind (&key callers calls)
                                data
                              (slclj-sprof-browser-add-expansion callers
                                                                   "Callers"
                                                                   0)
                              (slclj-sprof-browser-add-expansion calls
                                                                   "Calls"
                                                                   0))))))))

(defun slclj-sprof-browser-add-expansion (data type nesting)
  (when data
    (let ((inhibit-read-only t))
      (slclj-sprof-browser-add-line-text-properties '(expanded t))
      (end-of-line)
      (insert (format "\n     %s" type))
      (dolist (node data)
        (destructuring-bind (index name cumul) node
          (insert (format (format "\n%%%ds" (+ 7 (* 2 nesting))) ""))
          (slclj-insert-propertized
           (slclj-sprof-browser-name-properties)
           (let ((len (- 59 (* 2 nesting))))
             (format (format "%%-%ds " len)
                     (abbreviate-name name len))))
          (slclj-sprof-browser-add-line-text-properties
           `(profile-sub-index ,index))
          (insert (format "%6.2f" cumul)))))))

(defun slclj-sprof-browser-line-expanded-p ()
  (get-text-property (point) 'expanded))

(defun slclj-sprof-browser-add-line-text-properties (properties)
  (add-text-properties (point-at-bol)
                       (point-at-eol)
                       properties))

(defun slclj-sprof-browser-name-properties ()
  '(face sldb-restart-number-face))

;; "Go to function"

(defun slclj-sprof-browser-go-to ()                                           
  (interactive)
  (let ((sub-index (get-text-property (point) 'profile-sub-index)))
    (when sub-index
      (let ((pos (text-property-any
                  (point-min) (point-max) 'profile-index sub-index)))
        (when pos (goto-char pos))))))

;; Disassembly

(defun slclj-sprof-browser-disassemble-function ()
  (interactive)
  (let ((index (or (get-text-property (point) 'profile-index)
                   (get-text-property (point) 'profile-sub-index))))
    (when index
      (slclj-eval-describe `(swank:swank-sprof-disassemble
                             ,index)))))

;; View source

(defun slclj-sprof-browser-view-source ()
  (interactive)
  (let ((index (or (get-text-property (point) 'profile-index)
                   (get-text-property (point) 'profile-sub-index))))
    (when index
      (slclj-eval-async
       `(swank:swank-sprof-source-location ,index)
       (lambda (source-location)
         (destructure-case source-location
           ((:error message)
            (message "%s" message)
            (ding))
           (t
            (slclj-show-source-location source-location))))))))

;;; Menu

(defun slclj-sprof-init ()
  (slclj-require :swank-sprof)
  (let ((C '(and (slclj-connected-p)
             (equal (slclj-lisp-implementation-type) "SBCL"))))
    (setf (cdr (last (assoc "Profiling" slclj-easy-menu)))
          `("--"
            [ "Start sb-sprof"  slclj-sprof-start ,C ]
            [ "Stop sb-sprof"   slclj-sprof-stop ,C ]
            [ "Report sb-sprof" slclj-sprof-browser ,C ]))))

(provide 'slclj-sprof)
