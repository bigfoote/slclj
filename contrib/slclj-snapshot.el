;; slclj-snapshot.el --- Save&restore memory images without disconnecting

(defun slclj-snapshot (filename)
  "Save a memory image to the file FILENAME."
  (interactive (list (read-file-name "Image file: ")))
  (slclj-eval-with-transcript 
   `(swank-snapshot:save-snapshot ,(expand-file-name filename))))

(defun slclj-restore (filename)
  "Restore a memory image stored in file FILENAME."
  (interactive (list (read-file-name "Image file: ")))
  ;; bypass event dispatcher because we don't expect a reply. FIXME.
  (slclj-net-send `(:emacs-rex (swank-snapshot:restore-snapshot 
				,(expand-file-name filename))
			       nil t nil)
		  (slclj-connection)))

(defun slclj-snapshot-init ()
  (slclj-require :swank-snapshot))