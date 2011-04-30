
;;; TODO: `url-http-file-exists-p' is slow, make it optional behaviour.

(require 'url-http)
(require 'browse-url)

(defun slclj-hyperdoc-lookup-rpc (symbol-name)
  (slclj-eval-async `(swank:hyperdoc ,symbol-name)
    (lexical-let ((symbol-name symbol-name))
      #'(lambda (result)
          (slclj-log-event result)
          (loop with foundp = nil
                for (doc-type . url) in result do
                (when (and url (stringp url)
                           (let ((url-show-status nil))
                             (url-http-file-exists-p url)))
                  (message "Visiting documentation for %s `%s'..."
                           (substring (symbol-name doc-type) 1)
                           symbol-name)
                  (browse-url url)
                  (setq foundp t))
                finally
                (unless foundp
                  (error "Could not find documentation for `%s'." 
                         symbol-name)))))))

(defun slclj-hyperdoc-lookup (symbol-name)
  (interactive (list (slclj-read-symbol-name "Symbol: ")))
  (if (memq :hyperdoc (slclj-lisp-features))
      (slclj-hyperdoc-lookup-rpc symbol-name)
      (slclj-hyperspec-lookup symbol-name)))

(defvar slclj-old-documentation-lookup-function 
  slclj-documentation-lookup-function)

(defun slclj-hyperdoc-init ()
  (slclj-require :swank-hyperdoc)
  (setq slclj-documentation-lookup-function 'slclj-hyperdoc-lookup))

(defun slclj-hyperdoc-unload ()
  (setq slclj-documentation-lookup-function 
        slclj-old-documentation-lookup-function))

(provide 'slclj-hyperdoc)