;;; slclj-autoloads.el --- autoload definitions for SLCLJ

;; Copyright (C) 2007  Helmut Eller

;; This file is protected by the GNU GPLv2 (or later), as distributed
;; with GNU Emacs.

;;; Commentary:

;; This code defines the necessary autoloads, so that we don't need to
;; load everything from .emacs.

;;; Code:

(autoload 'slclj "slclj"
  "Start a Lisp subprocess and connect to its Swank server." t) 

(autoload 'slclj-mode "slclj"
  "SLCLJ: The Superior Lisp Interaction (Minor) Mode for Emacs." t)

(autoload 'slclj-connect "slclj"
  "Connect to a running Swank server." t)

(autoload 'hyperspec-lookup "hyperspec" nil t)

(autoload 'slclj-lisp-mode-hook "slclj")
(autoload 'slclj-scheme-mode-hook "slclj")

(defvar slclj-lisp-modes '(lisp-mode))
(defvar slclj-setup-contribs nil
  "List of contribst to load.
Modified my slclj-setup.")

(defun slclj-setup (&optional contribs)
  "Setup Emacs so that lisp-mode buffers always use SLCLJ.
CONTRIBS is a list of contrib packages to load."
  (when (member 'lisp-mode slclj-lisp-modes)
    (add-hook 'lisp-mode-hook 'slclj-lisp-mode-hook))
  (setq slclj-setup-contribs contribs)
  (add-hook 'slclj-load-hook 'slclj-setup-contribs))

(provide 'slclj-autoloads)

;;; slclj-autoloads.el ends here
