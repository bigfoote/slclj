;;; slclj-fancy.el --- Load and init some fancy SLCLJ contribs
;;
;; Authors: Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;; 
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slclj-load-hook (lambda () (require 'slclj-fancy)))
;;
;; We load all SLCLJ contribs that are currently working,
;; and which only "upgrade" the behavior of SLCLJ in some way.
;; This includes:
;;   * Adding new commands, keybindings, menu items
;;   * Making things clickable that would otherwise be just plain text

(require 'slclj-repl)
(slclj-repl-init)

;; Better arglist display, can be turned off by customization.
(unless (featurep 'xemacs)
  (require 'slclj-autodoc)
  (slclj-autodoc-init))

;; Adds new commands and installs compound-prefix-completion as
;; default completion command.  Behaves similar to standard Emacs
;; completion, unless dashes are present. --mkoeppe
(require 'slclj-c-p-c)
(slclj-c-p-c-init)

;; Just adds commands.  (Well, shadows commands in lisp-mode-map)
(require 'slclj-editing-commands)
(slclj-editing-commands-init)

;; Makes the inspector fancier.  (Once loaded, can't be turned off.)
(require 'slclj-fancy-inspector)
(slclj-fancy-inspector-init)

;; Just adds the command C-c M-i.  We do not make fuzzy completion the
;; default completion invoked by TAB. --mkoeppe
(require 'slclj-fuzzy)
(slclj-fuzzy-init)

;; Do not activate slclj-highlighting-edits by default, as it's easier
;; to explictly activate it (if a user really wants it) than to explictly
;; deactivate it once it got globally enabled. -TCR.
(require 'slclj-highlight-edits)
;(slclj-highlight-edits-init)

;; Load slclj-presentations even though they seem to be a
;; controversial feature, as they can be easily turned off by
;; customizing swank:*record-repl-results*. --mkoeppe
(require 'slclj-presentations)
(slclj-presentations-init)

;;; Do not load slclj-presentation-streams, as this is an experimental
;;; feature that installs patches into some Lisps. --mkoeppe
;;(require 'slclj-presentation-streams)

(require 'slclj-scratch)
(slclj-scratch-init)

;;; Do not load slclj-typeout-frame, as simply loading causes display of a
;;; typeout frame, which cannot be turned off. --mkoeppe
;;(require 'slclj-typeout-frame)

;; Just adds commands.
(when (locate-library "tree-widget")
  (require 'slclj-xref-browser))

;; Puts clickable references to documentation into SBCL errors.
(require 'slclj-references)
(slclj-references-init)

;;; Disabled -- after the removal of `slclj-enclosing-form-specs',
;;; this contrib has to be adapted.
;; Makes M-. work on local definitions, too.
;; (require 'slclj-mdot-fu)
;; (slclj-mdot-fu-init)

;; Add/Remove a symbol at point from the relevant DEFPACKAGE form
;; via C-c x.
(require 'slclj-package-fu)
(slclj-package-fu-init)

;; Fontify with-foo and do-foo like standard macros.
(require 'slclj-fontifying-fu)
(slclj-fontifying-fu-init)

(provide 'slclj-fancy)
