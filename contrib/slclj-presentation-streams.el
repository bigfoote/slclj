;;; swank-presentation-streams.el --- Streams that allow attaching object identities
;;;                                   to portions of output
;;;
;;; Authors: Alan Ruttenberg  <alanr-l@mumble.net>
;;;          Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;;          Helmut Eller  <heller@common-lisp.net>
;;;
;;; License: GNU GPL (same license as Emacs)
;;;
;;; Installation
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slclj-load-hook (lambda () (require 'slclj-presentation-streams)))
;;


;;; Initialization

(require 'slclj-presentations)

(defun slclj-presentation-streams-init ()
  (slclj-require :swank-presentation-streams))

(provide 'slclj-presentation-streams)

