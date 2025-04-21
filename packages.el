;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! ox-hugo)
(package! gptel)
(package! whisper
 :recipe (:host github :repo "natrys/whisper.el" :files ("*.el" "dist")))
(package! copilot
 :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))
(package! emacs-reveal
 :recipe (:host gitlab :repo "oer/emacs-reveal" :files ("*.el" "dist")))
(package! org-special-block-extras)
;; (package! org-ql)

