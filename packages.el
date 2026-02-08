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
(package! org-present
  :recipe (:host github :repo "rlister/org-present" :files ("*.el" "dist")))
(package! visual-fill-column
  :recipe (:host codeberg :repo "joostkremers/visual-fill-column" :files ("*.el" "dist")))
(package! org-ql)

