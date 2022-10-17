;; [[file:../readme.org::*Writeroom][Writeroom:1]]
(use-package writeroom-mode
  :general
  (jtd/leader-key "Tw" 'writeroom-mode))
;; Writeroom:1 ends here

;; [[file:../readme.org::*Darkroom][Darkroom:1]]
(use-package darkroom
  :general
  (jtd/leader-key "Td" 'darkroom-tentative-mode))
;; Darkroom:1 ends here

;; [[file:../readme.org::*Spelling][Spelling:1]]
(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (ispell-program-name "aspell"))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
	      ("C-;" . flyspell-correct-wrapper)))
;; Spelling:1 ends here
