;; [[file:../readme.org::*Writeroom][Writeroom:1]]
(use-package writeroom-mode
	:general
	(jtd/leader-key "tw" 'writeroom-mode))
;; Writeroom:1 ends here

;; [[file:../readme.org::*Darkroom][Darkroom:1]]
(use-package darkroom
	:general
	(jtd/leader-key "td" 'darkroom-tentative-mode))
;; Darkroom:1 ends here

;; [[file:../readme.org::*Spelling][Spelling:1]]
(use-package flyspell
	:custom
	(ispell-program-name "aspell")
	:hook ((text-mode . flyspell-mode))
	(prog-mode . flyspell-prog-mode))

(use-package flyspell-correct
	:after flyspell
	:bind (:map flyspell-mode-map
							("C-;" . flyspell-correct-wrapper)))
;; Spelling:1 ends here
