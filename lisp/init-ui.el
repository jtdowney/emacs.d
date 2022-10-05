;; [[file:../readme.org::*theme][theme:1]]
(load-theme 'modus-vivendi t)
;; theme:1 ends here

;; [[file:../readme.org::*doom modeline][doom modeline:1]]
(use-package doom-modeline
	:demand
	:custom
	(doom-modeline-buffer-encoding nil)
	(doom-modeline-env-enable-python nil)
	(doom-modeline-height 15)
	(doom-modeline-project-detection 'projectile)
	:config
	(doom-modeline-mode 1))
;; doom modeline:1 ends here
