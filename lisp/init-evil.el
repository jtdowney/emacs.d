;; [[file:../readme.org::*evil mode][evil mode:1]]
(use-package evil
	:general
	(jtd/leader-key
		"wv" 'evil-window-vsplit
		"ws" 'evil-window-split)
	:custom
	((evil-want-integration t)
	 (evil-want-keybinding nil)
	 (evil-want-abbrev-expand-on-insert-exit nil)
	 (evil-respect-visual-line-mode t)
	 (evil-want-C-i-jump nil)
	 (evil-want-C-d-scroll t)
	 (evil-want-C-u-scroll t)
	 (evil-want-C-w-delete nil)
	 (evil-want-Y-yank-to-eol t)
	 ;; (evil-undo-system 'undo-fu)
	 (evil-search-module 'evil-search)  ;; enables gn
	 (evil-split-window-below t)
	 (evil-vsplit-window-right t)
	 (evil-auto-indent nil)
	 (evil-want-C-w-in-emacs-state t))
	:init
	(evil-mode 1)
	(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
	(define-key evil-motion-state-map "_" 'evil-end-of-line)
	(define-key evil-motion-state-map "0" 'evil-beginning-of-line)
	(evil-set-initial-state 'messages-buffer-mode 'normal)
	(evil-set-initial-state 'dashboard-mode 'normal))
;; evil mode:1 ends here

;; [[file:../readme.org::*evil-collection][evil-collection:1]]
(use-package evil-collection
	:after evil
	:config
	(evil-collection-init))
;; evil-collection:1 ends here

;; [[file:../readme.org::*Preview registers][Preview registers:1]]
(use-package evil-owl
	:defer 2
	:custom
	((evil-owl-max-string-length 500)
	 (evil-owl-display-method 'window))
	:config
	(evil-owl-mode 1))
;; Preview registers:1 ends here
