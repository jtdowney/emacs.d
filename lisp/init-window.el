;; [[file:../readme.org::*Disable built in][Disable built in:1]]
(tab-bar-mode -1)
(tab-line-mode -1)

(global-unset-key (kbd "C-<tab>"))
;; Disable built in:1 ends here

;; [[file:../readme.org::*Centaur tabs][Centaur tabs:1]]
(use-package centaur-tabs
	:custom
	(centaur-tabs-style "bar")
	(centaur-tabs-height 32)
	(centaur-tabs-set-icons t)
	(centaur-tabs-set-modified-marker t)
	(centaur-tabs-show-navigation-buttons t)
	(centaur-tabs-set-bar 'under)
	(x-underline-at-descent-line t)
	(uniquify-separator "/")
	(uniquify-buffer-name-style 'forward)
	:config
	(centaur-tabs-headline-match)
	(centaur-tabs-mode 1)
	(centaur-tabs-group-by-projectile-project)

	(defun centaur-tabs-hide-tab (x)
		"Do no to show buffer X in tabs."
		(let ((name (format "%s" x)))
			(or
			 ;; Current window is not dedicated window.
			 (window-dedicated-p (selected-window))

			 ;; Buffer name not match below blacklist.
			 (string-prefix-p "*epc" name)
			 (string-prefix-p "*helm" name)
			 (string-prefix-p "*Helm" name)
			 (string-prefix-p "*Compile-Log*" name)
			 (string-prefix-p "*Messages*" name)
			 (string-prefix-p "*lsp" name)
			 (string-prefix-p "*company" name)
			 (string-prefix-p "*Flycheck" name)
			 (string-prefix-p "*tramp" name)
			 (string-prefix-p " *Mini" name)
			 (string-prefix-p "*help" name)
			 (string-prefix-p "*straight" name)
			 (string-prefix-p " *temp" name)
			 (string-prefix-p "*Help" name)
			 (string-prefix-p "*Async" name)

			 ;; Is not magit buffer.
			 (and (string-prefix-p "magit" name)
						(not (file-name-extension name)))
			 )))
	:hook
	(dashboard-mode . centaur-tabs-local-mode)
	(term-mode . centaur-tabs-local-mode)
	(calendar-mode . centaur-tabs-local-mode)
	(org-agenda-mode . centaur-tabs-local-mode)
	(org-src-mode . centaur-tabs-local-mode)
	(helpful-mode . centaur-tabs-local-mode)
	:general
	(jtd/leader-key "tt" 'centaur-tabs-mode)
	:bind
	("C-<prior>" . centaur-tabs-backward)
	("C-<next>" . centaur-tabs-forward)
	("C-c t s" . centaur-tabs-counsel-switch-group)
	("C-c t p" . centaur-tabs-group-by-projectile-project)
	("C-c t g" . centaur-tabs-group-buffer-groups)
	(:map evil-normal-state-map
				("g t" . centaur-tabs-forward)
				("g T" . centaur-tabs-backward)))
;; Centaur tabs:1 ends here

;; [[file:../readme.org::*Golden ratio][Golden ratio:1]]
(use-package golden-ratio
	:hook (after-init . golden-ratio-mode)
	:custom
	((golden-ratio-exclude-modes '(treemacs-mode imenu-list-major-mode))
	 (golden-ratio-extra-commands
		'(windmove-left
			windmove-right
			windmove-down
			windmove-up
			evil-window-left
			evil-window-right
			evil-window-up
			evil-window-down
			buf-move-left
			buf-move-right
			buf-move-up
			buf-move-down
			window-number-select
			select-window
			select-window-1
			select-window-2
			select-window-3
			select-window-4
			select-window-5
			select-window-6
			select-window-7
			select-window-8
			select-window-9)))
	:config
	(golden-ratio-mode 1))
;; Golden ratio:1 ends here

;; [[file:../readme.org::*Transpose frame][Transpose frame:1]]
(use-package transpose-frame
	:general
	(jtd/leader-key
		"wt" '(transpose-frame :wk "transpose")
		"wf" '(rotate-frame :wk "flip")))
;; Transpose frame:1 ends here
