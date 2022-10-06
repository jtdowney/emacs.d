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
