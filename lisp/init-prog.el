;; [[file:../readme.org::*Project management][Project management:1]]
(use-package projectile
	:defer 1
	:general
	(jtd/leader-key
		"p" '(:keymap projectile-command-map :wk "projectile"))
	:custom ((projectile-project-search-path '("~/code"))
					 (projectile-switch-project-action #'projectile-dired))
	:config
	(projectile-mode 1))
;; Project management:1 ends here

;; [[file:../readme.org::*magit][magit:1]]
(use-package magit
	:general
	(jtd/leader-key
		"gb" 'magit-blame
		"gl" 'magit-log
		"gg" 'magit-status
		"gG" 'magit-status-here))
;; magit:1 ends here

;; [[file:../readme.org::*Time machine][Time machine:1]]
(use-package git-timemachine
	:hook
	(git-time-machine-mode . evil-normalize-keymaps)
	:custom
	(git-timemachine-show-minibuffer-details t)
	:general
	(jtd/leader-key
		"gt" 'git-timemachine)
	(git-timemachine-mode-map
	 "C-k" 'git-timemachine-show-previous-revision
	 "C-j" 'git-timemachine-show-next-revision
	 "q" 'git-timemachine-quit))
;; Time machine:1 ends here
