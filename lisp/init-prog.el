;; [[file:../readme.org::*Line numbers][Line numbers:1]]
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; Line numbers:1 ends here

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
	:straight (:package "git-timemachine"
											:type git
											:host nil
											:repo "https://codeberg.org/pidu/git-timemachine.git")
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

;; [[file:../readme.org::*Highlight uncommitted changes][Highlight uncommitted changes:1]]
(use-package diff-hl
	:hook (((prog-mode text-mode vc-dir-mode) . diff-hl-mode)
				 (magit-pre-refresh . diff-hl-magit-pre-refresh)
				 (magit-post-refresh . diff-hl-magit-post-refresh)))
;; Highlight uncommitted changes:1 ends here

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

;; [[file:../readme.org::*Comments][Comments:1]]
(use-package evil-commentary
	:hook (prog-mode . evil-commentary-mode))
;; Comments:1 ends here

;; [[file:../readme.org::*Treat _ as part of a word like vim][Treat _ as part of a word like vim:1]]
(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
;; Treat _ as part of a word like vim:1 ends here

;; [[file:../readme.org::*Delete trailing white space][Delete trailing white space:1]]
(add-hook 'before-save-hook
					(lambda ()
						(when (derived-mode-p 'prog-mode)
							(whitespace-cleanup))))
;; Delete trailing white space:1 ends here

;; [[file:../readme.org::*Completion][Completion:1]]
(use-package company
	:hook (prog-mode . company-mode)
	:custom
	(company-minimum-prefix-length 1))
;; Completion:1 ends here

;; [[file:../readme.org::*Formatting][Formatting:1]]
(use-package apheleia
	:hook
	(after-init . apheleia-global-mode))
;; Formatting:1 ends here

;; [[file:../readme.org::*Tree sitter][Tree sitter:1]]
(use-package tree-sitter
	:hook
	(after-init . global-tree-sitter-mode)
	(tree-sitter-after-on-hook . tree-sitter-hl-mode))

(use-package tree-sitter-langs
	:after tree-sitter)
;; Tree sitter:1 ends here

;; [[file:../readme.org::*Rainbow Delimiters][Rainbow Delimiters:1]]
(use-package rainbow-delimiters
	:hook (prog-mode . rainbow-delimiters-mode))
;; Rainbow Delimiters:1 ends here

;; [[file:../readme.org::*Smartparens][Smartparens:1]]
(use-package smartparens
	:hook (prog-mode . smartparens-mode))
;; Smartparens:1 ends here

;; [[file:../readme.org::*Lispy][Lispy:1]]
(use-package lispy
	:hook (emacs-lisp-mode . lispy-mode))
;; Lispy:1 ends here

;; [[file:../readme.org::*Lispyville][Lispyville:1]]
(use-package lispyville
	:hook (lispy-mode . lispyville-mode)
	:config
	(lispyville-set-key-theme '((operators normal)
															c-w
															(prettify insert)
															(atom-movement t)
															slurp/barf-lispy
															additional
															additional-insert)))
;; Lispyville:1 ends here
