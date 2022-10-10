;; [[file:../readme.org::*bootstrap straight and straight-use-package][bootstrap straight and straight-use-package:1]]
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-recipes-gnu-elpa-use-mirror t)
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
			 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
			(bootstrap-version 6))
	(unless (file-exists-p bootstrap-file)
		(with-current-buffer
				(url-retrieve-synchronously
				 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
				 'silent 'inhibit-cookies)
			(goto-char (point-max))
			(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq comp-deferred-compilation-black-list nil)
;; bootstrap straight and straight-use-package:1 ends here

;; [[file:../readme.org::*Enable use-package statistics][Enable use-package statistics:1]]
(setq use-package-compute-statistics t)
;; Enable use-package statistics:1 ends here

;; [[file:../readme.org::*Sane defaults][Sane defaults:1]]
(use-package emacs
	:init
	(setq inhibit-startup-screen t
				initial-scratch-message nil
				sentence-end-double-space nil
				ring-bell-function 'ignore
				ad-redefinition-action 'accept
				frame-resize-pixelwise t)

	(setq user-full-name "John Downey"
				user-mail-address "jdowney@gmail.com")

	(setq read-process-output-max (* 1024 1024)) ;; 1mb

	;; always allow 'y' instead of 'yes'.
	(defalias 'yes-or-no-p 'y-or-n-p)

	;; default to utf-8 for all the things
	(set-charset-priority 'unicode)
	(setq locale-coding-system 'utf-8
				coding-system-for-read 'utf-8
				coding-system-for-write 'utf-8)
	(set-terminal-coding-system 'utf-8)
	(set-keyboard-coding-system 'utf-8)
	(set-selection-coding-system 'utf-8)
	(prefer-coding-system 'utf-8)
	(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

	;; write over selected text on input... like all modern editors do
	(delete-selection-mode t)

	;; enable recent files mode.
	(recentf-mode t)
	(setq recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
													,(expand-file-name "eln-cache/" user-emacs-directory)
													,(expand-file-name "etc/" user-emacs-directory)
													,(expand-file-name "var/" user-emacs-directory)))

	;; don't want ESC as a modifier
	(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

	;; Don't persist a custom file, this bites me more than it helps
	(setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
	(setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
	(setq enable-local-variables :all)     ; fix =defvar= warnings

	;; stop emacs from littering the file system with backup files
	(setq make-backup-files nil
				auto-save-default nil
				create-lockfiles nil)

	;; follow symlinks
	(setq vc-follow-symlinks t)

	;; don't show any extra window chrome
	(when (window-system)
		(tool-bar-mode -1)
		(toggle-scroll-bar -1))

	;; enable winner mode globally for undo/redo window layout changes
	(winner-mode t)

	(show-paren-mode t)

	;; less noise when compiling elisp
	(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
	(setq native-comp-async-report-warnings-errors nil)
	(setq load-prefer-newer t)

	;; clean up the mode line
	(display-time-mode -1)
	(setq column-number-mode t)

	;; use common convention for indentation by default
	(setq-default indent-tabs-mode t)
	(setq-default tab-width 2)

	;; Enable indentation+completion using the TAB key.
	;; Completion is often bound to M-TAB.
	(setq tab-always-indent 'complete))
;; Sane defaults:1 ends here

;; [[file:../readme.org::*Private configuration][Private configuration:1]]
(add-hook
 'after-init-hook
 (lambda ()
	 (let ((private-file (concat user-emacs-directory "private.el")))
		 (when (file-exists-p private-file)
			 (load-file private-file)))))
;; Private configuration:1 ends here

;; [[file:../readme.org::*Zoom][Zoom:1]]
(use-package emacs
	:init
	(global-set-key (kbd "C-=") 'text-scale-increase)
	(global-set-key (kbd "C--") 'text-scale-decrease))
;; Zoom:1 ends here

;; [[file:../readme.org::*macOS][macOS:1]]
(use-package emacs
	:init
	(when (eq system-type 'darwin)
		(setq mac-command-modifier 'super)     ; command as super
		(setq mac-option-modifier 'meta)     ; alt as meta
		(setq mac-control-modifier 'control))

	;; when on emacs-mac
	(when (fboundp 'mac-auto-operator-composition-mode)
			(mac-auto-operator-composition-mode)   ;; enables font ligatures
			(global-set-key [(s c)] 'kill-ring-save)
			(global-set-key [(s v)] 'yank)
			(global-set-key [(s x)] 'kill-region)
			(global-set-key [(s q)] 'kill-emacs)))
;; macOS:1 ends here

;; [[file:../readme.org::*Garbage collector magic hack][Garbage collector magic hack:1]]
(use-package gcmh
	:config
	(gcmh-mode 1))
;; Garbage collector magic hack:1 ends here

;; [[file:../readme.org::*Helpful][Helpful:1]]
(use-package helpful
	:bind
	([remap describe-function] . helpful-function)
	([remap describe-command] . helpful-command)
	([remap describe-variable] . helpful-variable)
	([remap describe-key] . helpful-key))
;; Helpful:1 ends here

;; [[file:../readme.org::*Save recent files][Save recent files:1]]
(use-package recentf
	:defer 1
	:straight (:type built-in)
	:config
	(recentf-mode 1))
;; Save recent files:1 ends here

;; [[file:../readme.org::*Highlight trailing whitespace][Highlight trailing whitespace:1]]
(dolist (hook '(prog-mode-hook text-mode-hook))
	(add-hook hook (lambda ())
			(setq-local show-trailing-whitespace t)))
;; Highlight trailing whitespace:1 ends here

;; [[file:../readme.org::*Undo][Undo:1]]
(use-package undo-fu)
;; Undo:1 ends here

;; [[file:../readme.org::*Tramp][Tramp:1]]
(use-package tramp
	:defer 1
	:straight (:type built-in)
	:custom
	(vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
																vc-ignore-dir-regexp
																tramp-file-name-regexp))
	(tramp-default-method "ssh")
	(tramp-auto-save-directory (expand-file-name "tramp-auto-save" user-emacs-directory))
	(tramp-persistency-file-name (expand-file-name "tramp-connection-history" user-emacs-directory))
	(tramp-use-ssh-controlmaster-options nil)
	(remote-file-name-inhibit-cache nil)
	(tramp-ssh-controlmaster-options (concat
																		"-o ControlPath=/tmp/ssh-tramp-%%r@%%h:%%p "
																		"-o ControlMaster=auto -o ControlPersist=yes")))

(use-package docker-tramp
	:defer 2)
;; Tramp:1 ends here
