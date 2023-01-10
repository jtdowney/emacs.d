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
  :custom
  (user-full-name "John Downey")
  (user-mail-address "jdowney@gmail.com")

  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (initial-major-mode 'text-mode)

  (sentence-end-double-space nil)
  (ring-bell-function 'ignore)
  (ad-redefinition-action 'accept)
  (frame-resize-pixelwise t)
  (vc-follow-symlinks t)

  (custom-file (make-temp-file ""))
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)

  (byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (native-comp-async-report-warnings-errors nil)

  (column-number-mode t)
  (tab-always-indent 'complete)
  :init
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

  (delete-selection-mode 1)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (winner-mode 1)
  (show-paren-mode 1)
  (display-time-mode -1)

  (setq-default indent-tabs-mode nil))
;; Sane defaults:1 ends here

;; [[file:../readme.org::*On hooks][On hooks:1]]
(use-package on
  :demand
  :straight (:host github :repo "ajgrf/on.el"))
;; On hooks:1 ends here

;; [[file:../readme.org::*Private configuration][Private configuration:1]]
(add-hook
 'after-init-hook
 (lambda ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))
;; Private configuration:1 ends here

;; [[file:../readme.org::*Zoom][Zoom:1]]
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
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
  :straight (:type built-in)
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
		     ,(expand-file-name "eln-cache/" user-emacs-directory)
		     ,(expand-file-name "etc/" user-emacs-directory)
		     ,(expand-file-name "var/" user-emacs-directory))))
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
  :config
  (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
				     vc-ignore-dir-regexp
				     tramp-file-name-regexp)
	tramp-default-method "ssh"
	tramp-auto-save-directory (expand-file-name "tramp-auto-save" user-emacs-directory)
	tramp-persistency-file-name (expand-file-name "tramp-connection-history" user-emacs-directory)
	tramp-use-ssh-controlmaster-options nil
	remote-file-name-inhibit-cache nil
	tramp-ssh-controlmaster-options (concat
					 "-o ControlPath=/tmp/ssh-tramp-%%r@%%h:%%p "
					 "-o ControlMaster=auto -o ControlPersist=yes")))

(use-package tramp-container
  :defer 2
  :straight (:type built-in))
;; Tramp:1 ends here
