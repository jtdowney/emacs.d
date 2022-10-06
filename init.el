;; [[file:readme.org::*init.el: startup optimization][init.el: startup optimization:1]]
;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this. However, this
;; may cause problems on builds of Emacs where its site lisp files aren't
;; byte-compiled and we're forced to load the *.el.gz files (e.g. on Alpine)
(unless (daemonp)
	(defvar doom--initial-file-name-handler-alist file-name-handler-alist)
	(setq file-name-handler-alist nil)
	;; Restore `file-name-handler-alist' later, because it is needed for handling
	;; encrypted or compressed files, among other things.
	(defun doom-reset-file-handler-alist-h ()
		;; Re-add rather than `setq', because changes to `file-name-handler-alist'
		;; since startup ought to be preserved.
		(dolist (handler file-name-handler-alist)
			(add-to-list 'doom--initial-file-name-handler-alist handler))
		(setq file-name-handler-alist doom--initial-file-name-handler-alist))
	(add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h)
	(add-hook 'after-init-hook (lambda ()
																;; restore after startup
																(setq gc-cons-threshold 16777216
																			gc-cons-percentage 0.1))))

;; Ensure Doom is running out of this file's directory
(setq user-emacs-directory (file-truename (file-name-directory load-file-name)))
;; init.el: startup optimization:1 ends here

;; [[file:readme.org::*init.el: load modules][init.el: load modules:1]]
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let ((file-name-handler-alist nil)
(gc-cons-threshold 100000000))
	(load "init-core")
	(load "init-evil")
	(load "init-ui")
	(load "init-window")
	(load "init-org")
	)
;; init.el: load modules:1 ends here
