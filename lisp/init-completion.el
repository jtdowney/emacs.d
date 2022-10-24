;; [[file:../readme.org::*Ignore case][Ignore case:1]]
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)
;; Ignore case:1 ends here

;; [[file:../readme.org::*Vertico][Vertico:1]]
(use-package vertico
  :demand
  :straight (:files (:defaults "extensions/*"))
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      ("C-l" . vertico-insert)
	      :map minibuffer-local-map
	      ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode 1)
  (advice-add #'vertico--format-candidate :around
	      (lambda (orig cand prefix suffix index _start)
		(setq cand (funcall orig cand prefix suffix index _start))
		(concat
		 (if (= vertico--index index)
		     (propertize "» " 'face 'vertico-current)
		   "  ")
		 cand))))

(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
;; Vertico:1 ends here

;; [[file:../readme.org::*Save history][Save history:1]]
(use-package savehist
  :after vertico
  :hook
  (on-first-input . savehist-mode))
;; Save history:1 ends here

;; [[file:../readme.org::*Consult][Consult:1]]
(use-package consult
  :general
  (jtd/leader-key
    "bb" 'consult-buffer
    "fr" 'consult-recent-file
    "pb" 'consult-project-buffer
    "so" 'consult-outline
    "si" 'consult-isearch
    "sr" 'consult-ripgrep
    "ss" 'consult-line)
  :init
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))
;; Consult:1 ends here

;; [[file:../readme.org::*Embark][Embark:1]]
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;; Embark:1 ends here

;; [[file:../readme.org::*Marginalia][Marginalia:1]]
(use-package marginalia
  :after vertico
  :hook
  (on-first-input . marginalia-mode))
;; Marginalia:1 ends here

;; [[file:../readme.org::*Better search matching][Better search matching:1]]
(use-package fussy
  :after vertico
  :config
  (push 'fussy completion-styles)
  (setq completion-category-defaults nil
	completion-category-overrides nil))

(use-package orderless
  :after fussy
  :commands orderless-filter
  :init
  (setq fussy-filter-fn 'fussy-filter-orderless))
;; Better search matching:1 ends here

;; [[file:../readme.org::*Corfu][Corfu:1]]
(use-package corfu
  :hook ((prog-mode . corfu-mode)
	 (org-mode . corfu-mode))
  :bind
  (:map corfu-map
	("C-j" . corfu-next)
	("C-k" . corfu-previous))
  :general
  (evil-insert-state-map "C-k" nil)
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-preselect-first t)
  :init
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
		(bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode +1))
;; Corfu:1 ends here

;; [[file:../readme.org::*Kind icon][Kind icon:1]]
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
;; Kind icon:1 ends here
