;; [[file:../readme.org::*evil mode][evil mode:1]]
(use-package evil
  :general
  (jtd/leader-key
    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split)
  (general-imap "C-g" 'evil-normal-state)
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
   (evil-undo-system 'undo-fu)
   (evil-search-module 'evil-search-module 'evil-search)  ;; enables gn
   (evil-split-window-below t)
   (evil-vsplit-window-right t)
   (evil-auto-indent nil)
   (evil-want-C-w-in-emacs-state t))
  :init
  (evil-mode 1)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
;; evil mode:1 ends here

;; [[file:../readme.org::*evil-collection][evil-collection:1]]
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
;; evil-collection:1 ends here

;; [[file:../readme.org::*Surround][Surround:1]]
(use-package evil-surround
  :after evil
  :hook
  (on-first-input . global-evil-surround-mode))
;; Surround:1 ends here

;; [[file:../readme.org::*Preview registers][Preview registers:1]]
(use-package evil-owl
  :hook
  (on-first-input . evil-owl-mode)
  :custom
  (evil-owl-max-string-length 500)
  (evil-owl-display-method 'window))
;; Preview registers:1 ends here
