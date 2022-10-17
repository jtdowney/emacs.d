;; [[file:../readme.org::*Disable built in][Disable built in:1]]
(tab-bar-mode -1)
(tab-line-mode -1)

(global-unset-key (kbd "C-<tab>"))
;; Disable built in:1 ends here

;; [[file:../readme.org::*Golden ratio][Golden ratio:1]]
(use-package golden-ratio
  :hook
  (on-first-buffer . golden-ratio-mode)
  :custom
  (golden-ratio-exclude-modes '(treemacs-mode imenu-list-major-mode))
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
;; Golden ratio:1 ends here

;; [[file:../readme.org::*Transpose frame][Transpose frame:1]]
(use-package transpose-frame
  :general
  (jtd/leader-key
    "wt" '(transpose-frame :wk "transpose")
    "wf" '(rotate-frame :wk "flip")))
;; Transpose frame:1 ends here
