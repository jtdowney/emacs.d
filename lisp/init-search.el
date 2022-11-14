;; [[file:../readme.org::*Avy][Avy:1]]
(use-package avy
  :general
  (jtd/leader-key
    "SPC" 'evil-avy-goto-subword-1
    "j" '(:ignore t :wk "jump")
    "jJ" 'evil-avy-goto-char-2
    "jj" 'evil-avy-goto-char
    "jl" 'evil-avy-goto-line
    "jw" 'evil-avy-goto-subword-1))
;; Avy:1 ends here
