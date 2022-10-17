;; [[file:../readme.org::*Ranger][Ranger:1]]
(use-package ranger
  :custom
  (ranger-key "zp")
  :general
  (jtd/leader-key
    "ar" 'ranger))
;; Ranger:1 ends here

;; [[file:../readme.org::*dired][dired:1]]
(use-package dired
  :straight (:type built-in)
  :general
  (jtd/leader-key
    "ad" 'dired)
  :config
  (require 'dired-x)
  (evil-define-key 'normal 'global
    (kbd "_") 'projectile-dired
    (kbd "-") 'dired-jump)
  (evil-define-key 'normal dired-mode-map (kbd "SPC") nil)
  (evil-define-key 'normal dired-mode-map "c" 'find-file))
;; dired:1 ends here

;; [[file:../readme.org::*Treemacs][Treemacs:1]]
(use-package treemacs
  :custom
  ((treemacs-project-follow-mode t)
   (treemacs-follow-mode t)
   (treemacs-filewatch-mode t))
  :general
  (jtd/leader-key
    "fd" 'treemacs-find-file
    "ft" 'treemacs))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))
;; Treemacs:1 ends here
