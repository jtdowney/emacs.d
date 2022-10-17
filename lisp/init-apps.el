;; [[file:../readme.org::*Notes][Notes:1]]
(use-package deft
  :general
  (jtd/leader-key
    "n" '(:ignore t :wk "notes")
    "nf" 'deft-find-file
    "nv" 'deft)
  (jtd/local-leader-key :keymaps 'deft-mode-map
    "c" 'deft-filter-clear
    "d" 'deft-delete-file
    "i" 'deft-toggle-incremental-search
    "n" 'deft-new-file
    "r" 'deft-rename-file)
  :config
  (evil-set-initial-state 'deft-mode 'insert)

  (setq deft-default-extension "org"
	deft-directory "~/notes"
	deft-use-filename-as-title nil
	deft-use-filter-string-for-filename t))
;; Notes:1 ends here

;; [[file:../readme.org::*mu4e][mu4e:1]]
(use-package mu4e
  :straight nil
  :custom
  (mu4e-headers-fields '((:human-date . 12)
			 (:flags . 6)
			 (:from . 22)
			 (:subject)))
  :general
  (jtd/leader-key
    "am" 'mu4e))
;; mu4e:1 ends here

;; [[file:../readme.org::*org-msg][org-msg:1]]
(use-package org-msg
  :after mu4e)
;; org-msg:1 ends here

;; [[file:../readme.org::*RSS][RSS:1]]
(use-package elfeed
  :custom
  (elfeed-sort-order 'ascending)
  :general
  (jtd/leader-key
    "af" 'elfeed))

(use-package elfeed-protocol
  :after elfeed
  :config
  (elfeed-protocol-enable))
;; RSS:1 ends here

;; [[file:../readme.org::*IRC][IRC:1]]
(use-package circe
  :custom
  (circe-reduce-lurker-spam t)
  :general
  (jtd/leader-key
    "ac" 'circe)
  :config
  (enable-circe-color-nicks))
;; IRC:1 ends here
