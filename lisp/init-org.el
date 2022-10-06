;; [[file:../readme.org::*Org mode][Org mode:1]]
(use-package org
	:hook (org-mode . variable-pitch-mode))
;; Org mode:1 ends here

;; [[file:../readme.org::*Evil integration][Evil integration:1]]
(use-package evil-org
	:hook (org-mode . evil-org-mode)
	:config
	(evil-org-set-key-theme '(textobjects
														insert
														navigation
														additional
														shift
														todo
														heading)))
;; Evil integration:1 ends here

;; [[file:../readme.org::*Structure templates][Structure templates:1]]
(use-package org-tempo
	:after org
	:straight nil
	:config
	(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
	(add-to-list 'org-structure-template-alist '("py" . "src python"))
	(add-to-list 'org-structure-template-alist '("sh" . "src shell")))
;; Structure templates:1 ends here

;; [[file:../readme.org::*Modern][Modern:1]]
(use-package org-modern
	:init
	(global-org-modern-mode))
;; Modern:1 ends here
