;; [[file:../readme.org::*Org mode][Org mode:1]]
(use-package org
  :defer t
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  :general
  (jtd/leader-key
    "o" '(:ignore t :wk "org")
    "oa" 'org-agenda-list
    "oc" 'org-capture
    "om" 'org-tags-view
    "oo" 'org-agenda
    "ot" 'org-todo-list)
  :custom
  (org-agenda-files '("~/org/inbox.org"
		      "~/org/projects.org"
		      "~/org/tickler.org"))
  (org-refile-targets '(("~/org/projects.org" :maxlevel . 3)
			("~/org/someday.org" :level . 1)
			("~/org/tickler.org" :maxlevel . 2)))
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  :config
  (jtd/local-leader-key :keymaps 'org-mode-map
    "!" 'org-time-stamp-inactive
    "'" 'org-edit-special
    "*" 'org-ctrl-c-star
    "," 'org-ctrl-c-ctrl-c
    "-" 'org-ctrl-c-minus
    "." 'org-time-stamp
    "/" 'org-sparse-tree
    ":" 'org-set-tags
    "A" 'org-archive-subtree
    "D" 'org-insert-drawer
    "H" 'org-shiftleft
    "I" 'org-clock-in
    "J" 'org-shiftdown
    "K" 'org-shiftup
    "L" 'org-shiftright
    "N" 'widen
    "O" 'org-clock-out
    "P" 'org-set-property
    "R" 'org-refile
    "Sh" 'org-promote-subtree
    "Sj" 'org-move-subtree-down
    "Sk" 'org-move-subtree-up
    "Sl" 'org-demote-subtree
    "T" 'org-show-todo-tree
    "^" 'org-sort
    "a" 'org-agenda
    "b" 'org-tree-to-indirect-buffer
    "c" 'org-capture
    "d" 'org-deadline
    "e" 'org-export-dispatch
    "f" 'org-set-effort
    "hI" 'org-insert-heading
    "hi" 'org-insert-heading-after-current
    "hs" 'org-insert-subheading
    "ia" 'org-attach
    "if" 'org-footnote-new
    "il" 'org-insert-link
    "l" 'org-open-at-point
    "n" 'org-narrow-to-subtree
    "q" 'org-clock-cancel
    "s" 'org-schedule
    "tE" 'org-table-export
    "tH" 'org-table-move-column-left
    "tI" 'org-table-import
    "tJ" 'org-table-move-row-down
    "tK" 'org-table-move-row-up
    "tL" 'org-table-move-column-right
    "tN" 'org-table-create-with-table.el
    "ta" 'org-table-align
    "tb" 'org-table-blank-field
    "tc" 'org-table-convert
    "tdc" 'org-table-delete-column
    "tdr" 'org-table-kill-row
    "te" 'org-table-eval-formula
    "th" 'org-table-previous-field
    "tiH" 'org-table-hline-and-move
    "tic" 'org-table-insert-column
    "tih" 'org-table-insert-hline
    "tir" 'org-table-insert-row
    "tj" 'org-table-next-row
    "tl" 'org-table-next-field
    "tn" 'org-table-create
    "tr" 'org-table-recalculate
    "ts" 'org-table-sort-lines
    "ttf" 'org-table-toggle-formula-debugger
    "tto" 'org-table-toggle-coordinate-overlays
    "tw" 'org-table-wrap-region
    "RET" 'org-ctrl-c-ret)
  (jtd/local-leader-key
    :definer 'minor-mode
    :keymaps 'org-src-mode
    "c" 'org-edit-src-exit
    "a" 'org-edit-src-abort
    "k" 'org-edit-src-abort)
  (setq org-capture-templates
	`(("b" "Books")
	  ("bf" "Finished book" table-line
	   (file+headline ,(concat org-directory "/books.org") "Finished")
	   "| %^{Title} | %^{Author} | %u |")
	  ("br" "Book to read" entry
	   (file+headline ,(concat org-directory "/books.org") "To Read")
	   "* %i%?\n")
	  ("g" "GTD")
	  ("gt" "Todo [inbox]" entry
	   (file+headline ,(concat org-directory "/inbox.org") "Tasks")
	   "* TODO %i%?")
	  ("gT" "Tickler" entry
	   (file+headline ,(concat org-directory "/tickler.org") "Tickler")
	   "* %i%? \n %U")
	  ("i" "Ideas")
	  ("ib" "Blog idea" entry
	   (file ,(concat org-directory "/blog-ideas.org"))
	   "* %?\n")
	  )))
;; Org mode:1 ends here

;; [[file:../readme.org::*Evil integration][Evil integration:1]]
(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
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
  :hook
  (org-mode . org-modern-mode))
;; Modern:1 ends here
