;; [[file:../readme.org::*Theme][Theme:1]]
(load-theme 'modus-vivendi t)
;; Theme:1 ends here

;; [[file:../readme.org::*Font][Font:1]]
(set-face-attribute 'default nil :font "Fira Code" :height 160)
(set-face-attribute 'variable-pitch nil :font "Fira Sans" :height 160)
;; Font:1 ends here

;; [[file:../readme.org::*Highlight current line][Highlight current line:1]]
(global-hl-line-mode 1)
;; Highlight current line:1 ends here

;; [[file:../readme.org::*Highlight indentation guides][Highlight indentation guides:1]]
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top))
;; Highlight indentation guides:1 ends here

;; [[file:../readme.org::*Doom modeline][Doom modeline:1]]
(use-package doom-modeline
  :hook
  (on-init-ui . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-env-enable-python nil)
  (doom-modeline-height 15))
;; Doom modeline:1 ends here

;; [[file:../readme.org::*All the icons][All the icons:1]]
(use-package all-the-icons)
;; All the icons:1 ends here

;; [[file:../readme.org::*Icons in the terminal][Icons in the terminal:1]]
(use-package icons-in-terminal
  :straight (:host github :repo "seagle0128/icons-in-terminal.el")
  :if (not (display-graphic-p))
  :config
  (defalias #'all-the-icons-insert #'icons-in-terminal-insert)
  (defalias #'all-the-icons-insert-faicon #'icons-in-terminal-insert-faicon)
  (defalias #'all-the-icons-insert-fileicon #'icons-in-terminal-insert-fileicon)
  (defalias #'all-the-icons-insert-material #'icons-in-terminal-insert-material)
  (defalias #'all-the-icons-insert-octicon #'icons-in-terminal-insert-octicon)
  (defalias #'all-the-icons-insert-wicon #'icons-in-terminal-insert-wicon)
  (defalias #'all-the-icons-icon-for-dir #'icons-in-terminal-icon-for-dir)
  (defalias #'all-the-icons-icon-for-file #'icons-in-terminal-icon-for-file)
  (defalias #'all-the-icons-icon-for-mode #'icons-in-terminal-icon-for-mode)
  (defalias #'all-the-icons-icon-for-url #'icons-in-terminal-icon-for-url)
  (defalias #'all-the-icons-icon-family #'icons-in-terminal-icon-family)
  (defalias #'all-the-icons-icon-family-for-buffer #'icons-in-terminal-icon-family-for-buffer)
  (defalias #'all-the-icons-icon-family-for-file #'icons-in-terminal-icon-family-for-file)
  (defalias #'all-the-icons-icon-family-for-mode #'icons-in-terminal-icon-family-for-mode)
  (defalias #'all-the-icons-icon-for-buffer #'icons-in-terminal-icon-for-buffer)
  (defalias #'all-the-icons-faicon #'icons-in-terminal-faicon)
  (defalias #'all-the-icons-octicon #'icons-in-terminal-octicon)
  (defalias #'all-the-icons-fileicon #'icons-in-terminal-fileicon)
  (defalias #'all-the-icons-material #'icons-in-terminal-material)
  (defalias #'all-the-icons-wicon #'icons-in-terminal-wicon)
  (defalias 'all-the-icons-default-adjust 'icons-in-terminal-default-adjust)
  (defalias 'all-the-icons-color-icons 'icons-in-terminal-color-icons)
  (defalias 'all-the-icons-scale-factor 'icons-in-terminal-scale-factor)
  (defalias 'all-the-icons-icon-alist 'icons-in-terminal-icon-alist)
  (defalias 'all-the-icons-dir-icon-alist 'icons-in-terminal-dir-icon-alist)
  (defalias 'all-the-icons-weather-icon-alist 'icons-in-terminal-weather-icon-alist))
;; Icons in the terminal:1 ends here

;; [[file:../readme.org::*Ligatures][Ligatures:1]]
(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures 't '("www" "ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
				       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
				       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
				       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
				       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
				       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
				       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
				       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
				       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
				       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
				       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
				       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
				       "\\\\" "://")))
;; Ligatures:1 ends here

;; [[file:../readme.org::*Pretty symbols][Pretty symbols:1]]
(global-prettify-symbols-mode 1)
;; Pretty symbols:1 ends here

;; [[file:../readme.org::*Hide modeline][Hide modeline:1]]
(use-package hide-mode-line
  :commands hide-mode-line)
;; Hide modeline:1 ends here

;; [[file:../readme.org::*Show end of file][Show end of file:1]]
(use-package vi-tilde-fringe
  :if (display-graphic-p)
  :hook (prog-mode . vi-tilde-fringe-mode))
;; Show end of file:1 ends here

;; [[file:../readme.org::*Emoji][Emoji:1]]
(use-package emojify
  :hook
  (on-init-ui . global-emojify-mode)
  :config
  (delete 'mu4e-headers-mode emojify-inhibit-major-modes))
;; Emoji:1 ends here

;; [[file:../readme.org::*Fill column indicator][Fill column indicator:1]]
(use-package display-fill-column-indicator
  :straight (:type built-in)
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :custom
  (fill-column 120))
;; Fill column indicator:1 ends here

;; [[file:../readme.org::*Dashboard][Dashboard:1]]
(use-package dashboard
  :demand t
  :custom
  (dashboard-projects-backend 'project-el)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-banner-logo-title nil)
  (dashboard-set-footer nil)
  (dashboard-items '((recents . 5)
		     (projects . 5)))
  :config
  (dashboard-setup-startup-hook))
;; Dashboard:1 ends here
