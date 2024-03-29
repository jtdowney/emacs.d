;; [[file:../readme.org::*General][General:1]]
(defun jtd/find-file-in-emacs ()
  "Find files in the emacs configuration directory"
  (interactive)
  (ido-find-file-in-dir user-emacs-directory))

(defun jtd/switch-to-scratch-buffer ()
  "Switch to the scratch buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun jtd/copy-buffer ()
  "Copy the entire buffer"
  (interactive)
  (mark-whole-buffer)
  (kill-ring-save 0 0 t))

(defun jtd/kill-other-buffers ()
  "Kill all buffers but the current one. Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(use-package general
  :config
  (general-evil-setup)

  (general-create-definer jtd/leader-key
    :keymaps 'override
    :states '(normal visual)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer jtd/local-leader-key
    :states '(normal visual motion)
    :prefix ","
    :global-prefix "C-,")

  (general-def '(normal insert visual emacs) "C-@" (general-simulate-key "C-SPC"))

  (jtd/leader-key
    ":" 'execute-extended-command
    "a" '(:ignore t :wk "apps")
    "b" '(:ignore t :wk "buffer")
    "bk" 'kill-buffer-and-window
    "bK" 'jtd/kill-other-buffers
    "bs" 'jtd/switch-to-scratch-buffer
    "bY" 'jtd/copy-buffer
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file")
    "fe" '(jtd/find-file-in-emacs :wk "find file in emacs.d")
    "g" '(:ignore t :wk "git")
    "h" '(:ignore t :wk "help")
    "ha" 'apropos-command
    "hf" 'helpful-function
    "hk" 'helpful-key
    "hm" 'helpful-macro
    "ho" 'helpful-symbol
    "hv" 'helpful-variable
    "hx" 'helpful-command
    "s" '(:ignore t :wk "search")
    "sj" '(imenu :wk "jump")
    "t" '(:ignore t :wk "tabs")
    "T" '(:ignore t :wk "toggle")
    "w" '(:ignore t :wk "window"))

  (general-define-key
   :definer 'minor-mode
   :states '(normal motion)
   :keymaps 'outline-minor-mode
   "]h" 'outline-next-visible-heading
   "[h" 'outline-prev-visible-heading))
;; General:1 ends here

;; [[file:../readme.org::*Which key][Which key:1]]
(use-package which-key
  :hook (on-first-input . which-key-mode)
  :custom
  (which-key-idle-delay 1))
;; Which key:1 ends here

;; [[file:../readme.org::*Hydra][Hydra:1]]
(use-package hydra)
;; Hydra:1 ends here
