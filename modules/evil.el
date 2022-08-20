(use-package evil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-C-i-jump nil)
  (evil-mode))
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer pdf))
  (evil-collection-init))
(use-package evil-tutor)

(use-package general
  :config
  (general-evil-setup t))
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "SPC"   '(counsel-M-x :which-key "Counsel M-x"))
