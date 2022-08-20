(setq-default fill-column 64)
(use-package writeroom-mode
  :config
  (setq writeroom-width 76))
(add-hook 'writeroom-mode-on-hook (lambda()
				    (setq buffer-face-mode-face '(:family "EB Garamond" :height 130))
				    (buffer-face-mode)))
(add-hook 'writeroom-mode-off-hook (lambda() (buffer-face-mode -1)))
(add-hook 'writeroom-mode-on-hook (lambda() (display-line-numbers-mode -1)))
(add-hook 'writeroom-mode-off-hook (lambda() (display-line-numbers-mode 1)))
(use-package ox-epub
  :after org)

(use-package powerthesaurus)

(defun justify-paragraph ()
  (interactive)
  (fill-paragraph 64))

(defun writeroom-dive-in ()
  (interactive)
  (writeroom-mode 1)
  (auto-fill-mode 1))

(defun writeroom-dive-out ()
  (interactive)
  (writeroom-mode -1)
  (auto-fill-mode -1))

(nvmap :keymaps 'override :prefix "SPC w"
  "i"  '(writeroom-dive-in :which-key "Dive in")
  "o"  '(writeroom-dive-out :which-key "Dive out")
  "w"  '(writeroom-mode :which-key "Toggle writeroom mode")
  "f"  '(auto-fill-mode :which-key "Toggle autofill mode")
  "j"  '(justify-paragraph :which-key "Justify text")
  "m"  '(flyspell-correct-word-before-point :which-key "Fix word")
  "c"  '(count-words :which-key "Count words")
  "u"  '(join-line :which-key "Join lines"))
