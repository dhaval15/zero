;;#+TITLE: Meow Configuration

;; Install

(use-package meow)
(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

;; Fast Movement

(defun meow-goto-top ()
  (interactive)
  (meow-beginning-of-thing 'buffer))
(defun meow-goto-bottom ()
  (interactive)
  (meow-end-of-thing 'buffer))

;; Better Insertion

(defun meow-open-at-start ()
  (interactive)
  (meow-join t)
  (meow-append)
  (backward-char 1))
(defun meow-open-at-end ()
  (interactive)
  (meow-line 1)
  (meow-append))
(defun kr-meow-append ()
  "Make `meow-append' behavior similar to evil-append."
  (unless (= (line-end-position) (point))
      (forward-char 1)))
(advice-add 'meow-append :after #'kr-meow-append)
(defun meow-yank-vi ()
  (interactive)
  ;;  "Make `meow-yank' behave simmilar to yank in vi"
  (forward-line 1)
  (meow-yank))

;; Motion Mode

(meow-motion-overwrite-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 '("<escape>" . ignore))

;; Keypad Mode

(meow-leader-define-key
 ;; SPC j/k will run the original command in MOTION state.
 '("j" . "H-j")
 '("k" . "H-k")
 '("e" . "C-z C-e")
 '("o" . "C-z C-o")
 '("w" . "C-z C-w")
 '("b" . "C-z C-b")
 '("r" . "C-z C-r")
 '("f" . "C-z C-f")
 ;; Use SPC (0-9) for digit arguments.
 '("1" . meow-digit-argument)
 '("2" . meow-digit-argument)
 '("3" . meow-digit-argument)
 '("4" . meow-digit-argument)
 '("5" . meow-digit-argument)
 '("6" . meow-digit-argument)
 '("7" . meow-digit-argument)
 '("8" . meow-digit-argument)
 '("9" . meow-digit-argument)
 '("0" . meow-digit-argument)
 '("/" . meow-keypad-describe-key)
 '("?" . meow-cheatsheet))

;; Normal Mode

(meow-normal-define-key
 '("0" . meow-expand-0)
 '("9" . meow-expand-9)
 '("8" . meow-expand-8)
 '("7" . meow-expand-7)
 '("6" . meow-expand-6)
 '("5" . meow-expand-5)
 '("4" . meow-expand-4)
 '("3" . meow-expand-3)
 '("2" . meow-expand-2)
 '("1" . meow-expand-1)
 '("-" . negative-argument)
 '("," . meow-reverse)
 '(";" . meow-inner-of-thing)
 '(":" . execute-extended-command)
 '("'" . meow-bounds-of-thing)
 '("[" . meow-beginning-of-thing)
 '("]" . meow-end-of-thing)
 '("<" . meow-block)
 '(">" . meow-to-block)
 '("/" . meow-visit)
 '("\\" . meow-cancel-selection)
 '("|" . meow-grab)
 '("a" . meow-append)
 '("A" . meow-open-at-end)
 '("b" . meow-back-word)
 '("B" . meow-back-symbol)
 '("c" . meow-change)
 '("d" . meow-kill)
 '("D" . meow-backward-delete)
 '("e" . meow-next-word)
 '("E" . meow-next-symbol)
 '("f" . meow-find)
 '("g" . meow-goto-bottom)
 '("G" . meow-goto-top)
 '("h" . meow-left)
 '("H" . meow-left-expand)
 '("i" . meow-insert)
 '("I" . meow-open-at-start)
 '("j" . meow-next)
 '("J" . meow-next-expand)
 '("k" . meow-prev)
 '("K" . meow-prev-expand)
 '("l" . meow-right)
 '("L" . meow-right-expand)
 '("m" . meow-join)
 '("n" . meow-search)
 '("o" . meow-open-below)
 '("O" . meow-open-above)
 '("p" . meow-yank-vi)
 '("P" . meow-yank)
 '("q" . meow-grab)
 '("Q" . meow-goto-line)
 '("r" . meow-replace)
 '("R" . meow-swap-grab)
 '("t" . meow-till)
 '("u" . meow-undo)
 '("U" . meow-undo-in-selection)
 '("v" . meow-line)
 '("V" . meow-visual-mode)
 '("w" . meow-mark-word)
 '("W" . meow-mark-symbol)
 '("x" . meow-delete)
 '("X" . meow-backward-delete)
 '("y" . meow-save)
 '("Y" . meow-sync-grab)
 '("z" . meow-pop-selection)
 '("Z" . clipboard-yank)
 '("." . repeat)
 '("<backspace>" . meow-left)
 '("<return>" . meow-next)
 '("<escape>" . ignore))
(meow-global-mode 1)


;; TODO Visual Line Mode

(setq meow-visual-keymap (make-keymap))
(meow-define-state visual
  "meow state for fast movement"
  :lighter " [ P]"
  :keymap meow-visual-keymap)

;; meow-define-state creates the variable
(setq meow-cursor-type-visual 'hollow)

(meow-define-keys 'visual
  '("<escape>" . meow-normal-mode)
  '("h" . meow-left-expand)
  '("l" . meow-right-expand)
  '("j" . meow-next-expand)
  '("k" . meow-prev-expand)
  '("c" . meow-change)
  '("d" . meow-kill)
  '("y" . meow-save))

