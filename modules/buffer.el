(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC b"
 "b" '(counsel-switch-buffer :which-key "Switch Buffers")
 "r" '(counsel-buffer-or-recentf :which-key "Recents")
 "c" '(clone-indirect-buffer-other-window :which-key "Clone")
 "n" '(next-buffer :which-key "Next Buffer")
 "p" '(previous-buffer :which-key "Previous Buffer")
 "k" '(delete-window :which-key "Delete Window")
 "d" '(kill-current-buffer :which-key "Kill Buffer"))

(general-define-key "M-<tab>" 'next-buffer)
(general-define-key "M-<iso-lefttab>" 'previous-buffer)
