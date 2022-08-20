(winner-mode 1)
(nvmap :prefix "SPC g" :which-key "Window"
       ;; Window splits
       "c"   '(evil-window-delete :which-key "Close window")
       "n"   '(evil-window-new :which-key "New window")
       "s"   '(evil-window-split :which-key "Horizontal split window")
       "v"   '(evil-window-vsplit :which-key "Vertical split window")
       ;; Window motions
       "h"   '(evil-window-left :which-key "Window left")
       "j"   '(evil-window-down :which-key "Window down")
       "k"   '(evil-window-up :which-key "Window up")
       "l"   '(evil-window-right :which-key "Window right")
       "w"   '(evil-window-next :which-key "Goto next window")
       ;; winner mode
       "<left>"  '(winner-undo :which-key "Winner undo")
       "<right>" '(winner-redo :which-key "Winner redo"))
