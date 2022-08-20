(defun counsel-find-file-at-home ()
  (interactive)
  (counsel-find-file zero/org-dir))

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC f" :which-key "Quick Access"
       "f h" '(counsel-find-file-at-home :which-key "Find files")
       "f f" '(counsel-find-file :which-key "Find files"))
