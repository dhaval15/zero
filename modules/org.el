(defun org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(defun org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (interactive)
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-on-heading-p))
      (goto-char pos)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(general-define-key "C-<tab>" 'org-show-next-heading-tidily)
(general-define-key "C-<iso-lefttab>" 'org-show-previous-heading-tidily)
(nvmap :keymaps 'override :prefix "SPC o"
       "]"   '(org-show-next-heading-tidily :which-key "Next Heading")
       "["   '(org-show-previous-heading-tidily :which-key "Previous Heading")
       "*"   '(org-ctrl-c-star :which-key "Org-ctrl-c-star")
       "+"   '(org-ctrl-c-minus :which-key "Org-ctrl-c-minus")
       "."   '(counsel-org-goto :which-key "Counsel org goto")
       "e"   '(org-export-dispatch :which-key "Org export dispatch")
       "f"   '(org-footnote-new :which-key "Org footnote new")
       "h"   '(org-toggle-heading :which-key "Org toggle heading")
       "i"   '(org-insert-link :which-key "Org insert link")
       "n"   '(org-store-link :which-key "Org store link")
       "o"   '(org-open-at-point :which-key "Org follow link")
       "p"   '(org-set-property :which-key "Org set property")
       "t"   '(org-todo :which-key "Org todo")
       "x"   '(org-toggle-checkbox :which-key "Org toggle checkbox")
       "B"   '(org-babel-tangle :which-key "Org babel tangle")
       "I"   '(org-toggle-inline-images :which-key "Org toggle inline imager")
       "T"   '(org-todo-list :which-key "Org todo list")
       "a"   '(org-agenda :which-key "Org agenda")
       "c"   '(org-capture :which-key "Capture")
       "s"   '(org-schedule :which-key "Org Schedule")
       "l"   '(org-cliplink :which-key "Org Link from clipboard"))

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-directory zero/org-dir
      org-agenda-files (directory-files-recursively zero/org-agenda-dir "\\.org$")
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-ellipsis " ... "
      org-log-done 'time
      org-refile-targets '(("/home/dhaval/Dev/Write/archives/daily.org" :maxlevel . 1)
                           ("/home/dhaval/Dev/Write/archives/tasks.org" :maxlevel . 1))
      org-journal-dir zero/org-journal-dir
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org"
      org-hide-emphasis-markers t)
(setq org-src-preserve-indentation nil
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)
(setq org-startup-folded t)

(use-package org-bullets
  :config
    (setq org-bullets-bullet-list '("●" "○" "◆" "◇" "•")))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; An example of how this works.
;; [[arch-wiki:Name_of_Page][Description]]
(setq org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
        '(("google" . "http://www.google.com/search?q=")
          ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
          ("ddg" . "https://duckduckgo.com/?q=")
          ("wiki" . "https://en.wikipedia.org/wiki/")))



(setq org-todo-keywords        
        '((sequence
           "TODO(t)"           
           "REVIEW(r)"
	       "IDEA(i)"
	       "THOUGHT(b)"
	       "ZETTLE(z)"
	       "ORGANIZE(o)"
           "|"                 
           "DONE(d)"
           "CANCELLED(c)" )))

(use-package org-tempo
  :config
    (setq org-tempo-keywords-alist
      '(("L" . "latex")
        ("H" . "html")
        ("A" . "ascii")
        ("i" . "index")
        ("T" . "title")
        ("S" . "subtitle")))
  :ensure nil) ;; tell use-package not to try to install org-tempo since it's already there.

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(setq org-blank-before-new-entry (quote ((heading . nil)
                                         (plain-list-item . nil))))

(use-package ox-man
  :ensure nil)
(setq org-export-html-style-include-scripts nil
       org-export-html-style-include-default nil)
(setq org-html-head zero/org-html-css)

(setq org-capture-templates
   '(("K" "Cliplink capture task" entry (file "bookmarks.org")
      "* LINK %(org-cliplink-capture) \n" :empty-lines 1)))

(setq org-export-html-validation-link nil)

(use-package org-super-agenda)
(org-super-agenda-mode t)
(setq org-agenda-prefix-format
      '((agenda . " %i %?-12t% s")
	(todo . " %i %-12:c")
	(tags . " %i %-12:c")
	(search . " %i %-12:c")))
(setq org-super-agenda-groups '((:auto-category t)))
(setq spacemacs-theme-org-agenda-height nil
      org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t)

(use-package org-view-mode
  :ensure t)
