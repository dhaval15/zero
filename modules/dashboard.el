(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-banner-logo-title "Let's Get Back To What's Important"
	dashboard-startup-banner zero/zero-logo
	dashboard-center-content t
	dashboard-items '((recents . 16)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
			      (bookmarks . "book"))))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(setq recentf-exclude '("/home/dhaval/Dev/dots/zero/*" "*.git/*" "/home/dhaval/Dev/OrgFiles/DigitalGarden/*"))
