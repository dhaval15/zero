(setq pheonix-public-dir "~/Dev/OrgFiles/PheonixWeb")
(setq pheonix-pheonix-file "~/Dev/OrgFiles/Pheonix.org")
(setq pheonix-index-file "index.org")
(setq pheonix-publish-dir "~/Dev/OrgFiles/Pheonix")

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC" :which-key "Pheonix"
       "p b" '(pheonix--build :which-key "Build")
       "p p" '(pheonix--index-file :which-key "Pheonix")
       "p i" '(pheonix--insert-page :which-key "Insert Page")
       "p t" '(pheonix--set-with-title :which-key "With title")
       "p o" '(pheonix--set-only-contents :which-key "Only contents")
       "p g" '(pheonix--set-group :which-key "Group")
       "p s" '(pheonix--set-search :which-key "Search"))

(defun pheonix--build ()
  (interactive)
  (save-excursion
    (switch-to-buffer (find-file-noselect pheonix-pheonix-file nil nil t))
    (pheonix--build-index (pheonix--parse))
    (save-buffer)
    (kill-buffer)))

(defun pheonix--index-file ()
  (interactive)
  (switch-to-buffer (find-file-noselect pheonix-pheonix-file nil nil t)))

(defun pheonix--insert-page ()
  (interactive)
  (let ((pheonix-node (org-roam-node-read)))
    (zero--insert-line (format "* %s" (org-roam-node-title pheonix-node)))
    (zero--insert-line ":PROPERTIES:")
    (zero--insert-line ":PHEONIX_PUBLISH: t")
    (zero--insert-line (format ":PHEONIX_ID: %s" (org-roam-node-id pheonix-node)))
    (zero--insert-line ":END:")))

(defun pheonix--set-with-title ()
  (interactive)
  (org-set-property "PHEONIX_WITH_TITLE" "t"))

(defun pheonix--set-only-contents ()
  (interactive)
  (org-set-property "PHEONIX_ONLY_CONTENTS" "t"))

(defun pheonix--set-group (group)
  (interactive 
    (let ((completion-ignore-case  t))
     (list (completing-read "Group: " nil nil t))))
  (org-set-property "PHEONIX_GROUP" group))

(defun pheonix--set-search (search)
  (interactive 
    (let ((completion-ignore-case  t))
     (list (completing-read "Search: " nil nil t))))
  (org-set-property "PHEONIX_SEARCH" search))

(defun pheonix--parse ()
  (-group-by
   'pheonix--group
   (-filter
    (lambda (entry) (car entry))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
	(list
	 (org-element-property :PHEONIX_PUBLISH headline) ;; 0
	 (org-element-property :raw-value headline) ;; 1
	 (org-element-property :PHEONIX_GROUP headline) ;; 2
	 (org-element-property :PHEONIX_ID headline) ;; 3
	 (org-element-property :PHEONIX_SEARCH headline) ;; 4
	 (org-element-property :PHEONIX_WITH_TITLE headline) ;; 5
	 (org-element-property :PHEONIX_ONLY_CONTENTS headline) ;; 6
	 ))))))

(defun pheonix--title (headline) (nth 1 headline))
(defun pheonix--group (headline) (nth 2 headline))
(defun pheonix--id (headline) (nth 3 headline))
(defun pheonix--search (headline) (nth 4 headline))
(defun pheonix--with-title (headline) (nth 5 headline))
(defun pheonix--only-contents (headline) (nth 6 headline))

(defun pheonix--build-index (groups)
  (save-window-excursion
    (switch-to-buffer (find-file-noselect (expand-file-name pheonix-index-file pheonix-publish-dir) nil nil t))
    (erase-buffer)
    (insert "#+TITLE: Pheonix")
    (cl-loop for group in groups
	     do (progn
		  (zero--insert-line (pheonix--group-link (car group)))
		  (pheonix--build-group (car group) (cdr group))))
    (save-buffer)
    (kill-buffer)))

(defun pheonix--group-link (group) 
    (format "* [[file:%s][%s]]" (pheonix--build-group-file-name group) group))

(defun pheonix--build-group (group headlines)
  (save-window-excursion
    (switch-to-buffer (find-file-noselect (pheonix--build-group-file-path group) nil nil t))
    (erase-buffer)
    (insert (format "#+TITLE: %s" group))
    (cl-loop for headline in headlines
	     do (progn
		  (zero--insert-line (format "* %s" (pheonix--build-link-from-headline headline))))
		  (pheonix--build-page-from-headline headline))
    (save-buffer)
    (kill-buffer)))

(defun pheonix--build-group-file-name (group) 
    (format "%s.org" (s-replace " " "_" group)))

(defun pheonix--build-group-file-path (group) 
    (expand-file-name (pheonix--build-group-file-name group) pheonix-publish-dir))

(defun pheonix--build-link-from-headline (headline)
  (pheonix--build-link
   (pheonix--id headline)
   (pheonix--title headline)))

(defun pheonix--build-page-from-headline (headline)
  (pheonix--build-page
   (pheonix--id headline)
   (pheonix--title headline)
   (pheonix--search headline)
   (pheonix--with-title headline)
   (pheonix--only-contents headline)))

(defun pheonix--build-page (id title search with-title only-contents)
  (let ((page-file-name (pheonix--build-page-file-path id title)))
  (save-window-excursion
    (switch-to-buffer (find-file-noselect page-file-name nil nil t))
    (erase-buffer)
    (insert (pheonix--build-include-content id title search with-title only-contents))
    (save-buffer)
    (kill-buffer))))

(defun pheonix--build-link (id title) 
    (format "[[file:%s][%s]]" (pheonix--build-page-file-name id title) title))

(defun pheonix--build-page-file-path (id title) 
    (expand-file-name (pheonix--build-page-file-name id title) pheonix-publish-dir))

(defun pheonix--build-page-file-name (id title) 
    (s-replace " " "_" (format "%s_%s.org" title id)))

(defun pheonix--build-include-content (id title search with-title only-contents)
  (let ((content-file (org-roam-node-file (org-roam-node-from-id id))))
    (concat
     (if with-title (format "#+TITLE: %s\n" title) nil)
     (format "#+INCLUDE: \"%s" content-file)
     (if search (format "::%s" search) nil)
     "\""
     (if only-contents " :only-contents t" nil))))
