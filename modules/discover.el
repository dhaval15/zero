(setq discover--types '("Pdf" "Doc" "Book" "Link"))
(setq discover--db-location "~/Dev/OrgFiles/Discover/index.db")
(setq discover--index-dir "~/Dev/OrgFiles/Discover/")
(defun discover--index-files ()
  (directory-files-recursively discover--index-dir "\\.org$"))
(setq discover--db (emacsql-sqlite discover--db-location))
(defun discover--db-query (sql &rest args)
  (apply #'emacsql discover--db sql args))

(defun discover--db-query! (handler sql &rest args)
  "Run SQL query on Org-roam database with ARGS.
SQL can be either the emacsql vector representation, or a string.
The query is expected to be able to fail, in this situation, run HANDLER."
  (condition-case err
      (discover--db-query sql args)
    (emacsql-constraint
     (funcall handler err))))

(defun discover--init()
 (interactive)
 (discover--db-query!
  (lambda (err)
             (lwarn 'org-roam :warning "%s for %s (%s) in %s"
                    (error-message-string err)
                    title id file))
	   [:create-table nodes
             ([(id string :primary-key) (title string) (file string) (type string) tags])]))

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC t" :which-key "Discover"
       "f" '(discover-node-find :which-key "Find")
       "i" '(discover-node-insert :which-key "Insert")
       "t" '(discover-index-find :which-key "Index")
       "s" '(org-roam-node-insert :which-key "Sync"))

(defun discover--id (heading) (car heading))
(defun discover--title (heading) (nth 1 heading))
(defun discover--file (heading) (nth 2 heading))
(defun discover--type (heading) (nth 3 heading))
(defun discover--tags (heading)
  (let ((tag-string (nth 4 heading)))
  (if tag-string
      (split-string
       (substring tag-string 1 -1) ":")
    nil)))

(defun discover--index ()
    (-map 'vconcat (zero-org--collect-headings '("DISCOVER_ID" "ITEM" "DISCOVER_FILE" "DISCOVER_TYPE" "TAGS") "DISCOVER_ID")))

(defun discover-sync ()
  (interactive)
  (save-window-excursion
    (dolist (file (discover--index-files))
    (switch-to-buffer (find-file-noselect file nil nil t))
    (dolist (item (discover--index))
	     (discover--db-query!
           (lambda (err)
             (lwarn 'org-roam :warning "%s"
                    (error-message-string err)))
	      [:insert
	       :into nodes
	       :values $v1]
	      item)))))

(defun discover--node-list ()
  (discover--db-query [:select *
             :from nodes]))

(defun discover--node-read-completions ()
  "Return an alist for node completion.
The car is the displayed title or alias for the node, and the cdr
is the `discover-node'.
The displayed title is formatted according to `discover-node-display-template'."
  ;;(setq org-roam-node-read--cached-display-format nil)
  (let ((nodes (discover--node-list)))
    (mapcar #'discover--node-read-to-candidate nodes)))

(defun discover--node-read-to-candidate (node)
  "Return a minibuffer completion candidate given NODE."
  (let ((candidate-main (discover--title node)))
    (cons (propertize candidate-main 'node node) node)))

(defun discover--node-read ()
   (let* ((completion-ignore-case  t)
	 (nodes (discover--node-read-completions))
	 (node (completing-read "Discover: " nodes nil t)))
     (cdr (assoc node nodes))))

(defun discover-node-find ()
  (interactive)
  (let ((node (discover--node-read)))
    (discover--open-file (discover--file node) (discover--type node))))

(defun discover-node-insert (title)
  (interactive (list (read-string "Title: ")))
  (let ((discover-id (org-id-new))
	(discover-file-path (counsel-pick-file-path))
	(type (completing-read "Type: " discover--types nil t)))
    (zero--insert-line (format "* %s" title))
    (zero--insert-line ":PROPERTIES:")
    (zero--insert-line (format ":DISCOVER_ID: %s" (org-id-new)))
    (zero--insert-line (format ":DISCOVER_FILE: %s"  discover-file-path))
    (zero--insert-line (format ":DISCOVER_TYPE: %s"  type))
    (zero--insert-line ":END:")))

(defun discover--files ()
 (-map
  (lambda (path) (substring (car (last (split-string path "/"))) 0 -4))
  (directory-files-recursively
   discover--index-dir "\\.org$")))

(defun discover--open-file (file type)
  (let ((command (cdr (assoc type discover--commands))))
    (if command (call-process-shell-command (format "%s \"%s\"" command file) nil 0))))

;;shell-command-to-string
(setq discover--commands '(("Pdf" . "zathura")
			   ("Book" . "zathura")
			   ("Link" . "chromium")
			   ("Doc" . "libreoffice")))

(defun discover-index-find (module)
  (interactive
   (let ((completion-ignore-case  t))
     (list (completing-read "Index: " (discover--files) nil t))))
  (switch-to-buffer
   (find-file-noselect
    (expand-file-name
     (format "%s.org" module)
     discover--index-dir)
    nil nil t)))
