(use-package org-roam
      :ensure t
      :custom
      (org-roam-directory zero/neuron)
      (org-roam-dailies-directory zero/org-journal-dir)
      (setq org-roam-completion-everywhere t)
      (setq org-roam-v2-ack t)
      (org-roam-db-location zero/neuron-db)
      :config
      (org-roam-setup)
      (require 'org-roam-protocol))

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)
(setq org-log-done 'time
    org-log-into-drawer t)

(use-package org-roam-ui
       :ensure t
       :config
       (setq org-roam-ui-sync-theme t
             org-roam-ui-follow t
             org-roam-ui-update-on-save t
             org-roam-ui-open-on-start nil))
(setq org-roam-v2-ack t)

(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

(defun org-roam-node-type (node)
  (let ((type (neuron-type node))) (if type (concat "<" type ">") "")))
(defun org-roam-node-genre (node)
  (let ((genre (neuron-genre node))) (if genre (concat ":" genre ":") "")))
(setq org-roam-node-display-template
      (concat
          "${title:*} "
	      (propertize "${type:16}" 'face 'org-level-6)
	      (propertize "${genre:16}" 'face 'org-level-8)
	      (propertize "${tags:28}" 'face 'org-tag)))

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC r" :which-key "Neuron"
       "f" '(neuron-find :which-key "Find")
       "d" '(neuron-find-self :which-key "Documentation")
       "i" '(org-roam-node-insert :which-key "Insert node")
       "I" '(neuron-node-insert-immediate :which-key "Insert immediately")
       "p" '(neuron-node-include :which-key "Include node")
       "u" '(neuron-find-undefined :which-key "Find undefined")
       "a" '(neuron-find-archived :which-key "Find Archived")
       "c" '(neuron-collection :which-key "Collection")
       "e" '(neuron-find-eternal :which-key "Find Eternal")
       "A" '(neuron-node-archive :which-key "Archive")
       "E" '(neuron-node-eternal :which-key "Eternal")
       "t" '(neuron-find-by-type :which-key "Find by type")
       "g" '(neuron-find-by-genre :which-key "Find by genre")
       "T" '(neuron-set-type :which-key "Set Type")
       "G" '(neuron-set-genre :which-key "Set Genre")
       "#" '(org-roam-tag-add :which-key "Add tag"))

(nvmap :keymaps 'override :prefix "SPC d"
  "d" '(neuron-today :which-key "Today")
  "t" '(neuron-tomorrow :which-key "Tomorrow")
  "y" '(neuron-yesterday :which-key "Yesterday")
  "g" '(neuron-daily :which-key "Goto date")
  "a" '(neuron-agenda-list :which-key "Agenda")
  "r" '(neuron-set-agenda :which-key "Set Agenda")
  "l" '(neuron-agenda-todos :which-key "Agenda Todos"))

(defun neuron-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(setq neuron-type "Neuron-Type"
      neuron-genre "Neuron-Genre"
      neuron-documentation "Neuron-Documentation"
      neuron-publish "Neuron-Publish"
      neuron-daily "Daily"
      neuron-daily-dir "Dailies"
      neuron-archived "Archived"
      neuron-eternal "Eternal")

(defun neuron-property (node key)
  (cdr (assoc key (org-roam-node-properties node))))
(defun neuron-type (node) (neuron-property node "NEURON_TYPE"))
(defun neuron-genre (node) (neuron-property node "NEURON_GENRE"))
(defun neuron-state (node) (neuron-property node "NEURON_STATE"))
(defun neuron--agenda (node) (neuron-property node "NEURON_AGENDA"))

(defun neuron-set-type (type)
  (interactive 
   (let ((completion-ignore-case  t))
     (list (completing-read "Type: " neuron-types nil t))))
  (org-set-property "NEURON_TYPE" type))

(defun neuron-set-agenda ()
  (interactive)
  (org-set-property "NEURON_AGENDA" "t"))

(defun neuron-set-genre (genre)
  (interactive 
    (let ((completion-ignore-case  t))
     (list (completing-read "Genre: " neuron-genres nil t))))
  (org-set-property "NEURON_GENRE" genre))

(defun neuron-node-eternal ()
  (interactive)
  (org-set-property "NEURON_STATE" neuron-eternal))

(defun neuron-node-archive ()
  (interactive)
  (org-set-property "NEURON_STATE" neuron-archived))

(defun neuron-node-link (node)
  (concat "[[id:" (org-roam-node-id node) "][" (org-roam-node-title node)  "]]"))

(defun neuron-node-from-type (type)
  (-first (-map (lambda (node) (neuron--filter-type type node)) (org-roam-node-list))))

(defun neuron-node-archived-p (node) (string= neuron-archived (neuron-state node)))
(defun neuron-node-eternal-p (node) (string= neuron-eternal (neuron-state node)))

(use-package dash)
(defun neuron--node-title-list-with-type (type)
  (-map
   'org-roam-node-title
   (-filter
    (lambda (node) (neuron--filter-type type node))
     (org-roam-node-list))))
(defun neuron--node-title-list-with-genre (genre)
  (-map
   'org-roam-node-title
   (-filter
    (lambda (node) (neuron--filter-genre genre node))
    (org-roam-node-list))))
(defun neuron--filter-type (type node)
  (string= type (neuron-type node)))
(defun neuron--filter-genre (genre node)
  (string= genre (neuron-genre node)))
(defun neuron--filter-daily (node)
  (string= neuron-daily (neuron-type node)))
(defun neuron--filter-archived (node)
  (string= neuron-archived (neuron-state node)))
(defun neuron--filter-eternal (node)
  (string= neuron-eternal (neuron-state node)))
(defun neuron--filter-self (node)
  (string-prefix-p "Neuron" (neuron-type node)))

(defun neuron-documentation()
  (interactive)
  (switch-to-buffer (find-file-noselect (org-roam-node-file (neuron-node-from-type neuron-documentation)) nil nil t)))
(defun neuron-setup()
  (interactive)
  (setq neuron-types (neuron--node-title-list-with-type neuron-type)
	neuron-genres (neuron--node-title-list-with-type neuron-genre)))
(neuron-setup)

(defun neuron-find-self (type &optional other-window initial-input &key templates)
  (interactive "P")
  (org-roam-node-find
      other-window
      initial-input
      'neuron--filter-self))

(defun neuron-find (type &optional other-window initial-input &key templates)
  (interactive "P")
  (org-roam-node-find
      other-window
      initial-input
      (lambda(node)
	(not (or (neuron--filter-archived node)
		 (neuron--filter-self node)
		(neuron--filter-daily node))))))

(defun neuron-find-archived (type &optional other-window initial-input &key templates)
  (interactive "P")
  (org-roam-node-find
      other-window
      initial-input
      'neuron--filter-archived
      ))

(defun neuron-find-eternal (type &optional other-window initial-input &key templates)
  (interactive "P")
  (org-roam-node-find
      other-window
      initial-input
      'neuron--filter-eternal
      ))

(defun neuron-find-by-genre (genre &optional other-window initial-input)
  (interactive
   (let ((completion-ignore-case  t))
     (list (completing-read "Genre: " neuron-genres nil t))))
  (org-roam-node-find
      other-window
      initial-input
      (lambda (node)
	(and (not (neuron-node-archived-p node)) (string= genre (neuron-genre node))))
      :templates '()
      ))

(defun neuron-find-by-type (type &optional other-window initial-input)
  (interactive
   (let ((completion-ignore-case  t))
     (list (completing-read "Type: " neuron-types nil t))))
  (org-roam-node-find
      other-window
      initial-input
      (lambda (node)
	(and (not (neuron-node-archived-p node)) (string= type (neuron-type node))))
      :templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" ":PROPERTIES:
:NEURON_TYPE: %(eval type)
:END:
#+title: ${title}")
           :unnarrowed t))
      ))

(defun neuron-find-undefined (&optional other-window initial-input)
  (interactive "P")
  (org-roam-node-find
      other-window
      initial-input
      (lambda (node)
	(if (neuron-type node) nil t))
      :templates '()
      ))

(defun neuron-today ()
  (interactive
   (neuron-daily
    (shell-command-to-string "echo -n $(date '+%B %d %Y')"))))

(defun neuron-tomorrow ()
  (interactive
   (neuron-daily
    (shell-command-to-string "echo -n $(date --date='tomorrow' '+%B %d %Y')"))))

(defun neuron-yesterday ()
  (interactive
   (neuron-daily
    (shell-command-to-string "echo -n $(date --date='yesterday' '+%B %d %Y')"))))

(defun neuron-daily (&optional initial-input)
  (interactive)
  (org-roam-node-find
   nil
   initial-input
   (lambda (node)
	(and (not (neuron-node-archived-p node)) (string= neuron-daily (neuron-type node))))
   :templates
        '(("d" "default" plain "%?"
           :if-new (file+head "Journal/%<%Y-%m-%d>.org" ":PROPERTIES:
:NEURON_TYPE: Daily
:END:
#+title: ${title}")
	   ))))

(defun neuron--agenda-files ()
  (-map 'org-roam-node-file (-filter 'neuron--agenda (org-roam-node-list))))

(defun neuron-agenda-list ()
  (interactive)
  (let ((org-agenda-files (neuron--agenda-files)))
	 (org-agenda-list)))

(defun neuron-agenda-todos ()
  (interactive)
  (let ((org-agenda-files (neuron--agenda-files)))
	 (org-todo-list)))
