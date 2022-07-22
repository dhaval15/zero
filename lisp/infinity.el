;; Defifed variables
(setq infinity-types '("Node" "Document" "Daily" "Box" "Meta")
      infinity-genres '("Research Paper" "Fiction" "Poem" "Assignment" "Thought" "Idea" "Project" "Review" "Dream")
      infinity-archived "Archived"
      infinity-eternal "Eternal")


(defun infinity-find (type &optional other-window initial-input &key templates)
  (interactive "P")
  (org-roam-node-find
      other-window
      initial-input
      (lambda (node)
	(not (infinity-node-archived-p node)))
      ))

(defun infinity-find-archived (type &optional other-window initial-input &key templates)
  (interactive "P")
  (org-roam-node-find
      other-window
      initial-input
      (lambda (node)
	(infinity-node-archived-p node))
      ))

(defun infinity-find-eternal (type &optional other-window initial-input &key templates)
  (interactive "P")
  (org-roam-node-find
      other-window
      initial-input
      (lambda (node)
	(infinity-node-eternal-p node))
      ))

(defun infinity-find-by-type (type &optional other-window initial-input)
  (org-roam-node-find
      other-window
      initial-input
      (lambda (node)
	(and (not (infinity-node-archived-p node)) (string= type (infinity-type node))))
      :templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" ":PROPERTIES:
:INFINITY_TYPE: %(eval type)
:END:
#+title: ${title}")
           :unnarrowed t))
      ))


(defun infinity-find-by-genre (genre &optional other-window initial-input)
  (interactive
   (let ((completion-ignore-case  t))
     (list (completing-read "Genre: " infinity-genres nil t))))
  (org-roam-node-find
      other-window
      initial-input
      (lambda (node)
	(and (not (infinity-node-archived-p node)) (string= genre (infinity-genre node))))
      :templates '()
      ))

(defun infinity-find-by-type (type &optional other-window initial-input)
  (interactive
   (let ((completion-ignore-case  t))
     (list (completing-read "Type: " infinity-types nil t))))
  (org-roam-node-find
      other-window
      initial-input
      (lambda (node)
	(and (not (infinity-node-archived-p node)) (string= type (infinity-type node))))
      :templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" ":PROPERTIES:
:INFINITY_TYPE: %(eval type)
:END:
#+title: ${title}")
           :unnarrowed t))
      ))

(defun infinity-find-undefined (&optional other-window initial-input)
  (interactive "P")
  (org-roam-node-find
      other-window
      initial-input
      (lambda (node)
	(if (infinity-type node) nil t))
      :templates '()
      ))

(defun infinity-type (node) (infinity-property node "INFINITY_TYPE"))
(defun infinity-genre (node) (infinity-property node "INFINITY_GENRE"))
(defun infinity-state (node) (infinity-property node "INFINITY_STATE"))

(defun infinity-node-archived-p (node) (string= infinity-archived (infinity-state node)))
(defun infinity-node-eternal-p (node) (string= infinity-eternal (infinity-state node)))

(defun infinity-property (node key)
  (cdr (assoc key (org-roam-node-properties node))))

(defun infinity-set-type (type)
  (interactive 
   (let ((completion-ignore-case  t))
     (list (completing-read "Type: " infinity-types nil t))))
  (org-set-property "INFINITY_TYPE" type))

(defun infinity-set-genre (genre)
  (interactive 
    (let ((completion-ignore-case  t))
     (list (completing-read "Genre: " infinity-genres nil t))))
  (org-set-property "INFINITY_GENRE" genre))


(defun infinity-node-eternal ()
  (interactive)
  (org-set-property "INFINITY_STATE" infinity-eternal))

(defun infinity-node-archive ()
  (interactive)
  (org-set-property "INFINITY_STATE" infinity-archived))

(message (org-roam-ui--get-links))
