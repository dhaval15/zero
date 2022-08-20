(use-package kaolin-themes)
(setq kaolin-themes-bold t       ; If nil, disable the bold style.        
      kaolin-themes-italic t     ; If nil, disable the italic style.
      kaolin-themes-underline nil
      kaolin-themes-distinct-metakeys nil
      kaolin-themes-modeline-border nil)
(kaolin-treemacs-theme)
(load-theme 'kaolin-valley-dark t)

;;(set-face-attribute 'default nil
  ;;:font "EB Garamond"
  ;;:font "Source Code Pro"
  ;;:height 115
  ;;:weight 'medium)
;;(set-face-attribute 'variable-pitch nil
  ;;:font "EB Garamond"
  ;;:height 115
  ;;:weight 'medium)
;; (set-face-attribute 'fixed-pitch nil
;;   :font "Source Code Pro"
;;   :height 115
;;   :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.04)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "Source Code Pro"))
;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
