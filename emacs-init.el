; emacs.d/init.el

;;; Enable ido-mode
(ido-mode 1)
; ???
(setq ido-enable-flex-matching t)
; ???
(setq ido-everywhere t)

;;; RETURN -> indent (come fa C-j)
; (define-key global-map (kbd "RET") 'newline-and-indent)

; 29.3 Tabs vs. Spaces
; per disattivare l'inserimento di <TAB> settare `indent-tabs-mode` a `nil`.
; (ricorda che un tab si inserisce con M-i)
(setq-default indent-tab-mode nil)
;; (setq-default tab-width 4)

; nasconde la toolbar
(tool-bar-mode -1)
; nasconde la scroll bar (perchè mai?)
; (scroll-bar-mode -1)

;;; Setta il color-theme (nuovo stile, emacs 24+)
(load-theme 'adwaita)

;; font
; (add-to-list 'default-frame-alist
; 	     '(font . "DejaVu Sans Mono-13"))

;;; org-mode
; general setup
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
; capture-file
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-todo-keywords
      '((sequence "TODO" "VERIFY" "|" "DONE" "DELEGATED")))

;;; Opzioni di (customize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/org/todo.org")))
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
