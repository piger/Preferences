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
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
; capture-file
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-todo-keywords
      '((sequence "TODO" "VERIFY" "|" "DONE" "DELEGATED")))
(setq org-tags-alist
      '((sequence "work" "personal" "computer" "blog")))
;; mobile org
(setq org-mobile-directory "~/Dropbox/org/mobile")
(setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))

;; repositories
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmelade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; Opzioni di (customize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" "9f42bccce1e13fa5017eb8718574db099e85358b9f424db78e7318f86d1be08f" default)))
 '(org-agenda-files (quote ("~/Dropbox/org/bookmarks.org" "~/Dropbox/org/todo.org" "~/Dropbox/org/personal.org")))
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
