; emacs.d/init.el

;;; Enable ido-mode
(ido-mode 1)
; ???
(setq ido-enable-flex-matching t)
; ???
(setq ido-everywhere t)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

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


;;; Packages
;;; (anti-zenburn-theme apache-mode bbdb cyberpunk-theme go-mode google-translate jinja2-mode js2-mode json json-mode less-css-mode markdown-mode mediawiki nginx-mode nzenburn-theme org osx-plist php-mode twilight-theme zenburn-theme)
(package-initialize)
;; check if a package is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	   (package-install package))))
 '(anti-zenburn-theme apache-mode cyberpunk-theme go-mode jinja2-mode js2-mode json json-mode less-css-mode markdown-mode nginx-mode nzenburn-theme osx-plist php-mode twilight-theme zenburn-theme))

;;; THEME
;;; Setta il color-theme (nuovo stile, emacs 24+)
(load-theme 'adwaita)
;(load-theme 'zenburn)

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

;; file mode
;; (add-to-list 'auto-mode-alist `(,(expand-file-name "~/Documents/appunti/") . markdown-mode))
(add-to-list 'auto-mode-alist '("/Users/sand/Documents/appunti/[^/]*\\.txt\\'" . markdown-mode))

;; miei script
(add-to-list 'load-path "~/elisp")

;; po-mode
(setq auto-mode-alist
      (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

;; golang
(defun my-go-mode-hook()
  ;;(add-hook 'before-save-hook 'gofmt-before-save)
  (setq default-tab-width 2))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; enable Multi Hops in TRAMP
;; aka: with this you can edit a remote file with sudo
;; C-x C-f /sudo:root@remote-host:/path/to-file
(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
	     '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
	     '((regexp-quote (system-name)) nil nil))

;;; Opzioni di (customize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1f4e6cf4cb3bdba8afdb9244e037698238080eeecb209084602f7d717225f102" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" "4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" "9f42bccce1e13fa5017eb8718574db099e85358b9f424db78e7318f86d1be08f" default)))
 '(org-agenda-files (quote ("~/Dropbox/org/diario.org" "~/Dropbox/org/bookmarks.org" "~/Dropbox/org/todo.org" "~/Dropbox/org/personal.org")))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
