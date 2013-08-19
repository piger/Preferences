;;; emacs.d/init.el
;;;
;;; - https://github.com/magnars/.emacs.d/blob/master/init.el
;;; - http://www.emacswiki.org/emacs/ImenuMode

;;; nasconde la toolbar; e' bene farlo all'inizio per evitare che venga mostrata brevemente
(tool-bar-mode -1)
;;; nasconde la scroll bar (perchè mai?)
; (scroll-bar-mode -1)
 
;;; OS X?
(setq is-mac (equal system-type 'darwin))

;;; emacs custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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
;(setq-default indent-tab-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default cperl-indent-level 4)

;;; Packages
;;; (anti-zenburn-theme apache-mode bbdb cyberpunk-theme go-mode google-translate jinja2-mode js2-mode json json-mode less-css-mode markdown-mode mediawiki nginx-mode nzenburn-theme org osx-plist php-mode twilight-theme zenburn-theme)

;; repositories
(require 'package)
;;; XXX questo setq sarebbe meglio trasformarlo in un add-to-list.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmelade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

; fetch the list of packages available
;; QUESTO E' SBAGLIATO. Non viene chiamato automaticamente e quindi l'installazione fallisce
;; perche' non ha refreshato la lista di pacchetti disponibili (tenendo in considerazione i
;; repository aggiunti qui sopra)
;;(when (not package-archive-contents)
;;  (package-refresh-contents))

;; check if a package is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	   (package-install package))))
 '(anti-zenburn-theme apache-mode cyberpunk-theme go-mode jinja2-mode js2-mode json json-mode less-css-mode markdown-mode nginx-mode nzenburn-theme osx-plist php-mode twilight-theme zenburn-theme))


;; miei script
(add-to-list 'load-path "~/elisp")

;;; themes
(add-to-list 'load-path "~/elisp/themes/tomorrow-theme")
(add-to-list 'custom-theme-load-path "~/elisp/themes/base16-theme")
(add-to-list 'custom-theme-load-path "~/elisp/themes/tomorrow-theme")

;;; THEME
;;; Setta il color-theme (nuovo stile, emacs 24+)
;(load-theme 'adwaita)
;(load-theme 'zenburn)
;(load-theme 'base16-default)
(load-theme 'tomorrow-night)


;;; Fonts
;;; questo non saprei cosa fa:
; (add-to-list 'default-frame-alist
; 	     '(font . "DejaVu Sans Mono-13"))

;;; Per i font bisogna personalizzare custom.el
;;; (custom-set-faces
;;;  '(default ((t (:height 120 :family "Cousine"))))
;;; )


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


;; Assign a specific mode for certain directories
;; note: you can't chain multiple paths in a single add-to-list call :(
(add-to-list 'auto-mode-alist '("/Documents/appunti/[^/]*\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("/Preferences/zsh/" . shell-script-mode))

;; auto-fill per rst-mode
(add-hook 'rst-mode-hook 'turn-on-auto-fill)

;; web-mode
;; http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))
(setq web-mode-engines-alist
	  '(("django"		. "/templates/.*\\.html\\'")
		("ctemplate"	. "/webui/index\\.html\\'"))
)

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; handlebars
(require 'handlebars-sgml-mode)
(handlebars-use-mode 'minor)

;; po-mode
(add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
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

;;; HTML & co
;; auto-start zencoding with SGML modes
;; (non e' comodissimo...)
;;; (add-hook 'sgml-mode-hook 'zencoding-mode)
;;; (add-hook 'web-mode-hook 'zencoding-mode)

;;; funzioni
;;; https://github.com/magnars/.emacs.d/blob/master/defuns/buffer-defuns.el
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))
