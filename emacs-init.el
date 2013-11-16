;;; emacs.d/init.el
;;;
;;; - https://github.com/magnars/.emacs.d/blob/master/init.el
;;; - http://www.emacswiki.org/emacs/ImenuMode

;;; nasconde la toolbar; e' bene farlo all'inizio per evitare che venga mostrata brevemente
(tool-bar-mode -1)
;;; nasconde la scroll bar (perchè mai?)
; (scroll-bar-mode -1)
 
;;; OS X?
(defconst *is-a-mac* (eq system-type 'darwin))

;;(when *is-a-mac*
;;  (setq mouse-wheel-scroll-amount '(0.001)))

;;; emacs custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Enable ido-mode
(require 'ido)
(ido-mode t)
;;; Non-nil means that `ido' will do flexible string matching.
;;; Flexible matching means that if the entered string does not
;;; match any item, any item containing the entered characters
;;; in the given sequence will match.
(setq ido-enable-flex-matching t)
;;; To use ido for all buffer and file selections in Emacs, customize the
;;; variable `ido-everywhere'.
(setq ido-everywhere t)

;;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; save-place per salvare la posizione nel buffer quando si esce, tipo
;;; viminfo in vim.
;;; (require 'saveplace)
;;; (setq-default save-place t)

;;; proviamo ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; hippie?
;;; (global-set-key (kbd "M-/") 'hippie-expand)

;;; default regexp search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;; show column number by default
(setq column-number-mode t)

;;; windmove
;;; per switchare finestra con shift+arrows o alt+arrows
(require 'windmove)
(windmove-default-keybindings 'meta)

;;; undo con C-z (al posto di minimize window)
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)

;;; browser con M-o
(global-set-key "\M-o" 'browse-url-generic)
(if (and (eq window-system 'x) (eq system-type 'gnu/linux))
	(setq browse-url-generic-program "gvfs-open"))
(if (and (eq window-system 'ns) *is-a-mac*)
	(setq browse-url-generic-program "open"))

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

;;; common repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;;; GNU for emacs < 24
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; add also Melpa
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; But don't take Melpa versions of certain packages (from purcell emacs.d)
(setq package-filter-function
      (lambda (package version archive)
        (or (not (string-equal archive "melpa"))
            (not (memq package '())))))

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
 '(anti-zenburn-theme apache-mode go-mode jinja2-mode js2-mode json-mode less-css-mode markdown-mode nginx-mode osx-plist php-mode twilight-theme zenburn-theme
					  web-mode handlebars-sgml-mode magit exec-path-from-shell flycheck))


;; miei script
(add-to-list 'load-path "~/elisp")

;;; themes
;;; https://github.com/owainlewis/emacs-color-themes
(add-to-list 'load-path "~/elisp/themes/tomorrow-theme")
(add-to-list 'custom-theme-load-path "~/elisp/themes/base16-theme")
(add-to-list 'custom-theme-load-path "~/elisp/themes/tomorrow-theme")

;;; THEME
;;; Setta il color-theme (nuovo stile, emacs 24+)
;(load-theme 'adwaita)
;(load-theme 'zenburn)
;(load-theme 'base16-default)
;; (load-theme 'tomorrow-night)
;;(load-theme 'dichromacy)
;; (load-theme 'wilson)
(load-theme 'solarized-light)


;;; Fonts
;;; questo non saprei cosa fa:
; (add-to-list 'default-frame-alist
; 	     '(font . "DejaVu Sans Mono-13"))

;;; Per i font bisogna personalizzare custom.el
;;; (custom-set-faces
;;;  '(default ((t (:height 120 :family "Cousine"))))
;;; )

;;; `exec()` PATH from shell
(require 'exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; flymake
;; (A quanto pare flymake e' l'originale "che funziona")
(require 'flymake)

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
		("django"		. "/templates-ink/.*\\.html\\'")
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
(setenv "GOPATH" (expand-file-name "~/dev/go"))
(setenv "PATH" (concat (getenv "PATH") ":" (concat (getenv "GOPATH") "/bin")))
(setq exec-path (append exec-path (list (expand-file-name "~/dev/go/bin"))))

(add-to-list 'load-path "~/dev/go/src/github.com/dougm/goflymake")
(require 'go-flymake)

(defun my-go-mode-hook()
  ;;(add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 2))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;;; flycheck
(when (eval-when-compile (>= emacs-major-version 24))
  (require 'flycheck)
  ;; (add-hook 'after-init-hook 'global-flycheck-mode)
)
;; (add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'flymake-mode)


;; flymake + Python
(setq pylint "/usr/local/bin/epylint")
(when (load "flymake" t)
  (defun flymake-pylint-init ()
	(let* ((temp-file (flymake-init-create-temp-buffer-copy
					   'flymake-create-temp-inplace))
		   (local-file (file-relative-name
						temp-file
						(file-name-directory buffer-file-name))))
	  (list (expand-file-name pylint "") (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
			   '("\\.py\\'" flymake-pylint-init)))

;; Configure to wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '3)

;; Keymaps to navigate to the errors
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)))

;; To avoid having to mouse hover for the error message, these functions make flymake error messages
;; appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (interactive)
  (require 'cl)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)

;; set as minor mode for Python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))

;;; show-paren-mode
(add-hook 'python-mode-hook '(lambda () (show-paren-mode)))

;;; flyspell prog mode
;; (if (fboundp 'prog-mode)
;;     (add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;   (dolist (hook '(lisp-mode-hook
;;                   emacs-lisp-mode-hook
;;                   scheme-mode-hook
;;                   clojure-mode-hook
;;                   ruby-mode-hook
;;                   yaml-mode
;;                   python-mode-hook
;;                   shell-mode-hook
;;                   php-mode-hook
;;                   css-mode-hook
;;                   haskell-mode-hook
;;                   caml-mode-hook
;;                   nxml-mode-hook
;;                   crontab-mode-hook
;;                   perl-mode-hook
;;                   tcl-mode-hook
;;                   javascript-mode-hook))
;;     (add-hook hook 'flyspell-prog-mode)))

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

;; wcheck / aspell / hunspell
(setq wcheck-language-data
	  '(("Italian"
		 (program . "/usr/local/bin/hunspell")
		 (args "-l" "-d" "/Users/sand/Documents/dictionaries/dict-it-it_and_latin_2013-03-31/it_IT/it_IT")
		 (action-program . "/usr/local/bin/hunspell")
		 (action-args "-a" "-d" "/Users/sand/Documents/dictionaries/dict-it-it_and_latin_2013-03-31/it_IT/it_IT")
		 (action-parser . wcheck-parser-ispell-suggestions))

		("English"
		 (program . "/usr/local/bin/enchant")
		 (args "-l" "-d" "en")
		 (action-program . " /usr/local/bin/enchant")
		 (action-args "-a" "-d" "en")
		 (action-parser . wcheck-parser-ispell-suggestions))))

;		("English"
;		 (program . "/usr/local/bin/hunspell")
;		 (args "-l" "-d" "/Users/sand/Documents/dictionaries/en_us/en_US")
;		 (action-program . "/usr/local/bin/hunspell")
;		 (action-args "-a" "-d" "/Users/sand/Documents/dictionaries/en_us/en_US")
;		 (action-parser . wcheck-parser-ispell-suggestions))))

;;; git
(require 'magit)
(require 'git-commit-mode)
(require 'git-rebase-mode)
