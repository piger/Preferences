;;; packages

(require 'cl)
(require 'package)

;;; common repositories
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; melpa
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; melpa-stable
;; http://hiddencameras.milkbox.net/
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

;; package-filters
;; Installando questo package e' possibile filtrare i pacchetti per repository,
;; in modo da installare alcuni package da stable e altri da "unstable"; purtroppo
;; il modo per farlo e' orribile...

;; (progn
;;   (switch-to-buffer
;;     (url-retrieve-synchronously
;;       "https://raw.github.com/milkypostman/package-filter/master/package-filter.el"))
;;   (package-install-from-buffer  (package-buffer-info) 'single))

;; (setq package-archive-enable-alist '(("gnu")
;;                                      ("melpa")
;;                                      ("melpa-stable"
;;                                       magit
;;                                       solarized-theme
;;                                       js2-mode)))

(setq package-archive-exclude-alist '(("gnu"
                                      magit
                                      solarized-theme
                                      js2-mode)
                                      ("melpa"
                                       magit
                                       solarized-theme
                                       js2-mode)
                                      ("melpa-stable")))

(package-initialize)

(defvar my-packages
 '(anzu
   ack-and-a-half
   apache-mode
   ; base16-theme
   browse-kill-ring
   buffer-move
   diminish
   expand-region
   exec-path-from-shell
   gitconfig-mode
   gitignore-mode
   go-mode
   go-eldoc
   helm
   flx-ido
   ido-ubiquitous
   ; xkcd
   js2-mode
   json-mode
   less-css-mode
   magit
   markdown-mode
   move-text
   nginx-mode
   ; osx-plist
   projectile
   php-mode
   rainbow-mode
   rainbow-delimiters
   smex
   undo-tree
   volatile-highlights
   ;;; web-mode
   ;;; yasnippet
)
 "A list of packages to ensure are installed at launch")

(defun my-packages-installed-p ()
  "Check if all packages in `my-packages' are installed"
  (every #'package-installed-p my-packages))

(defun pl-require-package (package)
  "Install PACKAGE unless already installed"
  (unless (memq package my-packages)
    (add-to-list 'my-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun pl-require-packages (packages)
  "Ensure PACKAGES are installed"
  (mapc #'pl-require-package packages))

(defun pl-install-packages ()
  "Install all packages listed in `my-packages'"
  (unless (my-packages-installed-p)
    (message "%s" "refreshing the package database...")
    (package-refresh-contents)
    (message "%s" "done.")
    (pl-require-packages my-packages)))

(pl-install-packages)


;; `exec()` PATH from shell
;; Questo va messo PRIMA di tutto perche' altrimenti tutti i PATH
;; presi dai vari plugin non prendono il setting e pescano la roba in
;; /usr/bin invece di /usr/local/bin
(require 'exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


(provide 'init-packages)
