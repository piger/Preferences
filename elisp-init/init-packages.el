;;; packages

(require 'cl)
(require 'package)

;;; common repositories
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; melpa
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; melpa-stable
;; http://hiddencameras.milkbox.net/
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 4))
  (add-to-list 'package-archives
               '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (setq package-pinned-packages '((magit . "melpa-stable"))))

;; load and activate installed packages
(package-initialize)

(defvar my-packages
 '(anzu
   ack-and-a-half
   apache-mode
   base16-theme
   browse-kill-ring
   buffer-move
   company
   diminish
   expand-region
   exec-path-from-shell
   gitconfig-mode
   gitignore-mode
   go-mode
   go-eldoc
   company-go
   gotest
   helm
   helm-projectile
   evil
   flx-ido
   ido-ubiquitous
   ; xkcd
   js2-mode
   json-mode
   less-css-mode
   git-gutter
   magit
   markdown-mode
   move-text
   nginx-mode
   ; osx-plist
   projectile
   php-mode
   ;; pyenv-mode
   ;; anaconda-mode
   rainbow-mode
   rainbow-delimiters
   ;; smex
   undo-tree
   volatile-highlights
   web-mode
   use-package
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

;; install missing packages
(pl-install-packages)

;; `exec()` PATH from shell
;; Questo va messo PRIMA di tutto perche' altrimenti tutti i PATH
;; presi dai vari plugin non prendono il setting e pescano la roba in
;; /usr/bin invece di /usr/local/bin
(require 'exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; enable use-package
(require 'use-package)

(provide 'init-packages)
