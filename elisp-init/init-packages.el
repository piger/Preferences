;;; packages

(require 'cl)
(require 'package)

;;; common repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;;; GNU for emacs < 24
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; add also Melpa
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; and Melpa stable
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://hiddencameras.milkbox.net/packages/") t)

(package-initialize)


;; check if a package is installed; if not, install it.
;; (mapc
;;  (lambda (package)
;;    (or (package-installed-p package)
;;        (if (y-or-n-p (format "Package %s is missing. Install it? " package))
;; 		   (package-install package))))
 
;;  '(anzu
;;    apache-mode
;;    base16-theme
;;    browse-kill-ring
;;    diminish
;;    exec-path-from-shell
;;    gitconfig-mode
;;    gitignore-mode
;;    go-mode
;;    handlebars-sgml-mode
;;    ; jinja2-mode
;;    js2-mode
;;    json-mode
;;    less-css-mode
;;    magit
;;    markdown-mode
;;    move-text
;;    nginx-mode
;;    osx-plist
;;    php-mode
;;    rainbow-mode
;;    smex
;;    undo-tree
;;    volatile-highlights
;;    web-mode
;;    yasnippet))

(defvar my-packages
 '(anzu
   ack-and-a-half
   apache-mode
   base16-theme
   browse-kill-ring
   diminish
   expand-region
   exec-path-from-shell
   gitconfig-mode
   gitignore-mode
   go-mode
   helm
   flx-ido
   ;; ido-vertical-mode
   ido-ubiquitous
   xkcd
   ; handlebars-sgml-mode
   ; jinja2-mode
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
   smex
   undo-tree
   volatile-highlights
   web-mode
   ;yasnippet
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
