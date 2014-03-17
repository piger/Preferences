;;; packages

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


;; check if a package is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
		   (package-install package))))
 '(apache-mode
   go-mode
   diminish
   jinja2-mode
   js2-mode
   json-mode
   less-css-mode
   markdown-mode
   nginx-mode
   osx-plist
   php-mode
   twilight-theme
   zenburn-theme
   web-mode
   handlebars-sgml-mode
   magit
   exec-path-from-shell
   smex
   yasnippet

   volatile-highlights
   rainbow-mode
   move-text
   gitignore-mode
   gitconfig-mode
   browse-kill-ring
   anzu
))


(provide 'init-packages)
