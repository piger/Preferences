;;; local elisp files and themes

;; miei script
(add-to-list 'load-path "~/Preferences/elisp")

;;; nagios-mode (da elisp locale)
(autoload 'nagios-mode "nagios-mode" nil t)

;;; themes
;;; https://github.com/owainlewis/emacs-color-themes
(add-to-list 'load-path "~/Preferences/elisp/themes/tomorrow-theme")
(add-to-list 'custom-theme-load-path "~/Preferences/elisp/themes/base16-theme")
(add-to-list 'custom-theme-load-path "~/Preferences/elisp/themes/tomorrow-theme")

(provide 'init-local-elisp)
