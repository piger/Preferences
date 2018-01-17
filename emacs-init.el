;;; new emacs settings using org mode

(defconst emacs-start-time (current-time)
  "This variable hold the time emacs was started.")

;; load custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; init package system to load the updated org-mode package from melpa (or is it from elpa??)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; load settings file
(org-babel-load-file (expand-file-name "~/Preferences/emacs-settings.org"))

;; display loading time
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Settings loaded in %.3fs" elapsed))

;;; to help troubleshooting:
;;; (setq debug-on-error t)
