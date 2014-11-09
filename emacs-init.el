;;; new emacs settings using org mode

(defconst emacs-start-time (current-time)
  "This variable hold the time emacs was started.")

;; load custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; load settings file
(require 'org)
(org-babel-load-file
 (expand-file-name "~/Dropbox/org/emacs-settings.org"))

;; display loading time
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Settings loaded in %.3fs" elapsed))
