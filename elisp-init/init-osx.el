;; OS X

;; try to use GNU ls from coreutils (installed with homebrew)
(let ((gnu-ls "/usr/local/bin/gls"))
  (if (file-exists-p gnu-ls)
      (setq insert-directory-program gnu-ls)))

;; non so se serve anche questo:
;; (setq ls-lisp-use-insert-directory-program t)  ;; use external ls

;; default browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; in dired use the trash
(setq delete-by-moving-to-trash t)

(provide 'init-osx)
