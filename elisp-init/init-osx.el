;; OS X

;; try to use GNU ls from coreutils (installed with homebrew)
(let ((gnu-ls "/usr/local/bin/gls"))
  (if (file-exists-p gnu-ls)
      (setq insert-directory-program gnu-ls)))

;; non so se serve anche questo:
;; (setq ls-lisp-use-insert-directory-program t)  ;; use external ls

;;; `exec()` PATH from shell
(require 'exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'init-osx)
