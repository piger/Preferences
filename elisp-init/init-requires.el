;;; various required packages

;; ido
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

;;; windmove
;;; per switchare finestra con shift+arrows o alt+arrows
(require 'windmove)
(windmove-default-keybindings 'meta)

;;; `exec()` PATH from shell
(require 'exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;; yasnippet
(require 'yasnippet)
;; O si abilita il global-mode, o il minor mode con degli hook per il major-mode
;; del linguaggio visualizzato; nel secondo caso pero' bisogna chiamare manualmente
;; (yas-reload-all)!
;;; (yas-global-mode 1
;;; (yas-reload-all)

;;; smex, remember recently and most frequently used commands
(require 'smex)
;; (setq smex-save-file (expand-file-name ".smex-items" prelude-savefile-dir))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; editor di regexp che evita la pazzia dei backslash
(require 're-builder)
;; evita la pazzia dei backslash
(setq reb-re-syntax 'string)

;; git
(require 'magit)
(require 'git-commit-mode)
(require 'git-rebase-mode)

;; dired bindings (tipo C-x C-j)
(require 'dired-x)

(provide 'init-requires)
