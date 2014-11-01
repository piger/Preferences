;;; various required packages

(use-package ido
  :init
  (progn
    (ido-mode +1)
    (ido-everywhere +1))
  :config
  (progn
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-everywhere t)
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(use-package flx-ido
  :init (flx-ido-mode 1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; meaningful names for buffers with the same name
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t          ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"))     ; don't muck with special buffers

;;; windmove
;;; per switchare finestra con shift+arrows o alt+arrows
(use-package windmove
  :config (windmove-default-keybindings 'meta))

;;; yasnippet
;; (require 'yasnippet)
;; O si abilita il global-mode, o il minor mode con degli hook per il major-mode
;; del linguaggio visualizzato; nel secondo caso pero' bisogna chiamare manualmente
;; (yas-reload-all)!
;;; (yas-global-mode 1
;;; (yas-reload-all)

(use-package smex
  :init (smex-initialize)
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))

;; editor di regexp che evita la pazzia dei backslash
;; (require 're-builder)
;; evita la pazzia dei backslash
;; (setq reb-re-syntax 'string)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :diminish git-gutter-mode
  :init (global-git-gutter-mode +1))

;; dired bindings (tipo C-x C-j)
(use-package dired-x)

; zone e' fondamentale direi (per avere zone-when-idle)
(use-package zone)

;; expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; move-text
;; (require 'move-text)
;; i binding di default sono alt+up e alt+down, gli stessi che uso
;; per switchare finestra.
;; (move-text-default-bindings)

;; evil-mode
(setq evil-want-C-u-scroll t)
(use-package evil
  :config
  (setq evil-emacs-state-cursor  '("red" box)
        evil-normal-state-cursor '("gray" box)
        evil-visual-state-cursor '("gray" box)
        evil-insert-state-cursor '("gray" bar)
        evil-motion-state-cursor '("gray" box)))

(provide 'init-requires)
