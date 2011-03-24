;;; Personalizzazioni

; voglio la menu-bar
(menu-bar-mode 1)

; IDO lets you  open files  and  switch buffers  with fuzzy  matching,
; really nice when you have lots of things open.
; http://www.emacsblog.org/2008/05/19/giving-ido-mode-a-second-chance/
;; (require 'ido)
;; (ido-mode t)
;; (setq ido-enable-flex-matching t)

; color-themes
(add-to-list 'load-path
             "~/.emacs.d/plugins/color-theme")
(require 'color-theme)
(color-theme-initialize)

(add-to-list 'load-path "~/.emacs.d/themes")


; Personalizzazione rozza per GUI vs Terminal
(if window-system
    (load "~/.emacs.d/themes/color-theme-radiance.el"))

(if (and (eq window-system 'ns) (eq system-type 'darwin))
    (set-face-attribute 'default nil :font "DejaVu Sans Mono 12"))

; dimensione frame
(if (window-system)
    (set-frame-size (selected-frame) 124 40))

; Font per linux GUI
(if (and (eq window-system 'x) (eq system-type 'gnu/linux))
    (set-face-attribute 'default nil :font "Monospace 9"))

; Navigazione dei TAB (come un browser web)
; C-tab -> next tab/buffer
; S-C-tab -> previous tab/buffer
(global-set-key (kbd "<C-tab>") 'next-tab-or-buffer)
(global-set-key (kbd "<S-C-tab>") 'previous-tab-or-buffer)

; Text-mode di default:
(setq default-major-mode 'text-mode)

; yasnippet
(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

; python.el
; (add-to-list 'load-path "~/.emacs.d/plugins")
; (require 'python)

; python-mode
(add-to-list 'load-path
             "~/.emacs.d/plugins/python-mode")
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

; NOTE
; Per andare a capo senza indentare a cazzo: C-j


; Pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;; ; ropemacs & soci
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

; python checkers

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list "~/bin/pycheckers"  (list local-file))))
   (add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init)))

; temi?
;; (load "~/.emacs.d/themes/bho1.el")
;; (load "~/.emacs.d/themes/bho2.el")
;; (load "~/.emacs.d/themes/bho3.el")
;; (load "~/.emacs.d/themes/bho4.el")
;; (load "~/.emacs.d/themes/bho5.el")
;; (load "~/.emacs.d/themes/bho6.el")
;; (load "~/.emacs.d/themes/bho7.el")
;; (load "~/.emacs.d/themes/bho8.el")
;; (load "~/.emacs.d/themes/bho9.el")
;; (load "~/.emacs.d/themes/bho10.el")
;; (load "~/.emacs.d/themes/color-theme-gruber-darker.el")
;; (load "~/.emacs.d/themes/color-theme-less.el")
;; (load "~/.emacs.d/themes/color-theme-molokai.el")
;; (load "~/.emacs.d/themes/color-theme-blackboard.el")
;; (load "~/.emacs.d/themes/color-theme-subdued.el")
;; (load "~/.emacs.d/themes/pigerrimitudo.el")
;; (load "~/.emacs.d/themes/inkpot.el")

; mostra le combinazioni non bindate
(require 'unbound)

; questo fa lisp z0rz0rz
(require 'pretty-mode)

; undo-tree
; http://www.dr-qubit.org/emacs.php#undo-tree
(require 'undo-tree)
