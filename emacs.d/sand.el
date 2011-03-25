; sand.el
; Ispirato da molti e da nessuno.
;
; last modified: 2011-03-25 02:56 by sand

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
;; (if window-system
;;     (load "~/.emacs.d/themes/color-theme-radiance.el"))
;(color-theme-zenburn)

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

;; ;; ; ropemacs & soci
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")

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
(load "~/.emacs.d/themes/color-theme-subdued.el")
;; (load "~/.emacs.d/themes/pigerrimitudo.el")
;; (load "~/.emacs.d/themes/inkpot.el")
(color-theme-subdued)

; mostra le combinazioni non bindate
(require 'unbound)

; questo fa lisp z0rz0rz
(require 'pretty-mode)

; undo-tree
; http://www.dr-qubit.org/emacs.php#undo-tree
(require 'undo-tree)

; cedet
;; (load-file "~/.emacs.d/plugins/cedet-1.0/common/cedet.el")
;; (global-ede-mode 1)
;; (semantic-load-enable-code-helpers)
;; (global-srecode-minor-mode 1)

;; (add-to-list 'load-path
;;              "~/.emacs.d/plugins/ecb-2.40")
;; (require 'ecb)

; plugin per switchare finestre
(require 'windmove)
(windmove-default-keybindings 'meta)

; elscreen
; (load "elscreen" "ElScreen" t)

; cua-mode
;; (cua-mode t)
;; (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;; (transient-mark-mode 1) ;; No region when it is not highlighted
;; (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

; rst.el
; (require 'rst)

; encoding
(prefer-coding-system 'utf-8-unix)
(set-variable 'default-buffer-file-coding-system 'utf-8-unix)

; time-stamp
(setq time-stamp-pattern "10/[Ll]ast modified: %:y-%02m-%02d %02H:%02M by %u$")
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'before-save-hook 'copyright-update)

; prova solo python
(add-hook 'python-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'delete-trailing-whitespace)))
