;; sand.el
;; Ispirato da molti e da nessuno.
;;
;; last modified: 2011-05-27 23:18 by sand
;;
;; TIPS & TRICKS
;; C-x r w <registro> - salva i layout (con winner-mode)
;; C-c <LEFT> - winner-mode undo (torna al layout precedente)
;; undo-tree-mode e C-\ e M-_

;; detect OS (http://www.xsteve.at/prg/emacs/.emacs.txt )
(defconst darwinp
  (eq system-type 'darwin)
  "Are we running on OSX?")

(defconst linuxp
  (or (eq system-type 'gnu/linux)
      (eq system-type 'linux))
  "Are we running on Linux?")

;; voglio la menu-bar
(menu-bar-mode 1)

;; IDO lets you  open files  and  switch buffers  with fuzzy  matching,
;; really nice when you have lots of things open.
;; http://www.emacsblog.org/2008/05/19/giving-ido-mode-a-second-chance/
;; (require 'ido)
;; (ido-mode t)
;; (setq ido-enable-flex-matching t)

;; color-themes
;; (add-to-list 'load-path
;;              "~/.emacs.d/plugins/color-theme")
;; (require 'color-theme)
;; (color-theme-initialize)

;; (add-to-list 'load-path "~/.emacs.d/themes")


; Personalizzazione rozza per GUI vs Terminal
;; (if window-system
;;     (load "~/.emacs.d/themes/color-theme-radiance.el"))
;(color-theme-zenburn)

;; Font per Darwin/OSX
(if (and (eq window-system 'ns) (eq system-type 'darwin))
    (set-face-attribute 'default nil :font "DejaVu Sans Mono 12"))

;; dimensione frame
(if (window-system)
    (set-frame-size (selected-frame) 124 40))

;; Font per linux GUI
(if (and (eq window-system 'x) (eq system-type 'gnu/linux))
    (set-face-attribute 'default nil :font "Monospace 9"))

;; Navigazione dei TAB (come un browser web) [e' per aquamacs?]
;; C-tab -> next tab/buffer
;; S-C-tab -> previous tab/buffer
;(global-set-key (kbd "<C-tab>") 'next-tab-or-buffer)
;(global-set-key (kbd "<S-C-tab>") 'previous-tab-or-buffer)

;; Text-mode di default:
(setq default-major-mode 'text-mode)

;; yasnippet
(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

;; python-mode
(add-to-list 'load-path
             "~/.emacs.d/plugins/python-mode")
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; NOTE
;; Per andare a capo senza indentare a cazzo: C-j

;; Pymacs
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

;; python checkers

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

;; temi?
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
;; (color-theme-subdued)
;(color-theme-blippblopp)
(load "~/.emacs.d/themes/color-theme-sanityinc-solarized.el")
(color-theme-sanityinc-solarized-light)
;; (zenburn)

;; cedet
;; (load-file "~/.emacs.d/plugins/cedet-1.0/common/cedet.el")
;; (global-ede-mode 1)
;; (semantic-load-enable-code-helpers)
;; (global-srecode-minor-mode 1)

;; (add-to-list 'load-path
;;              "~/.emacs.d/plugins/ecb-2.40")
;; (require 'ecb)

;; plugin per switchare finestre
(require 'windmove)
(windmove-default-keybindings 'meta)

;; plugin che ricorda i layout
(winner-mode 1)

;; elscreen
;; (load "elscreen" "ElScreen" t)

;; cua-mode
;; (cua-mode t)
;; (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;; (transient-mark-mode 1) ;; No region when it is not highlighted
;; (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; rst.el
;; (require 'rst)

;; encoding
(prefer-coding-system 'utf-8-unix)
(set-variable 'default-buffer-file-coding-system 'utf-8-unix)

;; Aggiorna il timestamp automaticamente (chiedendo conferma)
(setq time-stamp-pattern "10/[Ll]ast modified: %:y-%02m-%02d %02H:%02M by %u$")
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'before-save-hook 'copyright-update)

;; Cancella i "trailing whitespace" dai file python
(add-hook 'python-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'delete-trailing-whitespace)))

;; mostra le combinazioni non bindate
(require 'unbound)

;; questo fa lisp z0rz0rz
(require 'pretty-mode)

;; undo-tree
;; http://www.dr-qubit.org/emacs.php#undo-tree
(require 'undo-tree)

;; org-mode
(setq load-path (cons "~/.emacs.d/plugins/org-7.5/lisp" load-path))
(setq load-path (cons "~/.emacs.d/plugins/org-7.5/contrib/lisp" load-path))
(require 'org-install)

;; associo i file .org
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; (org-agenda-files (quote ("~/org/notes.org" "~/org/flagged.org")))

;; Aggiunge la data quando completi un task.
(setq org-log-done t)

;; MobileOrg
;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(setq org-default-notes-file (concat org-directory "/notes.org"))

;; transparency
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

;; lua-mode
;; (load "~/.emacs.d/plugins/lua-mode.el")
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;; (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
;; (add-hook 'lua-mode-hook 'turn-on-font-lock)

;; C-z per undo, che di minimizzare me ne fotte il cazzo
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)

;; rainbow mode
(load "~/.emacs.d/rainbow-mode.el")

;; GnuPG path
(if darwinp
    (setq epg-gpg-program "/usr/local/bin/gpg"))

;; Definisce "gnome-open" come comando per aprire in modo generico gli
;; URL. La funzione e' "browse-url-generic".
(global-set-key "\M-o" 'browse-url-generic)

;; Su linux il comando migliore e' "gnome-open"
(if (and (eq window-system 'x) (eq system-type 'gnu/linux))
    (setq browse-url-generic-program "gnome-open"))

;; Su OSX il comando migliore e' "open"
(if (and (eq window-system 'ns) (eq system-type 'darwin))
    (setq browse-url-generic-program "open"))

;; aspell invece di ispell
(setq-default ispell-program-name "aspell")

;; gpg
;; suggerimento:
;; --->  ;; -*- epa-file-encrypt-to: ("ueno@unixuser.org") -*-
(require 'epa-file)
(epa-file-enable)

;; zsh-mode, ma che cazzo!
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; renpy mode
(load "~/.emacs.d/plugins/Renpy.el")
