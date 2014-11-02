;;; general modes configuration

;;; 2/11/2014 - provo a usare Helm
;; (use-package ido
;;   :init
;;   (progn
;;     (ido-mode +1)
;;     (ido-everywhere +1))
;;   :config
;;   (progn
;;     (setq ido-enable-prefix nil
;;           ido-enable-flex-matching t
;;           ido-everywhere t)
;;     (add-to-list 'ido-ignore-files "\\.DS_Store")))

;; (use-package flx-ido
;;   :init (flx-ido-mode 1))

(use-package helm
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100
          helm-split-window-in-side-p t
          helm-ff-file-name-history-use-recentf t)
    (helm-mode))
  :diminish helm-mode
  :bind
  (("C-c h" . helm-mini)
   ("M-x" . helm-M-x)))

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

;; helm-M-x è meglio? - 02/11/2014
;; (use-package smex
;;   :init (smex-initialize)
;;   :bind
;;   ("M-x" . smex)
;;   ("M-X" . smex-major-mode-commands))

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

;; Assign a specific mode for certain directories
;; note: you can't chain multiple paths in a single add-to-list call :(
(add-to-list 'auto-mode-alist '("/Documents/appunti/[^/]*\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("/Preferences/zsh/" . shell-script-mode))

;; rst-mode: default to auto-fill
(add-hook 'rst-mode-hook 'turn-on-auto-fill)

;; po-mode
(add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
; (autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (defun piger/python-mode-hooks ()
    "Defaults for python-mode."
    (subword-mode +1)
    (show-paren-mode +1)
    ;; unfuck electric indentation
    (setq electric-indent-chars '(?\n)))
  (add-hook 'python-mode-hook 'piger/python-mode-hooks))

;; golang
(setenv "GOPATH" (expand-file-name "~/dev/go"))
(setenv "PATH" (concat (getenv "PATH") ":" (concat (getenv "GOPATH") "/bin")))
(setq exec-path (append exec-path (list (expand-file-name "~/dev/go/bin"))))

(eval-after-load 'go-mode
  '(progn
     (defun prelude-go-mode-defaults ()
       (add-hook 'before-save-hook 'gofmt-before-save nil t)
       (set (make-local-variable 'company-backends) '(company-go))
       (go-eldoc-setup)
       (setq tab-width 2)
       (local-set-key (kbd "C-c C-k") 'godoc)
       (subword-mode +1))

     (setq prelude-go-mode-hook 'prelude-go-mode-defaults)
     (add-hook 'go-mode-hook (lambda ()
                               (run-hooks 'prelude-go-mode-hook)))
     
     ;; Enable go-oracle-mode if available
     (let ((oracle (executable-find "oracle")))
       (when oracle
         (setq go-oracle-command oracle)
         (autoload 'go-oracle-mode "oracle")
         (add-hook 'go-mode-hook 'go-oracle-mode)))))

(use-package css-mode
  :config
  (progn
    (use-package rainbow-mode)
    (setq css-indent-offset 2)
    (rainbow-mode +1)
    (subword-mode +1)))

(add-hook 'prog-mode-hook
          (lambda ()
            (use-package idle-highlight-mode
              :init (idle-highlight-mode t))
            (prelude-font-lock-comment-annotations)
            (rainbow-delimiters-mode t)
            (setq show-trailing-whitespace t)
            (subword-mode t)))

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :interpreter ("node" . js2-mode)
  :config
  (progn
    (setq-default js2-global-externs '("module", "require", "console",
                                       "jQuery", "$"))
    (add-hook 'js2-init-hook
              (lambda ()
                (when (or (string-match-p "zAFS" (buffer-file-name))
                          (string-match-p "LogIntelligence" (buffer-file-name)))
                  (mapc (lambda (x)
                          (add-to-list 'js2-additional-externs x))
                        (list "Ember" "DS" "App")))))
    (add-hook 'js2-mode-hook (lambda () (subword-mode +1)))
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 4)))
    (set-variable 'indent-tabs-mode nil)
    ))

(use-package web-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
  :config
  (progn
    (add-hook 'web-mode-hook (lambda ()
                               (local-set-key (kbd "RET") 'newline-and-indent)))
    (setq web-mode-engines-alist
          '(("go" . "/dev/go/src/.*\\.html\\'")))))
  
; elisp defaults
(defun pl-elisp-mode-defaults ()
  "Some defaults for elisp mode"
  (turn-on-eldoc-mode)
  (diminish 'eldoc-mode)
  (rainbow-mode +1)
  (diminish 'rainbow-mode))
(setq pl-elisp-mode-hooks 'pl-elisp-mode-defaults)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'pl-elisp-mode-hooks)))

;; ack-and-a-half
(use-package ack-and-a-half
  :init
  (progn
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

;; projectile
(use-package projectile
  :bind ("\C-cf" . projectile-find-file)
  :init
  (progn
    (projectile-global-mode +1)
    (use-package helm-projectile
      :init (helm-projectile-on)))
  :config
  (setq projectile-mode-line '(:eval (format " &{%s}" (projectile-project-name)))))

;; company (completion)
(use-package company)

;; apache-mode
(use-package apache-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.htaccess\\'" . apache-mode))
    (add-to-list 'auto-mode-alist '("sites-\\(available|enabled\\)/" . apache-mode))))

;; outline mode
;; code folding with vim compatibility
;; https://raw.githubusercontent.com/yyetim/emacs-configuration/master/elisp/vim-fold.el
;; modificato leggermente, perche' io i marker li uso anche senza numero (e.g. "{{{1")
;; per indicare il livello di outline.
(defun set-vim-foldmarker (fmr)
  "Set Vim-type foldmarkers for the current buffer"
  (interactive "sSet local Vim foldmarker: ")
  (if (equal fmr "")
      (message "Abort")
    (setq fmr (regexp-quote fmr))
    (set (make-local-variable 'outline-regexp)
	 (concat ".*" fmr "\\([0-9]+\\)?"))
    (set (make-local-variable 'outline-level)
	 `(lambda ()
	    (save-excursion
	      (re-search-forward
	       ,(concat fmr "\\([0-9]+\\)") nil t)
              (if (match-string 1)
                  (string-to-number (match-string 1))
                (string-to-number "0")))))))
;; (add-hook 'outline-minor-mode-hook
;; 	  (lambda () (local-set-key "\C-c\C-c"
;; 				    outline-mode-prefix-map)))
(global-set-key (kbd "C-<tab>") 'outline-toggle-children)

(use-package volatile-highlights
  :init (volatile-highlights-mode +1)
  :diminish volatile-highlights-mode)

(use-package recentf
  :init (recentf-mode +1)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-exclude '("/tmp/" "/ssh:")))

(use-package undo-tree
  :init (global-undo-tree-mode +1)
  :diminish undo-tree-mode)

(use-package winner
  :init (winner-mode +1))

(use-package anzu
  :init (global-anzu-mode)
  :diminish anzu-mode)

(use-package flyspell
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

;;; org-mode
; general setup
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-c\M-p" 'org-babel-previous-src-block)
(global-set-key "\C-c\M-n" 'org-babel-next-src-block)
(global-set-key "\C-cS" 'org-babel-previous-src-block)
(global-set-key "\C-cs" 'org-babel-next-src-block)

; capture-file
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
;; (setq org-todo-keywords
;;       '((sequence "TODO" "VERIFY" "|" "DONE" "DELEGATED")))
(setq org-tags-alist
      '((sequence "work" "personal" "computer" "blog")))
;; mobile org
(setq org-mobile-directory "~/Dropbox/org/mobile")
(setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))

;; add timestamp to closed TODO entries
(setq org-log-done 'time)

;; highlight code blocks
(setq org-src-fontify-natively t)

;; turn off source blocks default indentation
(setq org-edit-src-content-indentation 0)

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq org-todo-keywords
      (quote
       ((sequence "SOMEDAY(s)" "TODO(t)" "INPROGRESS(i)" "WAITING(w@/!)"
                  "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("INPROGRESS" :foreground "deep sky blue" :weight bold)
              ("SOMEDAY" :foreground "purple" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))
    
(provide 'init-modes)
