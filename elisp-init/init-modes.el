;;; general modes configuration

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

;; highlight FIXME & co
(add-hook 'prog-mode-hook '(lambda () (prelude-font-lock-comment-annotations)))
;; add colors to parenthesis
(add-hook 'prog-mode-hook '(lambda () (rainbow-delimiters-mode)))

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
  :init
  (progn
    (projectile-global-mode +1)
    (bind-key "\C-cf" 'projectile-find-file)))
(use-package helm-projectile)

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


(provide 'init-modes)
