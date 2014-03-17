;;; general modes configuration

;; Assign a specific mode for certain directories
;; note: you can't chain multiple paths in a single add-to-list call :(
(add-to-list 'auto-mode-alist '("/Documents/appunti/[^/]*\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("/Preferences/zsh/" . shell-script-mode))

;; rst-mode: default to auto-fill
(add-hook 'rst-mode-hook 'turn-on-auto-fill)

;; po-mode
(add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

;; golang
(setenv "GOPATH" (expand-file-name "~/dev/go"))
(setenv "PATH" (concat (getenv "PATH") ":" (concat (getenv "GOPATH") "/bin")))
(setq exec-path (append exec-path (list (expand-file-name "~/dev/go/bin"))))

(defun my-go-mode-hook()
  ;;(add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 2))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(eval-after-load 'css-mode
  '(progn
     (require 'rainbow-mode)

     (defun pl-css-mode-defaults ()
       (setq css-indent-offset 2)
       (rainbow-mode +1))

     (setq pl-css-mode-hook 'pl-css-mode-defaults)
     (add-hook 'css-mode-hook (lambda ()
                                (run-hooks 'pl-css-mode-hook)))))

(defun prelude-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  ;;(smartparens-mode +1)
  (electric-indent-mode -1))

(setq prelude-python-mode-hook 'prelude-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'prelude-python-mode-hook)))

(provide 'init-modes)
