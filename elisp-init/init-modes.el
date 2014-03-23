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

;; highlight FIXME & co
(add-hook 'prog-mode-hook '(lambda () (prelude-font-lock-comment-annotations)))

;; subword mode (capisce CamelCase)
(add-hook 'js2-mode-hook (lambda ()
                           (subword-mode +1)))

(setq-default js2-global-externs '("module", "require", "console",
                                   "jQuery", "$"))
(add-hook 'js2-init-hook
          (lambda ()
            (when (string-match-p "zAFS" (buffer-file-name))
              (mapc (lambda (x)
                      (add-to-list 'js2-additional-externs x))
                    (list "Ember" "DS" "App")))))

; elisp defaults
(defun pl-elisp-mode-defaults ()
  "Some defaults for elisp mode"
  (turn-on-eldoc-mode)
  (rainbow-mode +1))
(setq pl-elisp-mode-hooks 'pl-elisp-mode-defaults)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'pl-elisp-mode-hooks)))


(provide 'init-modes)
