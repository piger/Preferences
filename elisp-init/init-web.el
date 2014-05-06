;;; web stuff

;; web-mode
;; http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(setq web-mode-engines-alist
	  '(("django"		. "/templates/.*\\.html\\'") ; flask
            ("ctemplate"	. "/webui/index\\.html\\'")))

;; in caso di aggiunta runtime:
;; (add-to-list 'web-mode-engines-alist '("go" . "webui/foo\\'"))

(defun my-web-mode-hook ()
  "Customizations for web-mode"
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Disable TABs when indenting code.
(setq js2-mode-hook
      '(lambda () (progn
                    (set-variable 'indent-tabs-mode nil))))

;; handlebars
;(require 'handlebars-sgml-mode)
;(handlebars-use-mode 'minor)


(provide 'init-web)
