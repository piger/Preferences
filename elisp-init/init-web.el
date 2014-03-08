;;; web stuff

;; web-mode
;; http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))
(setq web-mode-engines-alist
	  '(("django"		. "/templates/.*\\.html\\'")
		("django"		. "/templates-ink/.*\\.html\\'")
		("ctemplate"	. "/webui/index\\.html\\'"))
)

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Disable TABs when indenting code.
(setq js2-mode-hook
	  '(lambda () (progn
					(set-variable 'indent-tabs-mode nil))))

;; handlebars
(require 'handlebars-sgml-mode)
(handlebars-use-mode 'minor)


(provide 'init-web)
