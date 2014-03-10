;;; python

;; show-paren-mode
(add-hook 'python-mode-hook
		  '(lambda () (show-paren-mode)))

;; yasnippet
(add-hook 'python-mode-hook
		  '(lambda () (yas-minor-mode)))

;; eldoc (mostra nome funzione corrente nel mini buffer)
;;(add-hook 'python-mode-hook
;;		  '(lambda () (eldoc-mode 1)) t)

(provide 'init-python)
