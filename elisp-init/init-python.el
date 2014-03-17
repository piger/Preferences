;;; python

;; eldoc (mostra nome funzione corrente nel mini buffer)
;;(add-hook 'python-mode-hook
;;		  '(lambda () (eldoc-mode 1)) t)

(defun prelude-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  ;;(smartparens-mode +1)
  (show-paren-mode +1)
  (yas-minor-mode +1)
  (electric-indent-mode -1))

(setq prelude-python-mode-hook 'prelude-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'prelude-python-mode-hook)))


(provide 'init-python)
