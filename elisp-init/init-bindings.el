;;; keyboard bindings

;;; ibuffer (meglio del mode di default per browsare i buffer aperti)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; swap default search mode to regexp 
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;; undo con C-z (al posto di minimize window)
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)

;; font-size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; magit-mode
(global-set-key (kbd "C-x g") 'magit-status)

;;; browser con M-o
(global-set-key "\M-o" 'browse-url-generic)
(if (and (eq window-system 'x) (eq system-type 'gnu/linux))
	(setq browse-url-generic-program "gvfs-open"))
(if (and (eq window-system 'ns) *is-a-mac*)
	(setq browse-url-generic-program "open"))

;;; RETURN -> indent (come fa C-j)
; (define-key global-map (kbd "RET") 'newline-and-indent)

(provide 'init-bindings)
