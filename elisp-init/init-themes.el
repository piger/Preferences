;;; themes and fonts

;;; THEME
;;; Setta il color-theme (nuovo stile, emacs 24+)
;(load-theme 'adwaita)
;(load-theme 'zenburn)
;(load-theme 'base16-default)
;(load-theme 'tomorrow-night)
;;(load-theme 'dichromacy)
;; (load-theme 'wilson)
;; (load-theme 'solarized-light)
;; (load-theme 'ample)
(load-theme 'tomorrow-night-eighties t)

;;; Fonts
;;; questo non saprei cosa fa:
; (add-to-list 'default-frame-alist
; 	     '(font . "DejaVu Sans Mono-13"))

;;; Per i font bisogna personalizzare custom.el
;;; (custom-set-faces
;;;  '(default ((t (:height 120 :family "Cousine"))))
;;; )


(provide 'init-themes)
