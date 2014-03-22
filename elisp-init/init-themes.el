;;; themes and fonts

;;; THEME
;;; Setta il color-theme (nuovo stile, emacs 24+)
;(load-theme 'adwaita)
;(load-theme 'zenburn)
;(load-theme 'tomorrow-night)
;;(load-theme 'dichromacy)
;; (load-theme 'wilson t)
;; (load-theme 'solarized-light)
;; (load-theme 'ample t)
;(load-theme 'tomorrow-night-eighties t)
(load-theme 'tomorrow-day t)
;; (load-theme 'base16-default t)
; (load-theme 'flatui t)
; (load-theme 'soft-stone t)
; twilight?

;;; Fonts
;;; questo non saprei cosa fa:
; (add-to-list 'default-frame-alist
; 	     '(font . "DejaVu Sans Mono-13"))

;;; Per i font bisogna personalizzare custom.el
;;; (custom-set-faces
;;;  '(default ((t (:height 120 :family "Cousine"))))
;;; )


(provide 'init-themes)
