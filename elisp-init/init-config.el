;;; general configuration

; 29.3 Tabs vs. Spaces
; Settare indent-tabs-mode a nil per evitare che indent-to usi tabs.
;(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default cperl-indent-level 4)

;;; show column number by default
(setq column-number-mode t)

;;; transparency
(add-to-list 'default-frame-alist '(alpha 95 50))

;;(when *is-a-mac*
;;  (setq mouse-wheel-scroll-amount '(0.001)))

;; rember recent files
(recentf-mode 1)
(setq recentf-max-saved-items 200
	  recentf-exclude '("/tmp/" "/ssh:"))
;; (global-set-key (kbd "s-r") 'recentf-open-files)
;; il binding e' in init-functions, sulla funzione che integra ido

;; show parens mode
(show-paren-mode t)

;; align per puppet
;; https://github.com/jwiegley/dot-emacs/blob/master/lisp/puppet-ext.el
(add-hook 'puppet-mode-hook
		  (lambda ()
			(require 'align)
			(add-to-list 'align-rules-list
						 '(ruby-arrow
						   (regexp   . "\\(\\s-*\\)=>\\(\\s-*\\)")
						   (group    . (1 2))
						   (modes    . '(ruby-mode puppet-mode))))))

;;; save-place per salvare la posizione nel buffer quando si esce, tipo
;;; viminfo in vim.
;;; (require 'saveplace)
;;; (setq-default save-place t)

;; enable Multi Hops in TRAMP
;; aka: with this you can edit a remote file with sudo
;; C-x C-f /sudo:root@remote-host:/path/to-file
;; (require 'tramp)
;; (add-to-list 'tramp-default-proxies-alist
;; 	     '(nil "\\`root\\'" "/ssh:%h:"))
;; (add-to-list 'tramp-default-proxies-alist
;; 	     '((regexp-quote (system-name)) nil nil))

;; wcheck / aspell / hunspell
(setq wcheck-language-data
	  '(("Italian"
		 (program . "/usr/local/bin/hunspell")
		 (args "-l" "-d" "/Users/sand/Documents/dictionaries/dict-it-it_and_latin_2013-03-31/it_IT/it_IT")
		 (action-program . "/usr/local/bin/hunspell")
		 (action-args "-a" "-d" "/Users/sand/Documents/dictionaries/dict-it-it_and_latin_2013-03-31/it_IT/it_IT")
		 (action-parser . wcheck-parser-ispell-suggestions))

		("English"
		 (program . "/usr/local/bin/enchant")
		 (args "-l" "-d" "en")
		 (action-program . " /usr/local/bin/enchant")
		 (action-args "-a" "-d" "en")
		 (action-parser . wcheck-parser-ispell-suggestions))))

;		("English"
;		 (program . "/usr/local/bin/hunspell")
;		 (args "-l" "-d" "/Users/sand/Documents/dictionaries/en_us/en_US")
;		 (action-program . "/usr/local/bin/hunspell")
;		 (action-args "-a" "-d" "/Users/sand/Documents/dictionaries/en_us/en_US")
;		 (action-parser . wcheck-parser-ispell-suggestions))))

(provide 'init-config)
