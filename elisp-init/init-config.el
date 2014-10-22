;;; general configuration

;; save bookmarks every time a bookmark is added
(setq bookmark-save-flag 1)

; 29.3 Tabs vs. Spaces
;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

; Settare indent-tabs-mode a nil per evitare che indent-to usi tabs.
;(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default cperl-indent-level 4)

;;; show column number by default
(setq column-number-mode t)

;;; transparency
;; (add-to-list 'default-frame-alist '(alpha 95 80))

;; Save clipboard strings into kill ring before replacing them.
;; When one selects something in another program to paste it into Emacs,
;; but kills something in Emacs before actually pasting it,
;; this selection is gone unless this variable is non-nil,
;; in which case the other program's selection is saved in the `kill-ring'
;; before the Emacs kill and one can still paste it using C-y M-y.
;; Jul 2014 - disattivo per problemi su OS X, quando nel "buffer" di osx
;; non c'e' puro testo, emacs rompe il paste.
;; (setq save-interprogram-paste-before-kill t)

;; If non-nil, mouse yank commands yank at point instead of at click.
(setq mouse-yank-at-point t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; frame title
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; highlight the current line
;; (global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; disable startup screen
(setq inhibit-startup-screen t)

;;; disattivo la scroll-bar, effettivamente pia solo spazio.
(scroll-bar-mode -1)

;; line num
;; (global-linum-mode +1)

;; nice scrolling ???
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;;(when *is-a-mac*
;;  (setq mouse-wheel-scroll-amount '(0.001)))

;; rember recent files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15
      recentf-exclude '("/tmp/" "/ssh:"))
(recentf-mode +1)
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
;;; (setq save-place-file (concat user-emacs-directory "places"))

;; enable Multi Hops in TRAMP
;; aka: with this you can edit a remote file with sudo
;; C-x C-f /sudo:root@remote-host:/path/to-file
;; (require 'tramp)
;; (add-to-list 'tramp-default-proxies-alist
;; 	     '(nil "\\`root\\'" "/ssh:%h:"))
;; (add-to-list 'tramp-default-proxies-alist
;; 	     '((regexp-quote (system-name)) nil nil))
(require 'tramp)
(setq tramp-default-method "ssh")

;; undo-tree
(global-undo-tree-mode +1)
(diminish 'undo-tree-mode)

;; enable winner-mode to manage window configurations
(winner-mode +1)

;; flyspell
(defcustom pl-flyspell t
  "Non-nil values enable flyspell support."
  :type 'boolean
  :group 'prelude)

;; per ora disattivo flyspell di default
(setq pl-flyspell nil)

(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell
      ispell-extra-args '("--sug-mode=ultra"))

(defun pl-enable-flyspell ()
  "Prelude: enable command `flyspell-mode' if `pl-flyspell' is not nil."
  (when (and pl-flyspell (executable-find ispell-program-name))
    (flyspell-mode +1)))

(add-hook 'text-mode-hook 'pl-enable-flyspell)

;; anzu-mode enhances isearch by showing total matches and current match position
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
;; (setq gc-cons-threshold 50000000)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)


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
