;;; emacs-config.el --- main emacs configuration file -*- lexical-binding: t; -*-

;; Main configuration file.

(defconst emacs-start-time (current-time)
  "This variable hold the time Emacs was started.")

;; load custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar piger/preferences-dir (expand-file-name "~/Preferences/elisp-init")
  "The directory containing my elisp files.")

(add-to-list 'load-path "~/Preferences/emacs/")
(add-to-list 'load-path piger/preferences-dir)

(defconst *is-a-mac* (eq system-type 'darwin))

;; tune GC
;; https://github.com/hlissner/doom-emacs/blob/master/core/core.el
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(when (>= emacs-major-version 25)
  (setq package-archive-priorities
        '(("melpa-stable" . 30)
          ("marmalade" . 20)
          ("gnu" . 10)
          ("melpa" . 40))))

; try to pin helm and helm-core (which is a dependency)
(setq package-pinned-packages
      '((helm . "melpa-stable")
        (helm-core . "melpa-stable")))

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)
(require 'bind-key)

;; OSX stuff
(when *is-a-mac*
  (use-package exec-path-from-shell
    :ensure t
    :config
    (setq exec-path-from-shell-variables
          '("PATH" "MANPATH" "PYTHONPAHT" "GOPATH" "JAVA_HOME"))
    (exec-path-from-shell-initialize))

  ;; To fix 'foodcritic' (flycheck)
  (setenv "LANG" "en_GB.UTF-8")

  ;; use vkill on OSX because proced doesn't work (stolen from prelude)
  ;; Update Oct 2019: https://github.com/bbatsov/prelude/issues/1170
  ;; this package was hosted on EmacsWiki and got removed from MELPA.
  (use-package vkill
    :ensure t
    :disabled t
    :bind
    (("C-x p" . vkill)))

  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  ;; (stolen from prelude)
  (if (fboundp 'set-fontset-font)
      ;; https://github.com/zonuexe/emoji-fontset.el
      (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))
      ;; (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

  ;; try to use GNU ls from coreutils (installed with homebrew)
  (let ((gnu-ls "/usr/local/bin/gls"))
    (when (file-exists-p gnu-ls)
      (setq insert-directory-program gnu-ls)
      (setq dired-listing-switches "-aBhl --group-directories-first")))

  ;; non so se serve anche questo:
  ;; (setq ls-lisp-use-insert-directory-program t)  ;; use external ls

  ;; default browser
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)

  ;; in dired use the trash
  (setq delete-by-moving-to-trash t))

;; Themes
(use-package poet-theme
  :config
  :disabled t
  (load-theme 'poet t))

(use-package base16-theme
  :ensure t
  :disabled t
  :config
  ;; (load-theme 'base16-railscasts t)
  ;; (load-theme 'base16-tomorrow-night t)
  ;; (load-theme 'base16-tomorrow t)
  ;; (load-theme 'base16-paraiso t)
  (load-theme 'base16-gruvbox-dark-hard t))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t)
  ;; color FIX for YAML...
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#ab4a85")
  :disabled t)

;; hack to use use-package with this theme:
;; https://github.com/nashamri/spacemacs-theme/issues/42
(use-package spacemacs-theme
  :ensure t
  :disabled t
  :defer t
  :init
  (load-theme 'spacemacs-light t))

(use-package birds-of-paradise-plus-theme
  :disabled t
  :ensure t
  :config
  ;; (load-theme 'birds-of-paradise-plus t))
  )

(use-package doom-themes
  :config
  ;; these are the package defaults; I'll leave them here in case I want to disable them in the
  ;; future.
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; (load-theme 'doom-palenight t)
  ;; (load-theme 'doom-dark+ t)
  (load-theme 'doom-peacock t)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)

  ;; Treemacs
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

(use-package chocolate-theme
  :disabled t
  :config
  (load-theme 'chocolate t))

;; Generic settings
;; from emacs-doom
(set-language-environment "UTF-8")

;; from emacs-doom
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system   'utf-8)   ; please
(setq-default buffer-file-coding-system 'utf-8) ; with sugar on top

;; disable scroll bars, toll bars, etc...
(when (functionp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'nil))
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; disable bell
(setq ring-bell-function #'ignore)

;; smooth mouse scrolling
;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; minibuffer history
(use-package savehist
  :init
  (setq savehist-file "~/.emacs.d/savehist")
  :config
  (savehist-mode 1))

;; font
(set-frame-font "Mononoki-12")

;; font & cursor (put this in init-local.el)
;; (set-face-attribute 'default nil
;;                     :family "Operator Mono"
;;                     :height 131
;;                     :width 'medium
;;                     :weight 'medium
;;                     :slant 'normal)
;; (set-face-attribute 'cursor nil :background "Orange")

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
(setq-default c-basic-offset 4)
(setq-default cperl-indent-level 4)

;;; show long lines as "wrapped"
(setq-default truncate-lines nil)

;;; set the fill column (for text indentation) to 100 columns (130 is also a good value)
(setq-default fill-column 100)

;;; show column number by default
(setq column-number-mode t)

;; show files size in minibar
(size-indication-mode t)

;; show keystrokes in minibuffer early
(setq echo-keystrokes 0.1)

;; delete region if typing
(pending-delete-mode 1)

;; Kill whole line
(setq kill-whole-line t)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 8))

;; ask for confirmation before exiting emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;;; transparency
;; (set-frame-parameter (selected-frame) 'alpha '(95 80))
(add-to-list 'default-frame-alist '(alpha 95 80))

;; Save clipboard strings into kill ring before replacing them.
;; When one selects something in another program to paste it into Emacs,
;; but kills something in Emacs before actually pasting it,
;; this selection is gone unless this variable is non-nil,
;; in which case the other program's selection is saved in the `kill-ring'
;; before the Emacs kill and one can still paste it using C-y M-y.
;; Jul 2014 - disattivo per problemi su OS X, quando nel "buffer" di osx
;; non c'e' puro testo, emacs rompe il paste.
;; (setq save-interprogram-paste-before-kill t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; frame title
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; highlight the current line
(global-hl-line-mode +1)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; disable startup screen
(setq inhibit-startup-screen t)

;; this requires "fortune" to be installed.
(use-package fortune-cookie
  :ensure t
  :config
  (when (file-exists-p "~/Dropbox/fortunes")
    (setq fortune-cookie-fortune-args (list (expand-file-name "~/Dropbox/fortunes"))))
  (setq fortune-cookie-cowsay-enable nil)
  (fortune-cookie-mode))

;; line num
;; (global-linum-mode +1)

;; nice scrolling ???
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;;(when *is-a-mac*
;;  (setq mouse-wheel-scroll-amount '(0.001)))

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

;; enable Multi Hops in TRAMP
;; aka: with this you can edit a remote file with sudo
;; C-x C-f /sudo:root@remote-host:/path/to-file
;; (require 'tramp)
;; (add-to-list 'tramp-default-proxies-alist
;;           '(nil "\\`root\\'" "/ssh:%h:"))
;; (add-to-list 'tramp-default-proxies-alist
;;           '((regexp-quote (system-name)) nil nil))
(defun hostnames-from-file (filename)
  (split-string
   (with-temp-buffer
     (insert-file-contents filename)
     (buffer-substring-no-properties
      (point-min)
      (point-max))) "\n" t))

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "/etc/ssh/ssh_config")
                                   (tramp-parse-sconfig "~/.ssh/config")
                                   (tramp-parse-hosts "/etc/hosts"))))
;; (setq tramp-default-method "ssh")

;; 08/04/2015 - mi stai sul cazzo porcodio, ti commento
;; (add-hook 'text-mode-hook (lambda () (flyspell-mode +1)))


;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; extracts from better defaults: https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(use-package saveplace
  :config
  (save-place-mode 1))

(setq apropos-do-all t
      ;; If non-nil, mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t
      visible-bell t
      load-prefer-newer t
      save-place-file (concat user-emacs-directory "places")
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups")))
      ;; don't clutter the fs with auto-save files (they might be uploaded to chef during knife upload -_-)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Custom functions
;;; https://github.com/magnars/.emacs.d/blob/master/defuns/buffer-defuns.el
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

;; shutdown emacs server
;; http://www.emacswiki.org/emacs/EmacsAsDaemon
(defun shutdown-server ()
  "Save buffers, Quit and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defvar prelude-tips
  '("Press <C-c o> to open a file with external program."
    "Access the official Emacs manual by pressing <C-h r>."
    "Press <C-x v v> to do the next logical version control operation"
    "Magit is available with <C-x g>"
    "Press <j u> in Magit to jump to unstaged files"
    "disable-theme can unload a theme"
    "<C-:> is avy (the new ace-jump)"
    "<M-s o> is occur which is a nice thing to use, especially with ivy/counsel!"
    "<C-c p s s> runs ag on the projectile project"
    "<C-c p k> to close all the buffers of a project"
    "<C-x j> to switch window layout (transpose-frame)"
    "<C-h l> or view-lossage is the command to know How Did I Get There?"
    "(inf-ruby) is nicer than opening irb in a terminal window"
    "<C-x 4 w> to check a buffer with LangTool and <C-x 4 W> to exit"
    "Visit the EmacsWiki at http://emacswiki.org to find out even more about Emacs."))

(defun prelude-tip-of-the-day ()
  "Display a random entry from `prelude-tips'."
  (interactive)
  (unless (window-minibuffer-p)
    ;; pick a new random seed
    (random t)
    (message
     (concat "Tip of the day: " (nth (random (length prelude-tips)) prelude-tips)))))

(defun prelude-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(prelude-eval-after-init
 ;; greet me with useful tips
 (run-at-time 5 nil 'prelude-tip-of-the-day))

;; google
;; http://emacsredux.com/blog/2013/03/28/google/
(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "https://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

(defun prelude-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
(font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))

;; Use this command to create a new terminal buffer; use =C-x C-j= to
;; switch to =term-line-mode=, where you can select text and =C-c C-k= to
;; switch back to =character-mode=.
(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

;; Reopen the current visited file as root using tramp and sudo; I stole
;; this from prelude but I never used it.
(defun prelude-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Search Wikipedia using `eww'
(defun piger/eww-wiki (text)
  "Search TEXT inside Wikipedia using eww."
  (interactive (list (read-string "Wiki for: ")))
  (eww (format "https://en.wikipedia.org/wiki/Special:Search?search=%s"
                (url-encode-url text))))

;; Ansi colors (for console dumps from samson, for example)
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; Ricompila i file .el che si trovano in ~/.emacs.d
(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;; Per joinare una /region/
(defun join-region (beg end)
  "Apply join-line over region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))

;; To align terraform statements
(defun recker/do-fancy-equal-thingy (beg end)
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)\\ =" 1 0 t))

;; Set a larger font when a Thunderbolt Display is connected.
(defvar piger/font-small "Mononoki-11"
  "The font to use when there is no external monitor connected.")

(defvar piger/font-large "Mononoki-13"
  "The font to use when there is an external monitor connected.")

(defun set-the-right-font ()
  "Set the right font according to the connected displays"
  (interactive)
  (let ((monitors (shell-command-to-string "system_profiler SPDisplaysDataType | egrep '^ {8}[^ ]' | sed -e 's/^ *//' -e 's/:$//'"))
        (hasExternal nil))
    (dolist (monitor (split-string monitors "\n"))
      (when (string= monitor "Thunderbolt Display")
        (setq hasExternal t)))
    (if hasExternal
        (set-default-font piger/font-large)
      (set-default-font piger/font-small))))

(defun my/terminal-notifier-notify (title message)
  "Show a message with `terminal-notifier-command`."
  (interactive)
  (start-process "terminal-notifier"
                 "*terminal-notifier*"
                 "terminal-notifier"
                 "-title" title
                 "-message" message))

(defun piger/align-vars (start end)
  "Veritcally align stuff.
Example:

  owner 'root'
  group 'root'
  mode '0644'
  source options['nginx']['key_url']

becomes

  owner  'root'
  group  'root'
  mode   '0644'
  source options['nginx']['key_url']"

  (interactive "r")
  (align-regexp start end "\\S-+\\(\\s-+\\)" 1 2 nil))

(defun arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

;; Keyboard bindings
;; Mac OS X customization. Note that you should use my modified keyboard layout which permits
;; accented characters.
(when *is-a-mac*
  ;; NOTE: this is for the emacs version "emacs-mac": https://bitbucket.org/mituharu/emacs-mac/overview
  ;; install it with: brew cask install emacs-mac (see https://github.com/railwaycat/homebrew-emacsmacport)

  ;; Smart assignments of Mac specific keys
  (setq mac-option-modifier 'meta)
  ;; I need this set to "super" to have simpleclip work; I think the author recommends "hyper" instead:
  ;; https://gist.github.com/railwaycat/3498096
  (setq mac-command-modifier 'super)

  ;(setq mac-function-modifier 'super)  ;; questo sposta SUPER sul tasto Fn
  (setq mac-right-option-modifier nil) ;; questo permette le accentate con ALT destro

  ;; Disable OS X clipboard integration (kill-ring, yank-ring, ...)
  (setq interprogram-cut-function nil
        interprogram-paste-function nil))

;; Use simpleclip to bind CMD+c, CMD+v, CMD+x to copy, yank, cut
(use-package simpleclip
  :ensure t
  :if *is-a-mac*
  :config
  (setq simpleclip-unmark-on-copy t)
  (simpleclip-mode +1))

;; hippie-expand al posto di dabbrev-expand dabbrev
;; <2015-07-05 Sun> lo disabilito perché mi sembra esagerato.
;; (global-set-key (kbd "M-/") 'hippie-expand)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
;; (global-set-key (kbd "s-/") #'hippie-expand)

;;; swap default search mode to regexp
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;; undo con C-z (al posto di minimize window)
;(global-unset-key "\C-z")
;(global-set-key (kbd "\C-z") 'undo)

;; font-size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;; browser con M-o (I think this one is covered by crux?)
(global-set-key "\M-o" 'browse-url-generic)
(if (and (eq window-system 'x) (eq system-type 'gnu/linux))
    (setq browse-url-generic-program "gvfs-open"))
(if (and (eq window-system 'ns) *is-a-mac*)
    (setq browse-url-generic-program "open"))
(if (and (eq window-system 'mac) *is-a-mac*)
    (setq browse-url-generic-program "open"))

;;; RETURN -> indent (come fa C-j)
; (define-key global-map (kbd "RET") 'newline-and-indent)

;; Extra scripts
(use-package nagios-mode
  :commands nagios-mode
  :load-path "~/Preferences/elisp"
  :if (file-exists-p "~/Preferences/elisp"))

;; Programming
(use-package subword
  :commands subword-mode
  :diminish subword-mode)

(use-package abbrev
  :commands abbrev-mode
  :diminish abbrev-mode)

;; Assign a specific mode for certain directories
;; note: you can't chain multiple paths in a single add-to-list call :(
(add-to-list 'auto-mode-alist '("/Documents/appunti/[^/]*\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("/Preferences/zsh/" . shell-script-mode))
(add-to-list 'auto-mode-alist '("/Preferences/emacs/snippets/" . snippet-mode))

;; See also: http://www.flycheck.org/en/latest/languages.html#flycheck-languages
(use-package flycheck
  :ensure t
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :config
  ;; spaceline provides his own indicator for that
  (setq-default flycheck-mode-line nil)
  (setq flycheck-indication-mode 'right-fringe)

  ;; replace flycheck's wavy underline with a straight line
  (set-face-attribute 'flycheck-error nil :underline '(:color "#d32e00"))
  (set-face-attribute 'flycheck-warning nil :underline '(:color "#e3795c"))
  (set-face-attribute 'flycheck-info nil :underline '(:color "ForestGreen"))

  ;; make the flycheck arrow look like an exclamation point.
  ;; but only do it when emacs runs in a window, not terminal
  (when window-system
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [0 24 24 24 24 24 24 0 0 24 24 0 0 0 0 0 0]))

  (defun flycheck-foodcritic-porcodio ()
    "avoid issue with use-package macro expansion."
    (let ((parent-dir (f-parent default-directory)))
        (or
         (locate-dominating-file parent-dir "recipes")
         (locate-dominating-file parent-dir "cookbooks"))))

  (flycheck-define-checker chef-foodcritic
    "A Chef cookbooks syntax checker using Foodcritic."
    :command ("foodcritic" source)
    :error-patterns
    ((error line-start (message) ": " (file-name) ":" line line-end))
    :modes (enh-ruby-mode ruby-mode)
    :predicate flycheck-foodcritic-porcodio
    :next-checkers ((warnings-only . ruby-rubocop)))

  (global-flycheck-mode))

(use-package ediff
  :commands ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  ; ignore all white spaces
  (setq ediff-diff-options "-w"))

;;; elpy requires the 'jedi' python package
(use-package elpy
  :disabled t
  :commands elpy-enable)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook (python-mode . my-python-mode-hook)
  :config
  (defun my-python-mode-hook ()
    (subword-mode +1)
    (show-paren-mode +1)
    (flycheck-mode +1)
    (company-mode +1)
    (eldoc-mode +1)
    ;; (elpy-enable)
    ;; unfuck electric indentation
    (setq electric-indent-chars '(?\n))))

(use-package anaconda-mode
  :disabled t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :disabled t
  ;;; :requires (anaconda-mode company-mode)
  :after (company anaconda-mode)
  :config
  (with-eval-after-load 'company
    '(add-to-list 'company-backends 'company-anaconda)))

;; IPython / Jupiter notebook support
;; https://github.com/millejoh/emacs-ipython-notebook
(use-package ein
  :commands (ein:jupyter-server-start ein:notebooklist-login))

;; go
;; requires a bunch of tools:
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/nsf/gocode
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u golang.org/x/tools/cmd/gotype
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/gomvpkg
;; go get -u golang.org/x/tools/cmd/godex
;; NOTE: "oracle" waa renamed to "guru"
;;
;; Also "GOPATH" must be imported by exec-path-from-shell.
; Those env variables should be inherithed using exec-path-from-shell
; (setenv "GOPATH" (expand-file-name "~/dev/go"))
; (setenv "PATH" (concat (getenv "PATH") ":" (concat (getenv "GOPATH") "/bin")))
; (setq exec-path (append exec-path (list (expand-file-name "~/dev/go/bin"))))
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :bind ("M-." . godef-jump)
  :config
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save nil t)
    (setq gofmt-command "goimports")
    (with-eval-after-load 'company
      '(add-to-list 'company-backends 'company-go))
    (if (not (string-match "go" compile-command))
             (set (make-local-variable 'compile-command)
                  "go build -v && go test -v && go vet"))
    (go-eldoc-setup)
    (setq tab-width 4)
    (local-set-key (kbd "C-c C-k") 'godoc)
    (subword-mode +1)
    ;; (company-mode)
    (flycheck-mode)
    (go-guru-hl-identifier-mode)
    ;; (local-set-key (kbd "M-.") 'godef-jump)
    (diminish 'subword-mode))
  :hook (go-mode . my-go-mode-hook))

(use-package go-eldoc
  :commands (go-eldoc-setup)
  :ensure t)

(use-package gotest
  :after go-mode
  :ensure t)

(use-package go-guru
  :commands (go-guru-hl-identifier-mode)
  :config
  (defun projectile-guru-scope ()
    "Set the go guru scope from the projectile root directory."
    (interactive)
    (unless (= (length (projectile-project-root)) 0)
      (setq go-guru-scope (concat
                           (replace-regexp-in-string
                            (concat "^" (file-name-as-directory (getenv "GOPATH")) "src/") "" (projectile-project-root)) "..."))))
  :after go-mode)

(use-package rust-mode
  :mode "\\.rs\\'")

;; Collection of handy functions for ruby-mode
;; https://github.com/rejeep/ruby-tools.el
(use-package ruby-tools
  :diminish ruby-tools-mode
  :disabled t
  :hook ruby-mode)

(use-package rubocop
  :commands rubocop-mode
  :diminish rubocop-mode
  :hook (ruby-mode . rubocop-mode))

;; inf-ruby provides a REPL buffer connected to a Ruby subprocess.
(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  :bind
  (("C-c r r" . inf-ruby)))

(use-package robe
  :after (ruby company)
  :hook ruby-mode
  :config
  (with-eval-after-load 'company
    '(add-to-list 'company-backends 'company-robe)))

(defun piger/ruby-mode-hooks ()
  "Personalised 'ruby-mode' hooks."
  (cond ((string-match "/code/misc/zcfn/" buffer-file-name)
         (set (make-local-variable 'comment-auto-fill-only-comments) t)
         (setq-local fill-column 140)
         (auto-fill-mode t)))
  (subword-mode +1))

;; alternative to the above hook.
;; Put this in .dir-locals.el in the project root.
;; ((ruby-mode . ((fill-column . 80)
;;                (eval . (progn (auto-fill-mode t)
;;                               (set (make-local-variable 'comment-auto-fill-only-comments) t))))))

;; I use enh-ruby-mode because indentation in ruby-mode is fucked up
(use-package enh-ruby-mode
  :interpreter "ruby"
  :disabled t
  :mode ("\\.rb\\'"
         "\\.ru\\'"
         "\\.rake\\'"
         "\\.gemspec\\'"
         "Gemfile\\'"
         "Berksfile\\'"
         "Rakefile\\'"
         "Vagrantfile\\'"
         "Capfile\\'")
  :hook (enh-ruby-mode . subword-mode)
  :init
  (setq ruby-insert-encoding-magic-comment nil)
  (setq enh-ruby-indent-level 2
        enh-ruby-deep-indent-paren nil)
  :config
  (add-hook 'ruby-mode-hook 'piger/ruby-mode-hooks)
  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc"))

(use-package ruby-mode
  :interpreter "ruby"
  :mode ("\\.rb\\'"
         "\\.ru\\'"
         "\\.rake\\'"
         "\\.gemspec\\'"
         "Gemfile\\'"
         "Berksfile\\'"
         "Rakefile\\'"
         "Vagrantfile\\'"
         "Capfile\\'")
  :hook (ruby-mode . subword-mode)
  :custom
  (ruby-insert-encoding-magic-comment nil)
  :config
  (add-hook 'ruby-mode-hook 'piger/ruby-mode-hooks)
  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc"))

;; (use-package ruby-electric
;;   :disabled t
;;   :hook (ruby-mode . ruby-electric-mode))

(use-package rbenv
  :ensure t
  :init
  ;; rbenv installed via homebrew
  (setq rbenv-executable "/usr/local/bin/rbenv")
  (setq rbenv-modeline-function 'rbenv--modeline-plain)
  (setq rbenv-show-active-ruby-in-modeline nil)
  :config
  (global-rbenv-mode)
  (rbenv-use-global))

(use-package bundler
  :commands (bundle-check bundle-open bundle-update bundle-console bundle-install))

(use-package rake
  :commands rake)

(use-package rspec-mode
  :requires ruby-mode)

;; File mode specification error: (error Autoloading file /Users/dkertesz/.emacs.d/elpa/ruby-end-20141215.1223/ruby-end.elc failed to define function ruby-end)
(use-package ruby-end
  :diminish
  :disabled t
  :pin melpa
  :hook ruby-mode)

(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :custom
  (css-indent-offset 2)
  :hook ((css-mode . rainbow-mode)
         (css-mode . subword-mode)))

(use-package rainbow-mode
  :requires css-mode
  :ensure t)

(use-package less-css-mode
  :requires css-mode
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :disabled t
  :hook (js2-mode . subword-mode)
  :config
  (setq-default js2-global-externs
                '("module", "require", "console", "jQuery", "$"))
  (setq js-indent-level 4
        js2-basic-offset 4)
  (add-hook 'js2-init-hook
            (lambda ()
              (when (or (string-match-p "ProjectName1" (buffer-file-name))
                        (string-match-p "ProjectName2" (buffer-file-name)))
                (mapc (lambda (x)
                        (add-to-list 'js2-additional-externs x))
                      (list "Ember" "DS" "App"))))))

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :hook (rjsx-mode . subword-mode))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (defun my-json-mode-hook ()
    (flycheck-mode +1))
  :hook (json-mode . my-json-mode-hook))

(use-package web-mode
  :ensure t
  :mode ("\\.erb\\'"
         "\\.hbs\\'"
         "\\.html?\\'"
         "\\.j2\\'")
  :init
  (setq web-mode-engines-alist
        '(("go" . "/go/src/.*\\.html\\'")
          ("django" . "/dev/.*/templates/.*\\.html\\'")))
  :hook (web-mode . my-web-mode-hook)
  :config
  (defun my-web-mode-hook ()
    (local-set-key (kbd "RET") 'newline-and-indent)
    ;; (yas-minor-mode +1)
    (whitespace-cleanup-mode +1))
  (defun piger/web-mode-set-engine ()
    "Set web-mode engine based on some conditions."
    (if (and (file-exists-p (concat (projectile-project-root) "archetypes"))
             (file-exists-p (concat (projectile-project-root) "config.toml")))
        (web-mode-set-engine "go")))
  (add-hook 'web-mode-hook 'piger/web-mode-set-engine)

  (setq web-mode-enable-current-element-highlight t
        web-mode-enable-auto-quoting -1
        web-mode-code-indent-offset 4
        web-mode-markup-indent-offset 4))

;; To edit the engine list:
;; (setq web-mode-engines-alist (append '(("django" . "/sand/src/.*templates/")) web-mode-engines-alist))

(use-package logstash-conf
  :ensure t
  :commands logstash-conf-mode
  :config
  (setq logstash-indent 2))
  ;;(custom-set-variables '(logstash-indent 2)))

;; use this by calling "c-set-style"
(defconst piger-cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "piger-cc-style" piger-cc-style)

(use-package csharp-mode
  :mode "\\.cs\\'"
  :hook (csharp-mode . my-csharp-mode-hook)
  :config
  (defun my-csharp-mode-hook ()
    (electric-pair-local-mode 1)
    (eldoc-mode)
    (flycheck-mode)
    (company-mode 1)))

(use-package omnisharp
  :after csharp-mode
  :hook (csharp-mode . omnisharp-mode)
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp)))

(use-package php-mode
  :mode "\\.php\\'"
  :ensure t)

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :hook (prog-mode . highlight-symbol-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; https://github.com/mickeynp/smart-scan
;; Jump with M-n and M-p.
;; TODO fix init
;; Done (Total of 1 file compiled, 2 skipped)
;; Settings loaded in 46.471s
;; Saving file /Users/dkertesz/.emacs.d/custom.el...
;; Wrote /Users/dkertesz/.emacs.d/custom.el [2 times]
;; run-hooks: Cannot open load file: No such file or directory, smartscan
;; Cannot open load file: No such file or directory, smartscan
;; File mode specification error: (file-missing Cannot open load file No such file or directory smartscan)
(use-package smartscan
  :disabled t
  :hook (prog-mode . smartscan-mode))

;; Minor mode to selectively hide/show code and comment blocks.
(use-package hideshow
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :config
  (setq hs-special-modes-alist
        (mapcar 'purecopy
                '((c-mode "{" "}" "/[*/]" nil nil)
                  (json-mode "{" "}" "/[*/]" nil)
                  (js-mode "{" "}" "/[*/]" nil)
                  (javascript-mode "{" "}" "/[*/]" nil)))))

;; Highlight indentation with coloured bars.

;; *NOTE* its color autodetection code has some sort of bugs that triggers on init, while displaying
;; the scratch buffer (which do uses prog-mode) *while* the theme is still loading (i.e. still not
;; being displayed to the user):

;; color-values: Wrong type argument: stringp, nil

;; The "fix", since I only ever use one single theme nowadays, is to cheat: inspect the variables set
;; by the autodetect code and hardcode them in the configuration.

;; To get the current face color use this:

;; (face-attribute 'highlight-indent-guides-character-face :background)

;; Replace =:background= with =:foreground= when needed.

(use-package highlight-indent-guides
  ;; attempt to workaround the bug described above
  :defer 5
  ;;:hook (prog-mode . highlight-indent-guides-mode)
  :config
  ;; (setq highlight-indent-guides-auto-odd-face-perc 5
  ;;       highlight-indent-guides-auto-even-face-perc 10
  ;;       highlight-indent-guides-auto-character-face-perc 10
  ;;       highlight-indent-guides-auto-enabled nil)
  ;; (set-face-background 'highlight-indent-guides-odd-face "#f5efda")
  ;; (set-face-background 'highlight-indent-guides-even-face "#f0e6c6")
  ;; (set-face-foreground 'highlight-indent-guides-character-face "#f0e6c6"))
  ;;(run-with-timer 10 nil 'add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
  )

;; Other modes
;; ---------------------------------------------------------------------------------------
(ido-mode -1)

(use-package helm
  :pin melpa-stable
  :disabled t
  :config
  (require 'helm-config)

  (setq
   ;; Limit candidate number globally.
   helm-candidate-number-limit 100
   ;; open helm buffer inside current window, not occupy whole other window.
   ;; Maybe it's better to use the other half of the screen, it will contain more elements
   ;; than the minibuffer popup.
   ; helm-split-window-in-side-p t
   ;; skip files which you usually don't want to open
   ;; NO! this will make impossible to edit anything in .git/ !
   ;; helm-ff-skip-boring-files t
   helm-ff-file-name-history-use-recentf t
   ;; Max length of buffer names before truncate.
   helm-buffer-max-length 40)
  (helm-mode 1)
  ;; Enable "adaptive" (i.e. most frequent) sorting in Helm
  (helm-adaptive-mode 1)
  ;; What does this do??
  ; (helm-push-mark-mode 1)
  :diminish helm-mode
  :bind (("C-c h"   . helm-mini)
         ("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-x"     . helm-M-x)
         ("M-s o"   . helm-occur)
         ("M-y"     . helm-show-kill-ring)
         ("C-x C-d" . helm-browse-project)
         ("C-c i"   . helm-imenu-all-buffers)
         ; erano
         ; (define-key global-map (kbd "M-g a") 'helm-do-grep-ag)
         ("M-g a"   . helm-do-grep-ag)
         ("M-g g"   . helm-grep-do-git-grep)))

(use-package helm-ls-git
  :disabled t)

(use-package helm-ag
  :disabled t
  :requires helm)

(use-package helm-swoop
  :config
  :disabled t
  (global-set-key (kbd "C-c o") 'helm-swoop))

(use-package ivy
  :pin melpa
  :ensure t
  :diminish
  :demand t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x C-r" . counsel-recentf)
         ("C-c C-r" . ivy-resume))
  :custom
  (ivy-use-virtual-buffers t)
  ;;; to create a directory when ivy is stubborn, either press C-M-j or enable this:
  (ivy-use-selectable-prompt t)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1))

;; fancy popup window
(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-mode 1))

;; fancy descriptions in M-x
(use-package ivy-rich
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :pin melpa
  :after ivy
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-h f"   . counsel-describe-function)
   ("C-h v"   . counsel-describe-variable)
   ("C-c k"   . counsel-ag)
   ("C-c g"   . counsel-git)
   ("C-c j"   . counsel-git-grep)
   ("M-y"     . counsel-yank-pop))
  :diminish
  :config
  (counsel-mode 1)
  ;;; ignored files in C-x C-f
  (setq counsel-find-file-ignore-regexp "\\.pyc\\'"))

(use-package swiper
  :after ivy
  :bind
  ;;; NOTE: those are not the default bindings
  (("C-s" . swiper)
   ("C-r" . swiper)))

;; smex can augment counsel-M-x, adding for example the recent used commands.
(use-package smex
  ;; :bind (("M-x" . smex)
  ;;        ("M-X" . smex-major-mode-commands)
  ;;        ("C-c C-c M-x" . execute-extended-command))
  :config
  (smex-initialize))

(use-package ibuffer
  :ensure t
  :bind ("C-x C-b" . ibuffer))

;; meaningful names for buffers with the same name
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t          ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"))     ; don't muck with special buffers

;; Nice keybinding to switch frame using shift or alt + arrows
(use-package windmove
  ; Bind additional keys (other than the default meta+arrows) for modes where
  ; the default keys conflicts.
  :bind (("C-x <up>" . windmove-up)
         ("C-x <down>" . windmove-down)
         ("C-x <left>" . windmove-left)
         ("C-x <right>" . windmove-right))
  :config
  (windmove-default-keybindings 'meta))

;; This package provides some useful commands to move windows around, for example =transpose-frame=
;; on a frame with two vertical windows will give you an horizontal split.
(use-package transpose-frame
  :ensure t
  :bind (("C-x j" . transpose-frame)))

(use-package yasnippet
  :ensure t
  :disabled t
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/Preferences/emacs/snippets")))
  (yas-reload-all))

(use-package re-builder
  :disabled t
  :config
  (setq reb-re-syntax 'string))

;; Why 70 columns?
;; http://stackoverflow.com/questions/2290016/git-commit-messages-50-72-formatting

(use-package magit-popup
  :defer t
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-diff-refine-hunk 'all
        git-commit-turn-on-flyspell t
        git-commit-turn-on-auto-fill t
        git-commit-fill-column 72
        magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1
        git-commit-summary-max-length 70)
  (global-magit-file-mode 1)

  (use-package git-commit
    :ensure t
    :hook (git-commit-setup . git-commit-turn-on-flyspell)))

(use-package forge
  :after magit
  :ensure t
  :config
  (setq forge-pull-notifications nil))

(use-package github-review
  :ensure t
  :after forge)

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config
  :disabled t
  ;(git-gutter:linum-setup)
  (add-hook 'prog-mode-hook 'git-gutter-mode))

(use-package diff-hl
  :disabled t
  :hook ((prog-mode . diff-hl-mode)
         ;;; this enable the "live" mode, similar to flydiff.
         (prog-mode . diff-hl-flydiff-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package gitconfig-mode
  :ensure t
  :mode ("\\.?gitconfig\\'" . gitconfig-mode))

(use-package gitignore-mode
  :ensure t
  :mode ("\\.gitignore" . gitignore-mode))

(use-package magit-gh-pulls
  :disabled t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package gist
  :defer t)

(use-package git-timemachine
  :ensure t
  :bind (("C-x v t" . git-timemachine)))

(use-package github-clone
  :bind (("C-x v c" . github-clone)))

(use-package git-link
  :bind (("C-x v b" . git-link))
  :config
  (setq git-link-open-in-browser t))

(use-package magithub
  :after magit
  :disabled t
  :config
  (magithub-feature-autoinject t))

(use-package dired
  :bind ("C-x d" . dired)
  :hook (dired-mode . hl-line-mode))

(use-package dired-x
  :after dired)

;; This is useful to mark /things/ inside markers, for example the text inside a quoted string or
;; inside some parenthesis.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; (require 'move-text)
;; i binding di default sono alt+up e alt+down, gli stessi che uso
;; per switchare finestra.
;; (move-text-default-bindings)

(use-package evil
  :ensure t
  :commands (evil-mode evil-local-mode)
  :init
  (setq evil-want-C-u-scroll t) ; enable scroll-down with C-u
  :config
  (setq evil-emacs-state-cursor  '("red" box)
        evil-normal-state-cursor '("gray" box)
        evil-visual-state-cursor '("gray" box)
        evil-insert-state-cursor '("gray" bar)
        evil-motion-state-cursor '("gray" box)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "markdown")
  (setq markdown-fontify-code-blocks-natively t)
  (set-face-attribute 'markdown-pre-face nil :inherit 'markdown-markup-face)
  ;; (custom-set-faces
  ;;  '(markdown-pre-face ((t (:inherit markdown-markup-face)))))
  (add-hook 'markdown-mode-hook 'turn-on-auto-fill))

(use-package rst-mode
  :mode "\\.rst\\'"
  :config
  (add-hook 'rst-mode-hook 'turn-on-auto-fill))

;; =adoc-mode= use those markup-faces for headers and properties, and by default those have weird
;; sizes. Also you can't configure those fonts until you "activate" them, so it needs to be done in
;; =:config=.
(use-package adoc-mode
  :mode "\\.adoc\\'"
  :config
  (set-face-attribute 'markup-title-0-face nil :height 1.5)
  (set-face-attribute 'markup-title-1-face nil :height 1.4)
  (set-face-attribute 'markup-title-2-face nil :height 1.3)
  (set-face-attribute 'markup-title-3-face nil :height 1.2)
  (set-face-attribute 'markup-title-4-face nil :height 1.1)
  (set-face-attribute 'markup-title-5-face nil :height 1.0)
  (set-face-attribute 'markup-secondary-text-face nil :height 1.0)
  (set-face-attribute 'markup-meta-face nil :height 1.0)
  (set-face-attribute 'markup-meta-hide-face nil :height 1.0))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'"
  :hook (yaml-mode . flycheck-mode))

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package terraform-mode
  :mode "\\.tf\\'"
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package company-terraform
  :after (terraform-mode company)
  :config
  (company-terraform-init))

(use-package hcl-mode
  :mode "\\.hcl\\'")

(use-package nginx-mode
  :ensure t
  :commands nginx-mode)

(use-package ssh-config-mode
  :mode "\\.ssh/config\\'"
  :ensure t)

(use-package systemd
  :ensure t
  :mode ("\\.service\\'" . systemd-mode))

;; gettext on OS X (homebrew) ships with additional elisp files
(use-package po-mode
  :if (file-exists-p "/usr/local/opt/gettext/share/emacs/site-lisp")
  :load-path "/usr/local/opt/gettext/share/emacs/site-lisp"
  :mode "\\.po\\'\\|\\.po\\.")

(defun piger/prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (rainbow-delimiters-mode t)
  (company-mode t)
  (prelude-font-lock-comment-annotations)
  (subword-mode t)
  (which-function-mode t)
  (diminish 'subword-mode))

(setq piger/prog-mode-hook 'piger/prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'piger/prog-mode-hooks)))

;; spell check comments and strings
; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             ;; (use-package idle-highlight-mode
;;             ;;   :init (idle-highlight-mode t))
;;             (prelude-font-lock-comment-annotations)
;;             (rainbow-delimiters-mode t)
;;             ;; (setq show-trailing-whitespace t)
;;             (subword-mode t)))

; elisp defaults
(defun pl-elisp-mode-defaults ()
  "Some defaults for elisp mode"
  (turn-on-eldoc-mode)
  (diminish 'eldoc-mode)
  (rainbow-mode +1)
  (diminish 'rainbow-mode))
(setq pl-elisp-mode-hooks 'pl-elisp-mode-defaults)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'pl-elisp-mode-hooks)))

(use-package projectile
  :ensure t
  :init
  ;; like here: https://github.com/bbatsov/emacs.d/blob/8962c0f09abd261f76f00afb64408fd658eb3028/init.el#L286
  (setq projectile-completion-system 'ivy)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-mode-line '(:eval (format " &{%s}" (projectile-project-name)))
        projectile-globally-ignored-directories (quote (".git" ".tox" "Godeps" "build")))
  (add-to-list 'projectile-globally-ignored-files ".pyc")
  (add-to-list 'projectile-globally-ignored-files "__pycache__")
  ;;; https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-projects.el#L32-L38
  ;; Projectile root-searching functions can cause an infinite loop on TRAMP
  ;; connections, so disable them.
  (defun doom*projectile-locate-dominating-file (orig-fn &rest args)
    "Don't traverse the file system if on a remote connection."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'projectile-locate-dominating-file :around #'doom*projectile-locate-dominating-file))

(use-package helm-projectile
  :disabled t
  :config
  (helm-projectile-on))

(use-package ibuffer-projectile
  :disabled t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package counsel-projectile
  :after (counsel projectile)
  :pin melpa
  :ensure t
  :config
  (counsel-projectile-mode 1))

(use-package company
  :ensure t
  :diminish
  :hook (prog-mode . company-mode)
  :config
  (setq company-transformers '(company-sort-by-occurrence)))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

(use-package company-go
  :after (company go)
  :ensure t)

(use-package company-web
  :after (company web)
  :ensure t)

(use-package apache-mode
  :ensure t
  :mode (("\\.htaccess\\'" . apache-mode)
         ("/apache2?/sites-\\(available|enabled\\)/" . apache-mode)))

;; code folding with vim compatibility
;; https://raw.githubusercontent.com/yyetim/emacs-configuration/master/elisp/vim-fold.el
;; modificato leggermente, perche' io i marker li uso anche senza numero (e.g. "{{{1")
;; per indicare il livello di outline.
(defun set-vim-foldmarker (fmr)
  "Configure a Vim-like foldmarker for the current buffer, used with outline-mode"
  (interactive "sSet local Vim foldmarker: ")
  (if (equal fmr "")
      (message "Abort")
    (setq fmr (regexp-quote fmr))
    (set (make-local-variable 'outline-regexp)
         (concat ".*" fmr "\\([0-9]+\\)?"))
    (set (make-local-variable 'outline-level)
         `(lambda ()
            (save-excursion
              (re-search-forward
               ,(concat fmr "\\([0-9]+\\)") nil t)
              (if (match-string 1)
                  (string-to-number (match-string 1))
                (string-to-number "0")))))))
;; (add-hook 'outline-minor-mode-hook
;;        (lambda () (local-set-key "\C-c\C-c"
;;                                  outline-mode-prefix-map)))
(global-set-key (kbd "C-<tab>") 'outline-toggle-children)

;; (require 'volatile-highlights)
;; (volatile-highlights-mode t)
;; (eval-after-load "volatile-highlights" '(diminish 'volatile-highlights-mode))

(use-package volatile-highlights
  :disabled t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package recentf
  :config
  (setq recentf-max-saved-items 300
        recentf-max-menu-items 20
        recentf-exclude '(".recentf" "/elpa/" "\\.ido.last" "/ssh:" "/tmp/"
                          "COMMIT_EDITMSG" ".gz")
        recentf-auto-cleanup 600)
  (recentf-mode +1))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode +1))

;; Keep track of window layouts.
;; C-c LEFT and C-c RIGHT
(use-package winner
  :config (winner-mode +1))

;; Anzu shows an indicator inside the minibar when you are searching for things telling you how many
;; matches was found for the current search.
(use-package anzu
  :diminish anzu-mode
  :bind
  (("C-%" . anzu-query-replace-at-cursor)
   ("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "orange" :weight 'bold)
  (setq anzu-minimum-input-length 3)
  ; https://github.com/TheBB/spaceline/issues/130
  ; hide anzu modeline [i.e. (x/X matches)]
  (setq anzu-cons-mode-line-p nil))

;; Per usare =hunspell= bisogna scaricare i dizionari dal sito delle
;; http://extensions.openoffice.org/ extension di OpenOffice che altro non sono file zippati;
;; bisogna estrarre i file =.aff= e =.dic= e copiarli in =~/Library/Spelling=.

;; NOTA: se emacs ti dice:
;; Error enabling Flyspell mode:
;; (error: unknown encoding UTF8: using iso88591 as fallback
;; error: unknown encoding UTF8: using iso88591 as fallback
;; error: unknown encoding UTF8: using iso88591 as fallback
;; error: unknown encoding UTF8: using iso88591 as fallback

;; devi sostituire questa riga:
;; en_US.aff:SET UTF8
;; con "SET UTF-8".

(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

(let ((lt-jar "/Users/dkertesz/Downloads/LanguageTool-4.4/languagetool-commandline.jar"))
  (when (file-exists-p lt-jar)
    (use-package langtool
      :ensure t
      :bind (("C-x 4w" . langtool-check)
             ("C-x 4W" . langtool-check-done)
             ("C-x 4l" . langtool-switch-default-language)
             ("C-x 44" . langtool-show-message-at-point)
             ("C-x 4c" . langtool-correct-buffer))
      :config
      (setq langtool-language-tool-jar lt-jar
            langtool-java-bin "/usr/bin/java"
            ;; this is used to check for false friends; must be your native tongue!
            langtool-mother-tongue "it"
            langtool-disabled-rules '("WHITESPACE_RULE"
                                      "EN_UNPAIRED_BRACKETS"
                                      "COMMA_PARENTHESIS_WHITESPACE"
                                      "EN_QUOTES")))))

;;; hunspell on OS X seems to have problems with flyspell.
;; (if (file-exists-p "/usr/local/bin/hunspell")
;;     (progn
;;       (setq-default ispell-program-name "hunspell"
;;                     ispell-dictionary "en_US"))
;;   (progn (setq-default ispell-program-name "aspell")
;;          (setq ispell-personal-dictionary "~/.flydict"
;;                ispell-extra-args '("--sug-mode=normal" "--ignore=3"))))

; (setq-default ispell-program-name "aspell")
(setq ispell-personal-dictionary (expand-file-name "~/Preferences/emacs/flyspell.dict"))
; (setq ispell-extra-args '("--sug-mode=normal" "--ignore=3")

;; https://github.com/rolandwalker/flyspell-lazy
(use-package flyspell-lazy
  :disabled t
  :config
  (flyspell-lazy-mode +1))

(use-package flyspell
  :commands flyspell-mode
  :config
  (define-key flyspell-mode-map (kbd "M-n") 'flyspell-goto-next-error)
  (define-key flyspell-mode-map (kbd "M-.") 'ispell-word)
  (define-key flyspell-mode-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mode-map [mouse-3] #'undefined))

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g w" . avy-goto-word-1)))

;; Org mode
(use-package org
  ;;; :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c M-p" . org-babel-previous-src-block)
         ("C-c M-n" . org-babel-next-src-block)
         ("C-c S" . org-babel-previous-src-block)
         ("C-c s" . org-babel-next-src-block))
  ;; :hook (org-mode . turn-on-auto-fill)
  :custom-face
  ;; NOTE: to change this, run "customize-face" with parameter "variable-pitch"
  ;; (variable-pitch ((t (:family "ETBembo" :height 1.3))))
  ;; (org-document-title ((t (:foreground "#171717" :weight bold :height 1.5))))
  ;; (org-done ((t (:background "#E8E8E8" :foreground "#0E0E0E" :strike-through t :weight bold))))
  ;; (org-headline-done ((t (:foreground "#171717" :strike-through t))))
  ;; (org-level-1 ((t (:foreground "#090909" :weight bold :height 1.5))))
  ;; (org-level-2 ((t (:foreground "#090909" :weight normal :height 1.4))))
  ;; (org-level-3 ((t (:foreground "#090909" :weight normal :height 1.3))))
  ;; (org-image-actual-width '(600))
  :config
  ;; capture-file
  (setq org-directory "~/Dropbox/org"
        org-default-notes-file (concat org-directory "/notes.org")
        ;; org-todo-keywords '((sequence "TODO" "VERIFY" "|" "DONE" "DELEGATED")))
        ;; add timestamp to closed TODO entries
        org-log-done 'time

        ;; highlight code blocks
        org-src-fontify-natively t

        ;; do not indent at the same level of the header "stars"
        org-adapt-indentation nil

        ;; turn off source blocks default indentation
        org-edit-src-content-indentation 0)

  ;; folding symbol
  (setq org-ellipsis "  "
        org-pretty-entities t)

  ;; more ricing?
  ;; (setq org-startup-indented t
  ;;       ;;; org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
  ;;       org-ellipsis "  " ;; folding symbol
  ;;       org-pretty-entities t
  ;;       org-hide-emphasis-markers t
  ;;       ;; show actually italicized text instead of /italicized text/
  ;;       ;; org-agenda-block-separator ""
  ;;       org-fontify-whole-heading-line t
  ;;       org-fontify-done-headline t
  ;;       org-fontify-quote-and-verse-blocks t)

  ;; I hate subscripts (the small words that you markup with "_" in front or middle of the word).
  (setq org-use-sub-superscripts nil)

  ;; Disable flycheck in org src blocks
  ;; http://emacs.stackexchange.com/questions/16766/how-to-turn-off-emacs-lisp-checkdoc-of-flycheck-when-edit-source-block-in-org
  (defun piger/disable-flycheck-in-org-src-block ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (add-hook 'org-src-mode-hook 'piger/disable-flycheck-in-org-src-block)

  ;; use nicer word wrapping for long lines
  (add-hook 'org-mode-hook (lambda () (setq word-wrap t)))
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

  ;; (add-hook 'org-mode-hook
  ;;           '(lambda ()
  ;;              (setq line-spacing 0.2)
  ;;              (variable-pitch-mode 1)
  ;;              (mapc
  ;;               (lambda (face)
  ;;                 (set-face-attribute face nil :inherit 'fixed-pitch))
  ;;               (list 'org-code
  ;;                     'org-link
  ;;                     'org-block
  ;;                     'org-table
  ;;                     'org-verbatim
  ;;                     'org-block-begin-line
  ;;                     'org-block-end-line
  ;;                     'org-meta-line
  ;;                     'org-document-info-keyword))))

  ;; TODO states
  ;; the first letter is the quick key
  ;; ! means "add timestamp"
  ;; @ means "add timestamp and note"
  ;; f@/! means "add timestamp and note and timestamp when leaving this state"
  (setq org-todo-keywords
        (quote
         ((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w@/!)"
                    "|" "DONE(d!)" "DEFERRED(f@/!)" "CANCELLED(c@)"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "brown1" :weight bold)
                ("INPROGRESS" :foreground "deep sky blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("DEFERRED" :foreground "goldenrod" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)))))

(use-package org-bullets
  :disabled t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

(use-package smart-mode-line
  :disabled t
  :config
  ; this is customized in custom.el
  ; (setq sml/theme 'dark)
  (sml/setup))

(use-package spaceline-config
  :ensure spaceline
  :disabled t
  :config
  (setq powerline-default-separator 'box
        spaceline-window-numbers-unicode t
        spaceline-workspace-numbers-unicode t
        spaceline-flycheck-bullet "❖ %s")
  (spaceline-define-segment venv-el
    "Support for my venv.el"
    (when (and active
               (eq 'python-mode major-mode)
               (bound-and-true-p venv-current-name))
      (propertize venv-current-name
                  'face 'spaceline-python-venv
                  'help-echo (format "Virtual environment via venv.el"))))
  ;;;(spaceline-compile)
  (spaceline-emacs-theme '(venv-el)))

;; is this pinging github all the time??
;; Error running timer ‘doom-modeline--github-fetch-notifications’: (void-function async-inject-variables)
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init))

;; Per selezionare le finestre come su irssi con ALT-1, ALT-2, etc.
;; *NOTE*: this must be configured BEFORE spaceline: https://github.com/TheBB/spaceline/issues/68
(use-package "window-numbering"
  :disabled t
  :ensure t
  :config (window-numbering-mode))


;; replaces the unmaintained "window-numbering"
(use-package winum
  :ensure t
  :init
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-§") 'winum-select-window-by-number)
          (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          map))
  :config
  (winum-mode +1))

;; https://github.com/jscheid/dtrt-indent is a minor mode which guesses the indentation offset of a
;; source file and adjust the corresponding configuration in Emacs.
(use-package dtrt-indent
  :commands drt-indent-mode)

;; which-key whill show a list of possible completion for the key binding typed so far; it's very
;; useful for less used modes (like Org), for example I can press =C-c= and then read the list of
;; org bindings.
(use-package which-key
  :diminish
  :defer 5
  :commands which-key-mode
  :config
  (which-key-mode))

(use-package discover-my-major
  :ensure t
  :bind
  ("C-h C-m" . discover-my-major))

(use-package "dash-at-point"
  :if *is-a-mac*
  :disabled t)

(use-package buffer-move
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right)
  :ensure t)

(use-package unfill
  :commands (unfill-paragraph unfill-region)
  :ensure t)

(use-package whitespace-cleanup-mode
  :ensure t
  :commands whitespace-cleanup-mode
  :diminish 'whitespace-cleanup-mode)

;; misc utilities
(use-package crux
  :ensure t
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key (kbd "C-c o") #'crux-open-with))

(use-package neotree
  :commands neotree-toggle
  :bind ([f8] . neotree-toggle))

(use-package editorconfig
  :ensure t
  :diminish
  :config
  (editorconfig-mode 1))

;; "modern" delete
(use-package nv-delete
  :disabled t
  :bind (("C-<backspace>" . nv-delete-back-all)
         ("M-<backspace>" . nv-delete-back)))

(use-package all-the-icons
  :ensure t
  :config
  (use-package all-the-icons-dired
    :ensure t
    :hook (dired-mode . all-the-icons-dired-mode))

  (use-package all-the-icons-ivy
    :ensure t
    :after ivy
    :config
    (all-the-icons-ivy-setup)))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)))

(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x 1" . switch-window-then-maximize)
         ("C-x 2" . switch-window-then-split-below)
         ("C-x 3" . switch-window-then-split-right)
         ("C-x 4" . switch-window-then-delete))
  :disabled t)

;; Dims the non-active windows. Unfortunately it affects the command window too :(
(use-package dimmer
  :disabled t
  :config
  (setq dimmer-fraction 0.40)
  (dimmer-mode))

(use-package color-identifiers-mode
  :disabled t
  :config
  (global-color-identifiers-mode))

(use-package fireplace
  :commands fireplace)

;; not really that useful
(use-package electric-operator
  :disabled t
  :hook (python-mode . electric-operator-mode))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

(use-package engine-mode
  :ensure t
  :config
  (defengine github-repo
    "https://github.com/search?utf8=✓&type=Repositories&q=%s")
  (defengine github-code
    "https://github.com/search?utf8=✓&type=Code&q=%s")
  (defengine google
    "https://www.google.com/search?client=emacs&q=%s")
  (engine-mode))

;;; HTTP status code package.
;;; NOTE: the command is "hc"
(use-package httpcode
  :commands hc)

(use-package treemacs
  :ensure t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :defer t)

(use-package treemacs-projectile
  :ensure t
  :after treemacs projectile)

;; Aliases
(defalias 'qrr 'query-replace-regexp)

;; load the local settings file
(let ((piger/local-config
       (concat (file-name-as-directory piger/preferences-dir) "init-local.el")))
  (when (file-exists-p piger/local-config)
    (load piger/local-config)))

;;; end
;; reset GC
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;; display loading time
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Settings loaded in %.3fs" elapsed))

;;; to help troubleshooting:
;;; (setq debug-on-error t)
