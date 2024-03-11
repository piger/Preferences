;;; emacs-config.el --- main emacs configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;; NOTE
;; On macOS (12.1) it's better to use the Mituharu mac port packaged by railwaycat: https://github.com/railwaycat/homebrew-emacsmacport
;; brew tap railwaycat/emacsmacport
;; brew install --cask emacs-mac

;; Main configuration file.

;;; Code:

(defconst emacs-start-time (current-time)
  "This variable hold the time Emacs was started.")

;; set the font as early as possible so that in case of configuration issues we're
;; not stuck with the default font. The font can be changed later on in the configuration
;; or in a local settings file.
(set-frame-font "JetBrains Mono 14" nil t)

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
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024) ;; 1mb
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

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Assume "ensure t" for every declared package so that it's installed automatically.
(setq use-package-always-ensure t)

(use-package diminish)

(require 'bind-key)

;; OSX stuff
(when *is-a-mac*
  (use-package exec-path-from-shell
    :ensure t
    :init
    ;; since my shell configuration properly sets up environment variables in zshenv, I shouldn't
    ;; need exec-path-from-shell to invoke a login AND interactive shell (-l -i).
    ;; see also: https://github.com/purcell/exec-path-from-shell/tree/master#setting-up-your-shell-startup-files-correctly
    (setq exec-path-from-shell-arguments nil)
    ;; (setq exec-path-from-shell-debug t)
    (setq exec-path-from-shell-variables
          '("PATH" "MANPATH" "PYTHONPAHT" "GOPATH" "JAVA_HOME"))
    :config
    (exec-path-from-shell-initialize))

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
  (setq delete-by-moving-to-trash t)

  ;; bind cmd-z to undo
  (global-set-key (kbd "s-z") 'undo)

  ;; bind cmd-s to save
  (global-set-key (kbd "s-s") 'save-buffer)

  ;; bind cmd-o to open a file
  (global-set-key (kbd "s-o") 'find-file)

  ;; bind cmd-shift-z to redo (doesn't work!)
  (global-set-key (kbd "S-s-z") 'undo-redo))

;; Themes
(use-package base16-theme
  :disabled
  :config
  ;; (load-theme 'base16-railscasts t)
  ;; (load-theme 'base16-tomorrow-night t)
  ;; (load-theme 'base16-tomorrow t)
  ;; (load-theme 'base16-paraiso t)
  (load-theme 'base16-gruvbox-dark-hard t))

(use-package birds-of-paradise-plus-theme
  :disabled
  :config
  (load-theme 'birds-of-paradise-plus t))

;; Generic settings
;; (some of them coming from from emacs-doom)
(set-language-environment "UTF-8")

;; from emacs-doom: set UTF-8 as default, everywhere.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; uncomment to disable the scrollbar
(when (functionp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'nil))

;; disable the toolbar and the blinking cursor
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; disable bell
(setq ring-bell-function #'ignore)

;; smooth mouse scrolling - 2022-01-01: this is really not necessary.
;; http://www.emacswiki.org/emacs/SmoothScrolling
;; (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; show buffer boundaries in the fringe
(setq-default indicate-buffer-boundaries 'left)

;; show empty lines at the bottom of the buffer, kinda like Vim.
;; (setq-default indicate-empty-lines t)

;; enable context menu mode, which binds the right button to a context aware menu.
(when (display-graphic-p)
  (context-menu-mode))

;; Global auto-revert mode, to allow editing the same file in multiple editors
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; minibuffer history
(use-package savehist
  :init
  (setq savehist-file "~/.emacs.d/savehist")
  :config
  (savehist-mode 1))

;; font
;; (set-frame-font "Mononoki-12")

;; font & cursor (put this in init-local.el)
;; (set-face-attribute 'default nil
;;                     :family "Operator Mono"
;;                     :height 131
;;                     :width 'medium
;;                     :weight 'medium
;;                     :slant 'normal)
;; (set-face-attribute 'cursor nil :background "Orange")

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
    (fringe-mode 12))

;; ask for confirmation before exiting emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;;; transparency
;; (set-frame-parameter (selected-frame) 'alpha '(95 80))
;; (add-to-list 'default-frame-alist '(alpha 95 90))

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
;; 2024-02-19 disabling this because it causes Emacs to freeze when it gets minimized.
;; see also: https://www.reddit.com/r/emacs/comments/197zbtu/how_to_prevent_emacs_freezing_on_macos_seeking/
;; (setq frame-title-format
;;       '("" invocation-name " - " (:eval (if (buffer-file-name)
;;                                             (abbreviate-file-name (buffer-file-name))
;;                                           "%b"))))

;; highlight the current line
(global-hl-line-mode +1)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; disable startup screen
(setq inhibit-startup-screen t)

;; line num
;; (global-linum-mode +1)

;; add column number to the modeline
(use-package simple
  :ensure nil ;; this is a native package
  :config (column-number-mode +1))

;; Replace the active region just by typing text, just like modern editors.
(use-package delsel
  :config (delete-selection-mode +1))

;; SVG tags!
;; NOTE: needs to be enabled via a hook in each mode where you want to use this.
(use-package svg-tag-mode
  :init
  (setq svg-tag-tags
        '((":TODO:" . (svg-tag-make))
          ;; replace "// TODO: " with an SVG tag (useful for code)
          ("// \\(TODO:\\|XXX:\\) " . ((lambda (tag)
                                         (svg-tag-make tag :end -1 :inverse t)))))))

;; nice scrolling - 2022-01-01: not necessary
;; (setq scroll-margin 0
;;       scroll-conservatively 100000
;;       scroll-preserve-screen-position 1)

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
  :ensure nil ;; do not install from package repos, use the builtin version
  :config
  (setq tramp-default-method "ssh")
  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "/etc/ssh/ssh_config")
                                   (tramp-parse-sconfig "~/.ssh/config")
                                   (tramp-parse-hosts "/etc/hosts"))))
;; (setq tramp-default-method "ssh")

;; 08/04/2015 - I don't really like flyspell-mode...
;; (add-hook 'text-mode-hook (lambda () (flyspell-mode +1)))

;; alway start an emacs server
(server-start)

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

;; create the backups directory if it doesn't exists.
(let ((backups-dir (concat user-emacs-directory "backups")))
  (unless (file-exists-p backups-dir)
    (make-directory backups-dir)))

(setq apropos-do-all t
      ;; If non-nil, mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t
      visible-bell t
      load-prefer-newer t
      save-place-file (concat user-emacs-directory "places")
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
      make-backup-files nil
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups")))
      ;; don't clutter the fs with auto-save files (they might be uploaded to chef during knife upload -_-)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; tree-sitter
;; required because some modes are hardcoded into auto-mode-alist and use-package
;; can't override the mappings.
;; (setq major-mode-remap-alist
;;       '((bash-mode . bash-ts-mode)
;;         (json-mode . json-ts-mode)
;;         (python-mode . python-ts-mode)
;;         (go-mode . go-ts-mode)))
;; See notes in the mastering emacs article:
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; also note:
;; You can – should! – use tagged releases where possible. Most of Emacs 29.x is written for
;; grammars released no later than mid 2023. If you use grammars newer than that, you’ll probably
;; run into font locking and indentation problems.
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;; Install by running:
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

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
    "`disable-theme' can unload a theme"
    "<C-:> jumps to a given character on the buffer; useful to avoid using the mouse"
    "<M-s o> is occur which is a nice thing to use, especially with ivy/counsel!"
    "<C-c p s s> runs ag on the projectile project"
    "<C-c p k> to close all the buffers of a project"
    "<C-x j> to switch window layout (transpose-frame)"
    "<C-h l> or view-lossage is the command to know How Did I Get There?"
    "(inf-ruby) is nicer than opening irb in a terminal window"
    "<C-x v b> to open a view a code line in GitHub"
    "cmd-s, cmd-z, cmd-o work as a regular macOS application"
    "You can select a region and run `google' to quickly search for something"
    "The syntax for tramp is /ssh:hostname:/path/to/file"
    "zsh is configured with the `e' alias which opens a file with emacsclient"
    "<TAB> is for completion and <M-/> is for expansion"
    "You can use the `hc' command to look up HTTP status codes"
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
  :if *is-a-mac*
  :config
  (setq simpleclip-unmark-on-copy t)
  (simpleclip-mode +1))

;; use hippie-expand instead of dabbrev
;; see also: https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

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

;; Programming
(use-package subword
  :commands subword-mode
  :diminish subword-mode)

(use-package abbrev
  :ensure nil ;; this is a native package
  :commands abbrev-mode
  :diminish abbrev-mode)

;; Assign a specific mode for certain directories
;; note: you can't chain multiple paths in a single add-to-list call :(
(add-to-list 'auto-mode-alist '("/Preferences/zsh/" . shell-script-mode))

;; See also: http://www.flycheck.org/en/latest/languages.html#flycheck-languages
(use-package flycheck
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :config
  ;; spaceline provides his own indicator for that
  (setq-default flycheck-mode-line nil)
  (setq flycheck-indication-mode 'right-fringe)

  ;; replace flycheck's wavy underline with a straight line
  (set-face-attribute 'flycheck-error nil :underline '(:color "#d32e00" :style line :position -3))
  (set-face-attribute 'flycheck-warning nil :underline '(:color "#e3795c" :style line :position -3))
  (set-face-attribute 'flycheck-info nil :underline '(:color "ForestGreen" :style line :position -3))

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

  (global-flycheck-mode))

;; direnv
;; https://github.com/wbolster/emacs-direnv
(use-package direnv
  :config
  (direnv-mode))

;; eldoc and eldoc-box
;; https://github.com/casouri/eldoc-box
(use-package eldoc
  :ensure nil ;; use the bundled version!
  :bind (("C-c h" . eldoc))
  :init
  (global-eldoc-mode)) ;; start it via :init so it won't be deferred

;; should try eldoc-box-hover-mode
(use-package eldoc-box)

(use-package ediff
  :commands ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  ; ignore all white spaces
  (setq ediff-diff-options "-w"))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook (python-mode . my-python-mode-hook)
  :config
  (defun my-python-mode-hook ()
    (subword-mode +1)
    (show-paren-mode +1)
    (flycheck-mode +1)
    ;; (company-mode +1)
    (eldoc-mode +1)
    ;; (elpy-enable)
    ;; unfuck electric indentation
    (setq electric-indent-chars '(?\n))))

;; go
;; requires a bunch of tools:
;; go install golang.org/x/tools/gopls@latest
;; go install golang.org/x/tools/cmd/godoc@latest
;; go install golang.org/x/tools/cmd/goimports@latest
;; go install golang.org/x/tools/cmd/gorename@latest
;; go install golang.org/x/tools/cmd/gomvpkg@latest
(use-package go-mode
  :mode "\\.go\\'"
  ;; this binding exists by default
  ;; :bind ("M-." . godef-jump)
  :config
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook #'lsp-organize-imports -20 t)
    (add-hook 'before-save-hook #'lsp-format-buffer -10 t)
    ;; (add-hook 'before-save-hook #'gofmt-before-save)
    ;; add hook to run gofmt before save; add it with priority -10 (ie. earlier than others)
    ;; and as buffer-local (as opposed to global, which would run it for *every* buffer).
    ;; (add-hook 'before-save-hook #'eglot-interactively-organize-imports -20 t)
    ;; (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    ;; (setq gofmt-command "goimports")
    ;; (with-eval-after-load 'company
    ;;   '(add-to-list 'company-backends 'company-go))
    (if (not (string-match "go" compile-command))
             (set (make-local-variable 'compile-command)
                  "go build -v && go test -v && go vet"))
    (setq tab-width 4)
    (subword-mode +1)
    ;; (company-mode)
    (flycheck-mode)
    (svg-tag-mode t)
    (diminish 'subword-mode))
  :hook (go-mode . my-go-mode-hook))

(use-package go-ts-mode
  :config
  (defun my-go-ts-mode-hook()
    (setq tab-width 4)
    (setq go-ts-mode-indent-offset 4))
  :hook (go-ts-mode . my-go-ts-mode-hook))

(use-package rust-mode
  :mode "\\.rs\\'")

;; inf-ruby provides a REPL buffer connected to a Ruby subprocess.
(use-package inf-ruby
  :init
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  :bind
  (("C-c r r" . inf-ruby)))

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

;; to be enabled if I have to deal with Ruby.
;; requires: brew install rbenv
(use-package rbenv
  :init
  :disabled
  (setq rbenv-executable "/usr/local/bin/rbenv")
  (setq rbenv-modeline-function 'rbenv--modeline-plain)
  (setq rbenv-show-active-ruby-in-modeline nil)
  :config
  (global-rbenv-mode)
  (rbenv-use-global))

(use-package css-mode
  :mode "\\.css\\'"
  :custom
  (css-indent-offset 2)
  :hook ((css-mode . rainbow-mode)
         (css-mode . subword-mode)))

(use-package rainbow-mode
  :init
  (rainbow-mode))

(use-package less-css-mode
  :mode "\\.less\\'")

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :disabled
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
  :mode "\\.js\\'"
  :interpreter "node"
  :hook (rjsx-mode . subword-mode))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (defun my-json-mode-hook ()
    (flycheck-mode +1))
  :hook (json-mode . my-json-mode-hook))

(use-package web-mode
  :mode ("\\.erb\\'"
         "\\.hbs\\'"
         "\\.html?\\'"
         "\\.tmpl\\'"
         "\\.j2\\'")
  :init
  (setq web-mode-engines-alist
        '(("go" . "/go/src/.*\\.html\\'")
          ("go" . "/nginx-templates/default\\.conf\\.tmpl\\'")
          ("django" . "/dev/.*/templates/.*\\.html\\'")))
  (setq web-mode-enable-auto-indentation nil)
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

;; use this by calling "c-set-style"
(defconst piger-cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "piger-cc-style" piger-cc-style)

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :init
  (setq highlight-symbol-idle-delay 0.5)
  :hook (prog-mode . highlight-symbol-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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

(use-package jsonnet-mode
  :mode "\\.jsonnet\\'")

;; Highlight indentation with coloured bars.
;; this is nice but I think it's very unmaintained and possibly quite broken.
(use-package highlight-indent-guides
  :disabled
  :init
  (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-responsive 'top)
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . bug-reference-mode))
  ;; :config
  ;; (electric-pair-mode))

;; https://oylenshpeegul.gitlab.io/blog/posts/20230129/
;; Add a contextual menu to launch 'git-link' by right-clicking on a line.
(defun piger/git-link-context-menu (menu click)
  "Context menu for git-link."
  (define-key-after menu [separator-git-link] menu-bar-separator)
  (define-key-after menu [git-link] '(menu-item "git link" git-link :help "Link to GitHub"))
  menu)
(add-hook 'context-menu-functions #'piger/git-link-context-menu)

;; Other modes
;; ---------------------------------------------------------------------------------------
(ido-mode -1)

(use-package ivy
  :pin melpa
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

;; fancy descriptions in M-x
(use-package ivy-rich
  :after (ivy counsel)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package counsel
  :pin melpa
  :after ivy
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-h f"   . counsel-describe-function)
   ("C-h v"   . counsel-describe-variable)
   ("C-c k"   . counsel-rg)
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
;; NOTE: ivy reuses smex (or alternatives like amx) automatically if they are installed;
;; https://oremacs.com/swiper/#packages
(use-package smex
  ;; :bind (("M-x" . smex)
  ;;        ("M-X" . smex-major-mode-commands)
  ;;        ("C-c C-c M-x" . execute-extended-command))
  :config
  (smex-initialize))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; meaningful names for buffers with the same name
(use-package uniquify
  :ensure nil ;; this is a native package
  :demand t
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
  :bind (("C-x j" . transpose-frame)))

(use-package magit-popup
  :defer t)

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :config
  (setq magit-diff-refine-hunk 'all)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

;; magit's mode for editing git commits.
(use-package git-commit
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :config
  ;; set the subject line to a maximum of 70 columns.
  (setq git-commit-summary-max-length 70)
  ;; also reset fill-column to be 70 columns for the body of the commit message.
  (add-hook 'git-commit-mode-hook (lambda () (setq fill-column 70))))

;; major modes for git-related files. https://github.com/magit/git-modes
(use-package git-modes)

(use-package forge
  :after magit
  :config
  (setq forge-pull-notifications nil))

(use-package gist
  :defer t)

(use-package git-timemachine
  :bind (("C-x v t" . git-timemachine)))

(use-package git-link
  :bind (("C-x v b" . git-link))
  :config
  (setq git-link-open-in-browser t))

(use-package dired
  :ensure nil ;; this is a native package
  :bind ("C-x d" . dired)
  :hook (dired-mode . hl-line-mode))

(use-package dired-x
  :ensure nil ;; this is a native package
  :after dired)

;; This is useful to mark /things/ inside markers, for example the text inside a quoted string or
;; inside some parenthesis.
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; (require 'move-text)
;; i binding di default sono alt+up e alt+down, gli stessi che uso
;; per switchare finestra.
;; (move-text-default-bindings)

(use-package evil
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
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "markdown")
  ;; (setq markdown-fontify-code-blocks-natively t)
  ;; (set-face-attribute 'markdown-pre-face nil :inherit 'markdown-markup-face)
  ;; (custom-set-faces
  ;;  '(markdown-pre-face ((t (:inherit markdown-markup-face)))))
  (add-hook 'markdown-mode-hook 'turn-on-auto-fill))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook (yaml-mode . flycheck-mode))

(use-package toml-mode
  :mode "\\.toml\\'")

;; brew install hashicorp/tap/terraform-ls
;; NOTE: not sure if it's a problem with eglot or terraform-ls, but eldoc gives
;; pretty much NOTHING useful.
(use-package terraform-mode
  :mode "\\.tf\\'")
  ;; :hook (terraform-mode . eglot-ensure))

(use-package company-terraform
  :after (terraform-mode company)
  :disabled
  :config
  (company-terraform-init))

(use-package hcl-mode
  :mode "\\.hcl\\'")

(use-package nginx-mode
  :commands nginx-mode)

(use-package ssh-config-mode
  :mode "\\.ssh/config\\'"
  :config
  (setq ssh-config-mode-indent 4))

(use-package systemd
  :mode ("\\.service\\'" . systemd-mode))

;; gettext on OS X (homebrew) ships with additional elisp files
(use-package po-mode
  :disabled
  :if (file-exists-p "/usr/local/opt/gettext/share/emacs/site-lisp")
  :load-path "/usr/local/opt/gettext/share/emacs/site-lisp"
  :mode "\\.po\\'\\|\\.po\\.")

(defun piger/prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (rainbow-delimiters-mode t)
  ;; (company-mode t)
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
  :init
  (setq projectile-project-search-path '("~/code/"))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (add-to-list 'projectile-globally-ignored-files ".pyc")
  (add-to-list 'projectile-globally-ignored-files "__pycache__"))

;; required by the command projectile-ripgrep
(use-package rg)

;; 2021-12-29 - disabling this because it has been extremely slow for the past year or more.
(use-package counsel-projectile
  :disabled
  :after (counsel projectile)
  :pin melpa
  :config
  (counsel-projectile-mode 1))

(use-package company
  :diminish
  :hook (prog-mode . company-mode)
  :custom
  ;; Is this useful?
  ;; (setq company-transformers '(company-sort-by-occurrence)))
  ;; Search other buffers with the *same major/minor mode* for completion instead of
  ;; searching all other buffers.
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t))

(use-package company-quickhelp
  :disabled
  :config
  (company-quickhelp-mode 1))

(use-package company-go
  :after (company go)
  :disabled)

(use-package company-web
  :after (company web)
  :disabled)

;; Goggles highlights the modified region using pulse. Currently the commands undo, yank, kill and
;; delete are supported.
;; https://github.com/minad/goggles
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

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

(use-package recentf
  :config
  (setq recentf-max-saved-items 300
        recentf-max-menu-items 20
        recentf-exclude '(".recentf" "/elpa/" "\\.ido.last" "/ssh:" "/tmp/"
                          "COMMIT_EDITMSG" ".gz")
        recentf-auto-cleanup 600)
  (recentf-mode +1))

(use-package undo-tree
  :disabled
  :diminish undo-tree-mode
  :config
  ;;; until I figure out if I want undo-history I don't want this damn backup files
  ;;; scattered EVERYWHERE.
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode +1))

;; Keep track of window layouts.
;; C-c LEFT and C-c RIGHT
(use-package winner
  :config
  (winner-mode +1))

;; install hunspell with brew, then download the dictionaries:
;; curl --output-dir ~/Library/Spelling -O https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff
;; curl --output-dir ~/Library/Spelling -O https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic
(when (executable-find "hunspell")
  (setq ispell-program-name "hunspell"))

;; tell hunspell where to find the dictionaries.
;; see: https://passingcuriosity.com/2017/emacs-hunspell-and-dictionaries/
(setenv
  "DICPATH"
  (concat (getenv "HOME") "/Library/Spelling"))

;; set en_US for spell checking.
(setenv "LANG" "en_US.UTF-8")

(setq ispell-personal-dictionary (expand-file-name "~/Preferences/emacs/flyspell.dict"))

(use-package flyspell
  :commands flyspell-mode
  :config
  (set-face-attribute 'flyspell-duplicate nil :underline '(:color "#d79921" :style line :position -3))
  (set-face-attribute 'flyspell-incorrect nil :underline '(:color "#951b9e" :style line :position -3))
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
  (setq org-ellipsis "⤵"
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

  ;; custom link example
  ;; (defun org-open-jira-link (ticket-id)
  ;;   "Open the Jira ticket TICKET-ID in the browser."
  ;;   (browse-url (concat "https://jira.atlassian.net/browse/" ticket-id)))

  ;; (org-link-set-parameters "jira"
  ;;                          :follow #'org-open-jira-link)

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

;; newer org versions implemented C-c C-, to insert templates;
;; for the old behaviour (<s <e etc.) you need to load org-tempo.
;; ref: https://emacs.stackexchange.com/questions/46988/why-do-easy-templates-e-g-s-tab-in-org-9-2-not-work
(use-package org-tempo
  :ensure nil ;; this comes with org?
  :after (org))

(use-package doom-modeline
  :config
  ;; Install a Nerd Font font from https://www.nerdfonts.com/ to use it in the modeline
  ;; (no need to set it as a global font!)
  (setq doom-modeline-unicode-fallback t)
  (setq nerd-icons-font-family "JetBrainsMono Nerd Font")
  ;; Increase the size of the modeline
  (setq doom-modeline-height 30)
  :init
  (doom-modeline-mode 1))

;; replaces the unmaintained "window-numbering"
(use-package winum
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
;; NOTE: this will conflict with projects that use editorconfig; it would be nice to disable this
;; package in those instances.
;; See also:
;; - https://github.com/doomemacs/doomemacs/blob/master/modules/tools/editorconfig/config.el
;; - https://github.com/jscheid/dtrt-indent/issues/53
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
  :bind
  ("C-h C-m" . discover-my-major))

(use-package buffer-move
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right))

(use-package unfill
  :commands (unfill-paragraph unfill-region))

(use-package whitespace-cleanup-mode
  :commands whitespace-cleanup-mode
  :diminish 'whitespace-cleanup-mode)

;; misc utilities
(use-package crux
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key (kbd "C-c o") #'crux-open-with))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

;; NOTE: remember to install the fonts! Run: all-the-icons-install-fonts
(use-package all-the-icons
  :disabled
  :config
  (use-package all-the-icons-dired
    :ensure t
    :hook (dired-mode . all-the-icons-dired-mode))

  (use-package all-the-icons-ivy
    :disabled
    :after ivy
    :config
    (all-the-icons-ivy-setup)))

(use-package helpful
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
  :disabled)

(use-package engine-mode
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
  :load-path "~/dev/httpcode.el"
  :commands hc
  :config
  ;; https://support.cloudflare.com/hc/en-us/articles/115003011431/
  (setq custom-http-codes
        '(
          (499 ("Client Closed Request" "A non-standard status code introduced by nginx for the case when a client closes the connection while nginx is processing the request."))
          (520 ("Web server returns an unknown error" "Occurs when the origin server returns an empty, unknown, or unexpected response to Cloudflare."))
          (521 ("Web server is down" "Occurs when the origin web server refuses connections from Cloudflare. Security solutions at your origin may block legitimate connections from certain Cloudflare IP addresses."))
          (522 ("Connection timed out" "Occurs when Cloudflare times out contacting the origin web server."))
          (523 ("Origin is unreachable" "Occurs when Cloudflare cannot contact your origin web server."))
          (524 ("A timeout occurred" "Indicates that Cloudflare successfully connected to the origin web server, but the origin did not provide an HTTP response before the default 100 second connection timed out."))
          (525 ("SSL handshake failed" "525 errors are often caused by a configuration issue on the origin web server."))
          (526 ("Invalid SSL certificate" "Error 526 occurs when these two conditions are true: 1) Cloudflare cannot validate the SSL certificate at your origin web server, and 2) Full SSL (Strict) SSL is set in the Overview tab of your Cloudflare SSL/TLS app."))
          (527 ("Railgun Listener to origin error" "A 527 error indicates an interrupted connection between Cloudflare and your origin's Railgun server (rg-listener)."))
          (530 ("1XXX Error" "HTTP error 530 is returned with an accompanying 1XXX error displayed.")))))

(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua")

(use-package dockerfile-mode
  :mode ("Dockerfile" . dockerfile-mode))

;;; trim whitespaces only on edited lines
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; re-builder can be useful
;; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
;; C-c C-w copies the regex in the clipboard, in emacs syntax (re-introducing double escaping).
;; C-c C-q exits from re-builder
(use-package re-builder
  :commands re-builder
  :config
  ;; set the syntax to the normal syntax (i.e. you don't need double escaping)
  (setq reb-re-syntax 'string))

;; https://github.com/magnars/multiple-cursors.el
;; See also:
;; - mc/edit-beginnings-of-lines
;; - mc/edit-ends-of-lines
;; - mc/mark-next-like-this, mc/mark-previous-like-this, mc/mark-all-like-this
(use-package multiple-cursors
  :bind ("C-S-c C-S-c" . mc/edit-lines))

;; spell checking
;; NOTE:
;; brew install enchant pkg-config
;; NOTE:
;; it just crash on emacs 28.2 on macOS 13.3.1
;; railwaycat/emacsmacport/emacs-mac: stable emacs-28.2-mac-9.1
;; see also: https://github.com/minad/jinx/issues/48
(use-package jinx
  :disabled
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

;; eglot
;; An alternative to lsp-mode, now an emacs builtin.
(use-package eglot
  :disabled
  :config
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
  ;; don't log every event (from emacs-bedrock)
  (fset #'jsonrpc--log-event #'ignore))
  ;; (defun eglot-interactively-organize-imports ()
  ;;   (call-interactively 'eglot-code-action-organize-imports))
  ;; Python: install pyright, or search for another language server.
  ;; :hook (python-mode . eglot-ensure))

;; lsp
(use-package lsp-mode
  :disabled
  :init
  ;; when calling eldoc, render all the documentation instead of just the signature.
  (setq lsp-eldoc-render-all t)
  ;; disable highlighting the symbol; in Go functions this highlight the whole function body
  (setq lsp-enable-symbol-highlighting nil)
  :hook
  ((go-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package tab-bar
  :bind (("s-{" . tab-bar-switch-to-prev-tab)
         ("s-}" . tab-bar-switch-to-next-tab)
         ("s-t" . tab-bar-new-tab)
         ("s-w" . tab-bar-close-tab)))

(use-package difftastic
  :demand t
  :bind (:map magit-blame-read-only-mode-map
         ("D" . difftastic-magit-show)
         ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (add-to-list 'treemacs-litter-directories "/dsfjsodfjsdoifjds/")
  (treemacs-project-follow-mode t)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

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
(setq debug-on-error nil)

;; shake fist!!
(set-frame-font "JetBrains Mono 14" nil t)

;; bug reference mode
;; Local Variables:
;; bug-reference-bug-regexp: "\\([Bb]ug[#-]\\([0-9]+\\)\\)"
;; bug-reference-url-format: "https://project.org/issues/%s"
;; End:

(provide 'emacs-config)
;;; emacs-config.el ends here.
