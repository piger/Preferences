;;; org-mode

; general setup
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
; capture-file
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-todo-keywords
      '((sequence "TODO" "VERIFY" "|" "DONE" "DELEGATED")))
(setq org-tags-alist
      '((sequence "work" "personal" "computer" "blog")))
;; mobile org
(setq org-mobile-directory "~/Dropbox/org/mobile")
(setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))

;; add timestamp to closed TODO entries
(setq org-log-done 'time)

(provide 'init-org)

