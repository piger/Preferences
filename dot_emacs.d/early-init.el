;; early-init.el --- Early init file. -*- lexical-binding: t; -*-

;; https://www.reddit.com/r/emacs/comments/1w4ems0/the_emacs_security_settings_that_might_silently/
;; https://www.jamescherti.com/emacs-security-settings/

;; tls.el is deprecated, apparently; in case it's ever used, this should tell it to... check the
;; certificate against trusted root certs. You know, the whole point of using TLS.
(with-eval-after-load "tls" (setq tls-checktrust t))

;; in case GnuTLS is used (?), certificate validation failures are fatal (otherwise I think
;; you'd get a prompt asking to continue)
(setq gnutls-verify-error t)

;; reject DH key exchange primes smaller than 3072 bits. (probably doesn't hurt, but seems odd
;; having to configure this)
(setq gnutls-min-prime-bits 3072)

;; disable ffap (find-file-at-point) network lookups.
;; If the text under point looks like a hostname – say, something.com in a comment – ffap tries to
;; ping it to check if it’s reachable. On a slow or firewalled network, that’s a multi-second hang.
(setq ffap-machine-p-known 'reject)

;; to trigger a manual code review before upgrading a package
;; (setq package-review-policy t)

(provide 'early-init)
