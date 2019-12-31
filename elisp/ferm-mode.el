;;; ferm-mode.el --- Major mode for editing ferm config files.

;; Copyright (C) 2013  Dmitry Bogatov

;; Author: Dmitry Bogatov <KAction@gnu.org>
;; Keywords: convenience, tools, unix

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This is rought mode for ferm tool. It does basic highlighting and
;; indentation, derived from shell mode.  There is too much matchers, I am
;; going to add them on demand.
;;

;;; Code:
(eval-when-compile (require 'cl))
(defconst ferm-mode-version "1.0"
  "Ferm mode version.")
(defvar ferm-mode-hook nil
  "Hook called when entering `ferm-mode'.")

(defconst ferm-terminal-target-list
  (list "ACCEPT" "DROP" "REJECT" "RETURN" "NOP" "CLASSIFY" "CLUSTERIP" "CONNMARK"
	"CONNSECMARK" "DNAT" "ECN" "HL" "IPV4OPTSSTRIP" "LOG" "MARK" "MASQUERADE"
	"MIRROR" "NETMAP" "NOTRACK" "NFLOG" "NFQUEUE" "QUEUE" "REDIRECT" "SAME"
	"SECMARK" "SET" "SNAT" "TCPMSS" "TOS" "TTL" "ULOG"))
(defconst ferm-module-name-list
  (list "account" "addrtype" "ah" "comment" "condition" "connbytes" "connlimit"
	"connmark" "conntrack" "ecn" "esp" "eui64" "fuzzy" "hbh" "hl" "helper"
	"icmp" "iprange" "ipv4options" "ipv6header" "hashlimit" "length" "limit"
	"mac" "mh" "multiport" "nth" "osf" "owner" "physdev" "pkttype" "policy"
	"psd" "quota" "random" "realm" "recent" "rt" "set" "state" "statistic"
	"string" "tcpmss" "time" "tos" "ttl" "u32" "unclean"))
(defconst ferm-match-keyword-list
  ;; A lot more to add
  (list "interface" "outerface" "protocol" "saddr" "daddr" "fragment" "source" "destination"
	"sport" "dport" "syn" "mac" "state"))

(define-derived-mode ferm-mode shell-script-mode "ferm"
  (font-lock-add-keywords
   nil
   (list
    (cons (rx
	   (or (group "domain" (+ blank)
		      (group-n 2 word-start (or "ip" "ipv6") word-end))
	       (group "table" (+ blank)
		      (group-n 2 word-start
			       (or "filter" "nat" "mangle") word-end))
	       (group "policy" (+ blank)
		      (group-n 2 word-start
			       (or (eval (cons 'or ferm-terminal-target-list))
				   word-end)))
	       (group "mod" (+ blank)
		      (group-n 2 word-start
			       (or (eval (cons 'or ferm-module-name-list)))))
	       (group "chain" (+ blank)
		      (group-n 2 word-start (+ word) word-end))
	       (group "@hook" (+ blank) (or "pre" "post" "flush"))))
	 '(2 font-lock-type-face))
    (cons (rx (eval (cons 'or ferm-terminal-target-list)))
	  'font-lock-preprocessor-face)
    (cons (rx word-start (eval (cons 'or ferm-match-keyword-list)) word-end)
	  'font-lock-builtin-face)
    (cons (rx "&" word-start (+ word) word-end)
	  'font-lock-function-name-face)
    (cons (rx "$" word-start (+ word) word-end)
	  'font-lock-variable-name-face)
    (cons (rx (or bol blank)
	       (or "domain" "table" "chain" "policy" "jump" "mod"
		   "realgoto" "@subchain")
	       word-end)
     'font-lock-keyword-face)
    (cons (rx (or bol blank) "@"
	      (or "def" "include" "hook" "if" "else" "eq" "ne" "not" "resolve"
		  "hook" "cat" "substr" "length" "basename" "dirname" "ipfilter")
		  word-end)
	  'font-lock-constant-face))))
(provide 'ferm-mode)
;;; ferm-mode.el ends here
