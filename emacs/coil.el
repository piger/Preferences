;;; coil.el --- Coil major mode

;; Copyright (C) 2008 ITA Software

;; Author: dlowe@itasoftware.com
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; (autoload 'coil-mode "coil" nil t)
;; (add-to-list 'auto-mode-alist '("\\.coil\\'"   . coil-mode))
;;

(defvar coil-mode-map
  (let ((map (make-sparse-keymap)))
    ; (define-key map [foo] 'coil-do-foo)
    map)
  "Keymap for `coil-mode'.")

(defvar coil-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    st)
  "Syntax table for `coil-mode'.")

(defvar coil-font-lock-keywords
  '(("#.*" 0 font-lock-comment-face)
    ("@\\(extends\\|file\\|package\\|root\\)" 0 font-lock-keyword-face)
    ("\\(=\\|~\\)" 0 font-lock-builtin-face)
    ("\\(None\\|True\\|False\\)" 0 font-lock-constant-face)
    ("\\([a-zA-Z][a-zA-Z0-9-_.:]*\\)" 1 font-lock-variable-name-face)
    ("[0-9][0-9.]*" 0 font-lock-constant-face))
  "Keyword highlighting specification for `coil-mode'.")

;;;###autoload
(define-derived-mode coil-mode fundamental-mode "Coil"
  "A major mode for editing Coil files."
  :syntax-table coil-mode-syntax-table
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults)
       '(coil-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'coil-indent-line))

;;; Indentation

(defun coil-indent-line ()
  "Indent current line of Coil code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (coil-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun coil-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (save-excursion
    (beginning-of-line)
    (if (< (point) tab-width)
        0
      (skip-chars-forward " \t")
      (let ((indent-above (if (eq (char-syntax (following-char)) ?\) )
                              0
                            tab-width)))
        (backward-up-list)
        (forward-char)
        (skip-chars-forward " \t")
        (if (= (point) (line-end-position))
            (+ (current-indentation) indent-above)
          (current-column))))))


(provide 'coil)
;;; coil.el ends here
