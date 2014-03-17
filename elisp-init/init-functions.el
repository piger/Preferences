;;; miscellaneous functions

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

;; use ido to browse recentf files
;; http://www.xsteve.at/prg/emacs/power-user-tips.html
(defun ido-recentf ()
  "Use ido bla"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
	(find-file
	 (ido-completing-read "Recentf open: "
						  (mapcar (lambda (path)
									(replace-regexp-in-string home "~" path))
								  recentf-list)
						  nil t))))
(global-set-key (kbd "s-r") 'ido-recentf)

;; shutdown emacs server
;; http://www.emacswiki.org/emacs/EmacsAsDaemon
(defun shutdown-server ()
  "Save buffers, Quit and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
)

(provide 'init-functions)
