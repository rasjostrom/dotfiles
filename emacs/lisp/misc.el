(provide 'misc)

(defun misc/cut-path ()
  "Push full path of the current file to kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

(defun misc/revert-buffer-no-prompt ()
  "Reverts buffer without prompting."
  (revert-buffer t t))
