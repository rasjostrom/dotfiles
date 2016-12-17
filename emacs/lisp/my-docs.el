(provide 'my-docs)

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-tools-install))
(add-hook 'pdf-tools-install-hook
          (lambda ()
            (global-set-key (kbd "<f5>") 'misc/revert-buffer-no-prompt)))
