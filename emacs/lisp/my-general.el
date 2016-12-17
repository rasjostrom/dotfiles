(provide 'my-general)

;; -- General Settings --
(setq inhibit-startup-message t)
(setq column-number-mode t)
(blink-cursor-mode 0)
(setq tab-width 2)
(display-battery-mode t)

;; Set theme
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/themes"))
(load-theme 'alect-black t)

;; Enable tab autocomplete for elisp
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; -- Modes & bindings --
;; Swap CTRL-X with CTRL-U
(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])

;;(global-set-key (kbd "C-i") #'delete-backward-char)
(global-set-key (kbd "C-<tab>") #'next-multiframe-window)

;; ace-jump-mode
(global-set-key (kbd "C-x SPC") #'ace-jump-mode)
(global-set-key (kbd "C-x C-SPC") #'ace-jump-mode-pop-mark)

;; helm-mode
(helm-mode 1)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)

