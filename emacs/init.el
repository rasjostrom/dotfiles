(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(require 'org)
(require 'meghanada)

;; -- Installed Packages --
(defvar package-list
  '(better-defaults org-plus-contrib ein elpy
    flycheck py-autopep8 web-mode
    company-go company-shell company-web
    ace-jump-mode multiple-cursors
    ace-jump-buffer ace-mc ace-isearch
    helm helm-swoop avy magit org-mru-clock
    pdf-tools markdown-preview-mode
    rainbow-delimiters persistent-scratch
    org-bullets go-mode projectile
    which-key spaceline eyebrowse))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      package-list)

;; -- Custom Lisp --
;; Custom lisp helper functions/libraries etc are kept in the
;; '~/.emacs.d/lisp' folder and loaded here.
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/god-mode/")

(mapc #'(lambda (lib)
          (require (intern lib)))
      '("misc" "pretty-outlines"))

;; Mode tweaks, key bindings and general settings are written in
;; org-mode.
(org-babel-load-file
 (expand-file-name "configuration.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-function avy-goto-word-1)
 '(ace-isearch-input-length 3)
 '(ace-isearch-jump-delay 0.25)
 '(avy-highlight-first nil)
 '(avy-keys (quote (97 111 101 117 105 100 104 116 110)))
 '(avy-line-insert-style (quote below))
 '(avy-style (quote at-full))
 '(custom-safe-themes
   (quote
    ("5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "b378249b7f647796b186c70f61eaaee7aa1bd123681d5ca8c44d3ca8875e1b70" "b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" "365d9553de0e0d658af60cff7b8f891ca185a2d7ba3fc6d29aadba69f5194c7f" "6f11ad991da959fa8de046f7f8271b22d3a97ee7b6eca62c81d5a917790a45d9" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(ibuffer-never-show-predicates (quote ("^\\*meghanada" "^\\*helm M-x")) nil (ibuf-ext))
 '(org-agenda-files
   (quote
    ("~/doc/org/sigma-todo.org" "~/doc/org/sigma-daily.org")))
 '(package-selected-packages
   (quote
    (clj-mode org-jira org-journal geben company-lsp flycheck-popup-tip lsp-ui dash-functional paredit use-package macrostep php-eldoc company-php php+-mode org-pomodoro helm-tramp org-mru-clock bash-completion json-mode php-mode yaml-mode golden-ratio auto-dim-other-buffers pabbrev esxml nov spaceline spaceline-all-the-icons gratuitous-dark-theme which-key ace-window all-the-icons-gnus outshine avy-flycheck neotree magit ace-jump-buffer avy ace-mc ace-isearch doom-themes highlight org-alert eyebrowse elfeed elfeed-goodies elfeed-org lua-mode org-plus-contrib hydra groovy-mode helm-projectile projectile helm-swoop meghanada company-go company-jedi company-quickhelp company-shell company-web react-snippets rjsx-mode go-mode flycheck-pyflakes web-mode rainbow-delimiters py-autopep8 persistent-scratch pdf-tools org-bullets org multiple-cursors material-theme markdown-preview-mode helm flycheck elpy ein better-defaults alect-themes ace-jump-mode)))
 '(warning-suppress-types (quote ((\(yasnippet\ backquote-change\)))))
 '(which-function-mode t)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background-face ((t (:foreground "dim gray"))))
 '(avy-lead-face ((t (:background "#51afef" :distant-foreground "black" :foreground "black"))))
 '(avy-lead-face-0 ((t (:underline t :weight extra-bold :width semi-expanded)))))
(put 'narrow-to-region 'disabled nil)
