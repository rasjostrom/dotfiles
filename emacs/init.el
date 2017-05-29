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

;; -- Installed Packages --
(defvar package-list
  '(better-defaults ein elpy
    flycheck py-autopep8 web-mode
    company-go company-shell company-web
    ace-jump-mode helm multiple-cursors
    pdf-tools markdown-preview-mode
    rainbow-delimiters alect-themes
    flx flx-ido flx-isearch
    persistent-scratch org-bullets
    go-mode rjsx-mode react-snippets))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      package-list)

;; -- Customizations --
;; Custom lisp helper functions/libraries etc are kept in the
;; '~/.emacs.d/lisp' folder and loaded here.
(add-to-list 'load-path "~/.emacs.d/lisp")
(mapc #'(lambda (lib)
          (require (intern lib)))
      '("misc" "nm"))

;; Mode tweaks, key bindings and general settings are written in
;; literate programming using org-mode.
(org-babel-load-file
 (expand-file-name
  "configuration.org" user-emacs-directory))
