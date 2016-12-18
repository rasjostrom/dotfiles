(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; -- Installed Packages --
;; Look into: smex, org-mode
(defvar package-list
  '(better-defaults ein  elpy
    flycheck py-autopep8 material-theme
    ace-jump-mode helm pdf-tools
    web-mode multiple-cursors
    markdown-preview-mode rainbow-delimiters
    alect-themes))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      package-list)

;; -- Customizations --
;; Customizations for different modes and contexts are separated and
;; kept in the '~/.emacs.d/lisp' folder.
(add-to-list 'load-path "~/.emacs.d/lisp")
(mapc #'(lambda (lib)
          (require (intern lib)))
      '("misc" "my-general" "my-web"
        "my-python" "my-docs"))
