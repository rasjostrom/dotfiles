;;; company-jedi-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "company-jedi" "company-jedi.el" (21818 43001
;;;;;;  227355 382000))
;;; Generated autoloads from company-jedi.el

(autoload 'company-jedi "company-jedi" "\
`company-mode' completion back-end for Python JEDI.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'company-jedi--setup "company-jedi" "\


\(fn)" nil nil)

(setq jedi:setup-function #'company-jedi--setup)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; company-jedi-autoloads.el ends here
