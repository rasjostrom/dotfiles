;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Restart the cider REPL
(fset 'rs-cider
   [?\C-u ?k ?c ?i ?d ?e ?r ?- ?r ?e ?p ?l return ?y ?y ?\M-x ?c ?i ?d ?e ?r ?  ?j ?a ?c ?k return ?\C-u ?b return])
