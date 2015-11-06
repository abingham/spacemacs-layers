;; Ctrl-L to top of screen
;; wilt
;; ycmd from dev
;; traad
;; 



(setq emacs-ycmd-root "~/projects/emacs-ycmd")
(set-variable 'ycmd-server-command '("/usr/local/bin/python2" "-u" "/Users/austinbingham/projects/ycmd/ycmd"))
(set-variable 'ycmd-extra-conf-whitelist '("~/projects/*" "~/sandbox/*"))
(set-variable 'ycmd-global-config "~/.emacs.d/ycm_global_conf.py")


