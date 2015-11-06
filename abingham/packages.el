(setq abingham-packages
      '(
        deferred
        grunt
        helm-projectile
        ycmd
        company
        company-ycmd
        paredit
        python
      ))

(setq abingham-excluded-packages '())

;; For each package, define a function abingham/init-<package-name>
;;
(defun abingham/post-init-company ()
  (global-company-mode)
  )

(defun abingham/init-deferred ()
  (use-package deferred :ensure t))

(defun abingham/init-grunt ()
  (use-package grunt
    :ensure t
    :config
    (setq grunt-base-command "/usr/local/bin/node /usr/local/bin/grunt")))

(defun abingham/post-init-helm-projectile ()
  "Initialize stuff"
  (use-package projectile)
  (use-package helm)
  (use-package helm-projectile))


(defun abingham/post-init-ycmd ()
  (set-variable 'ycmd-parse-conditions '(save new-line buffer-focus))
  (set-variable 'ycmd-idle-change-delay 0.1)
  (set-variable 'url-show-status nil)
  (set-variable 'ycmd-request-message-level -1))

(defun abingham/post-init-ycmd ()
  (add-hook 'python-mode-hook 'ycmd-mode)

  (setq emacs-ycmd-root "~/projects/emacs-ycmd")
  (set-variable 'ycmd-server-command '("/usr/local/bin/python2" "-u" "/Users/austinbingham/projects/ycmd/ycmd"))
  (set-variable 'ycmd-extra-conf-whitelist '("~/projects/*" "~/sandbox/*"))
  (set-variable 'ycmd-global-config "~/.emacs.d/ycm_global_conf.py"))

(defun abingham/post-init-company-ycmd ()
  (push 'company-ycmd company-backends-python-mode))

(defun abingham/post-init-paredit ()
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook          #'enable-paredit-mode))


