(setq abingham-packages
      '(
        deferred
        grunt
        helm-projectile
        ycmd
        company
        company-ycmd
        python
      ))

(setq abingham-excluded-packages '())

;; For each package, define a function abingham/init-<package-name>
;;
(defun abingham/init-company ()
  (global-company-mode)
  )

(defun abingham/init-deferred ()
  (use-package deferred :ensure t))

(defun abingham/init-grunt ()
  (use-package grunt
    :ensure t
    :config
    (setq grunt-base-command "/usr/local/bin/node /usr/local/bin/grunt")))

(defun abingham/init-helm-projectile ()
  "Initialize stuff"
  (use-package projectile)
  (use-package helm)
  (use-package helm-projectile))

(defun abingham/init-ycmd ()
  (set-variable 'ycmd-parse-conditions '(save new-line buffer-focus))
  (set-variable 'ycmd-idle-change-delay 0.1)
  (set-variable 'url-show-status nil)
  (set-variable 'ycmd-request-message-level -1))

(defun abingham/post-init-ycmd ()
  (add-hook 'python-mode-hook 'ycmd-mode))

(defun abingham/post-init-company-ycmd ()
  (push 'company-ycmd company-backends-python-mode))
