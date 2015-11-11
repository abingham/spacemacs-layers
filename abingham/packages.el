(setq abingham-packages
      '(
        deferred
        diff-hl
        epresent
        grunt
        helm-projectile
        ycmd
        company
        company-ycmd
        ox-reveal
        paredit
        python
        live-py-mode
        traad
        uniquify
        pyvenv))

(setq abingham-excluded-packages '())

;; For each package, define a function abingham/init-<package-name>
;;
(defun abingham/post-init-company ()
  (global-company-mode))

(defun abingham/init-live-py-mode ()
  (use-package live-py-mode :ensure t))

(defun abingham/init-epresent ()
  (use-package epresent :ensure t))

(defun abingham/init-ox-reveal ()
  (use-package ox-reveal :ensure t))

(defun abingham/post-init-pyvenv ()
  (add-hook 'pyvenv-post-activate-hooks
            'abingham-pyvenv-hook))

(defun abingham/post-init-diff-hl ()
  (setq diff-hl-side 'left)
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

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

(defun abingham/post-init-python ()
  (add-hook 'python-mode-hook 'ycmd-mode)
  (setq python-indent-offset 4)
  
  ;; This makes TAB behave sensibly in repls
  (add-hook 'inferior-python-mode-hook
            (lambda () (setq tab-width 4 indent-tabs-mode nil))))

(defun abingham/post-init-company-ycmd ()
  (push 'company-ycmd company-backends-python-mode))

(defun abingham/init-paredit ()
  (autoload 'enable-paredit-mode
    "paredit"
    "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook          #'enable-paredit-mode))

(defun abingham/post-init-uniquify ()
  (setq uniquify-buffer-name-style 'forward))

;; TODO: Get therapy in place before enabling this.
;; (defun abingham/init-traad ()
;;   (use-package traad
;;     :bind
;;     (([(ctrl x) (t) (r)] . traad-rename)
;;      ([(ctrl x) (t) (u)] . traad-undo)
;;      ([(ctrl x) (t) (d)] . traad-goto-definition)
;;      ([(ctrl x) (t) (o)] . traad-display-doc)
;;      ([(ctrl x) (t) (c)] . traad-display-calltip))
;;     :init
;;     (progn
;;       (require 'traad)
;;       (set-variable 'traad-server-port 0)
;;       (set-variable 'traad-server-args '("-V" "2"))
;;       (add-hook
;;        'therapy-python3-hooks
;;        (lambda ()
;;          (set-variable 'traad-environment-root "traad3")
;;          (set-variable 'traad-environment-virtualenv '("pyvenv-3.4"))))
;;       (add-hook
;;        'therapy-python2-hooks
;;        (lambda ()
;;          (set-variable 'traad-environment-root "traad")
;;          (set-variable 'traad-environment-virtualenv '("virtualenv")))))
;;     :load-path "~/projects/traad/elisp")
;;   )
