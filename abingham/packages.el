(setq abingham-packages
      '(
        (codesearch :location local)
        deferred
        diff-hl
        elm-mode
        epresent
        feature-mode
        flycheck
        grunt
        helm-projectile
        ycmd
        company
        company-quickhelp
        company-ycmd
        elm-yasnippets
        multiple-cursors
        ox-reveal
        paredit
        python
        live-py-mode
        spaceline
        (therapy :location local)
        uniquify
        python-environment
        pyvenv
        org
        request-deferred
        (traad :location local)
        web-mode
        (wilt :location local)
        vagrant
        vagrant-tramp))

(setq abingham-excluded-packages '(rainbow-delimiters))

;; For each package, define a function abingham/init-<package-name>
;;

(defun abingham/init-vagrant ()
  (use-package vagrant
    :ensure t))

(defun abingham/init-vagrant-tramp ()
  (use-package vagrant-tramp
    :ensure t))

(defun abingham/init-elm-mode ()
  (use-package elm-mode
    :ensure t
    :config
    (progn
      (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
      (add-to-list 'company-backends 'company-elm))))

(defun abingham/init-elm-yasnippets ()
  (use-package elm-yasnippets
    :ensure t))

(defun abingham/init-multiple-cursors ()
  (use-package multiple-cursors
    :ensure t
    :config
    ;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    ))

(defun abingham/post-init-company-quickhelp ()
  (company-quickhelp-mode 1))

(defun abingham/post-init-company ()
  (global-company-mode))

(defun abingham/post-init-spaceline ()
  (spaceline-toggle-hud-off)
  (abingham-mode-line-theme))

(defun abingham/init-feature-mode ()
  (use-package feature-mode :ensure t))

(defun abingham/post-init-flycheck ()
  (add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode)
  (set-variable 'flycheck-display-errors-delay 0.25))

(defun abingham/post-init-web-mode ()
  (message "post-init-web-mode")
  (add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mak$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.pt$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.cshtml$" . web-mode))
  (add-hook 'web-mode-hook (lambda ()
                             (setq web-mode-markup-indent-offset 4)
                             (setq web-mode-code-indent-offset 4))))

(defun abingham/post-init-org ()
  (add-to-list 'org-babel-load-languages '(python . t))
  (org-babel-do-load-languages
   'org-babel-load-languages
   org-babel-load-languages))

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
  (add-hook 'python-mode-hook #'ycmd-mode)
  ;; append this hook so it runs after other flycheck stuff. Seems to be
  ;; necessary...:-/
  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list 'flycheck-disabled-checkers 'ycmd)
              (eldoc-mode 0))
            t)
  (setq python-indent-offset 4)

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              ;; This makes TAB behave sensibly in repls
              (setq tab-width 4 indent-tabs-mode nil)
              ;; nicer repl clearing
              (local-set-key "\C-cl" 'abingham-clear-comint-buffer))))

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

(defun abingham/init-python-environment ()
  (use-package python-environment))

(defun abingham/init-request-deferred ()
  (use-package request-deferred))

(defun abingham/init-traad ()
  (use-package traad
    :bind
    (([(ctrl x) (t) (r)] . traad-rename)
     ([(ctrl x) (t) (u)] . traad-undo)
     ([(ctrl x) (t) (d)] . traad-goto-definition)
     ([(ctrl x) (t) (o)] . traad-display-doc)
     ([(ctrl x) (t) (c)] . traad-display-calltip))
    :init
    (progn
      (require 'traad)
      (set-variable 'traad-server-port 0)
      (set-variable 'traad-server-args '("-V" "2"))
      (add-hook
       'therapy-python3-hooks
       (lambda ()
         (message "traad therapy3 hook")
         (set-variable 'traad-environment-root "traad3")
         (set-variable 'traad-environment-virtualenv traad-py3-environment-virtualenv)))
      (add-hook
       'therapy-python2-hooks
       (lambda ()
         (set-variable 'traad-environment-root "traad2")
         (set-variable 'traad-environment-virtualenv traad-py2-environment-virtualenv))))))

(defun abingham/init-codesearch ()
  "Initialize codesearch"
  (use-package codesearch
    :init
    (require 'codesearch)
   :bind
    (("M-'" . codesearch-search)
     ("M-." . projectile-codesearch-search))))

(defun abingham/init-therapy ()
  (use-package therapy
    :config
    (progn
      (add-hook
       'therapy-python3-hooks
       'abingham-activate-python3)

      (add-hook
       'therapy-python2-hooks
       'abingham-activate-python2))))

(defun abingham/init-wilt ()
  "Initialize wilt."
  (use-package wilt
    :init
    (add-hook 'python-mode-hook 'wilt-mode)
    )
  )
