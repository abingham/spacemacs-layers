(setq abingham-packages
      '(
        (anaconda-mode :excluded t)
        cc-mode
        (company-anaconda :excluded t)
        ;; company-lsp
        codesearch
        company
        company-ycmd
        counsel-codesearch
        ;; csharp-mode
        elog
        feature-mode
        flycheck-vale
        elm-mode
        elm-yasnippets
        ;; helm-codesearch
        imenu-list

        ;; importmagic relies on epc and impormagic being installed, which
        ;; they're generally not. So I'm getting rid of it for now.
        (importmagic :excluded t)

        js2-mode
        lsp-ui
        mmm-mode
        (omnisharp :excluded t)
        org
        (outline-toc :location local)
        paredit
        py-autopep8
        python
        spaceline
        tox
        (traad :location local)
        virtualenvwrapper
        web-mode
        (wilt :location local)
        window-purpose
        ycmd
        ))

(setq abingham-excluded-packages '(rainbow-delimiters))

;; For each package, define a function abingham/init-<package-name>
;;

(defun abingham/post-init-window-purpose ()
  (use-package window-purpose
    :config
    (progn
      (purpose-mode)
      ;; (add-to-list 'purpose-user-mode-purposes '(prog-mode . prog))
      (add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . util))
      (add-to-list 'purpose-user-mode-purposes '(magit-mode . util))
      (add-to-list 'purpose-user-mode-purposes '(git-commit-mode . util))
      (add-to-list 'purpose-user-mode-purposes '(compilation-mode . util))
      (add-to-list 'purpose-user-mode-purposes '(paradox-menu-mode . util))
      (add-to-list 'purpose-user-mode-purposes '(help-mode . util))
      (purpose-compile-user-configuration))))

(defun abingham/post-init-lsp-ui ()
  (use-package lsp-ui
    :ensure t
    :init
    (progn
      (setq lsp-ui-sideline-enable nil)
      (setq lsp-ui-doc-mode t)
      (add-hook 'lsp-mode-hook 'lsp-ui-mode))))

(defun abingham/init-company-lsp ()
  (use-package company-lsp
    :ensure t
    :config (push 'company-lsp company-backends)))

(defun abingham/init-imenu-list ()
  (use-package imenu-list))

(defun abingham/init-flycheck-vale ()
  (use-package flycheck-vale
    :ensure t
    :init
    (progn
      (flycheck-vale-setup))))

(defun abingham/post-init-cc-mode ()
  (setq-default c-default-style "bsd")
  (setq-default c-basic-offset 4)
  (setq-default tab-width 4)
  (add-hook 'c++-mode-hook 'ycmd-mode))

(defun abingham/post-init-elm-mode ()
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-elm)))

(defun abingham/post-init-mmm-mode ()
  (setq mmm-global-mode 'maybe)
  (mmm-add-classes
   '((markua-python
      :submode python-mode
      ;; :front "^{.*lang\\(uage\\)?=\"?python\"?.*}[\r\n]+~\\{8,\\}",
      :front "^{language=[python|pycon].*}[\r\n]+~\\{8,\\}[\r\n]"
      :back "^~\\{8,\\}$")))
  (mmm-add-mode-ext-class 'markdown-mode nil 'markua-python))

(defun abingham/init-elm-yasnippets ()
  (use-package elm-yasnippets
    :ensure t))

(defun abingham/post-init-company ()
  (global-company-mode))

(defun abingham/post-init-spaceline ()
  (spaceline-toggle-hud-off)

  ;; We put wilt as a spaceline segment rather than a minor-mode highlighter.
  ;; It seems a bit cleaner that way.
  (spaceline-define-segment abingham-wilt
    "Spaceline segment for showing the WILT of the current buffer"
    (when (bound-and-true-p wilt-mode)
      (format "WILT %.2f" wilt-current)))

  (spaceline-spacemacs-theme 'abingham-wilt)
  (setq powerline-default-separator nil)

  ;; Some stuff we don't want all the time
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-buffer-encoding-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-minor-modes-off))

(defun abingham/init-tox ()
  (use-package tox :ensure t))

(defun abingham/init-feature-mode ()
  (use-package feature-mode :ensure t))

(defun abingham/post-init-web-mode ()
  (add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mak$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.pt$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.cshtml$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
  (add-hook 'web-mode-hook (lambda ()
                             (setq web-mode-markup-indent-offset 4)
                             (setq web-mode-code-indent-offset 4))))

(defun abingham/post-init-org ()
  (setq org-todo-keywords (list "TODO" "IN-PROGRESS" "IMPEDED" "DONE"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

(defun abingham/post-init-ycmd ()
  ;; (setq ycmd-force-semantic-completion t)
  (set-variable 'ycmd-parse-conditions '(save new-line buffer-focus))
  (set-variable 'ycmd-idle-change-delay 0.1)
  (set-variable 'url-show-status nil)
  (set-variable 'ycmd-request-message-level -1)

  ;; these are correct for my normal emacs installations, but are obviously
  ;; somewhat installation-dependent. If you need to set these to something
  ;; different for your machine, change them in `dotspacemacs/user-config'.
  (set-variable 'ycmd-extra-conf-whitelist '("~/repos/*" "~/sandbox/*"))
  (set-variable 'ycmd-server-command
                (list (expand-file-name "~/.virtualenvs/ycmd/bin/python") "-u"
                      (expand-file-name "~/repos/ycmd/ycmd"))))

(defun abingham/init-py-autopep8 ()
  (use-package py-autopep8
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "cf" 'py-autopep8-buffer)))

(defun abingham/post-init-python ()
  (add-hook 'python-mode-hook
            (lambda ()
              ;; I was seeing tramp slowdowns when ycmd was active...
              ;; (unless (tramp-tramp-file-p (buffer-file-name (current-buffer)))
              ;;   (ycmd-mode))
              )))

(defun abingham/post-init-js2-mode ()
  (add-hook 'js2-mode-hook 'ycmd-mode))

(defun abingham/post-init-csharp-mode ()
  (add-hook 'csharp-mode-hook 'ycmd-mode))

(defun abingham/post-init-company-ycmd ()
  ;; (push 'company-ycmd company-backends-js2-mode)
  ;; (push 'company-ycmd company-backends-python-mode)
  ;; (push 'company-ycmd company-backends-csharp-mode)
  )


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
  (add-hook 'clojure-mode-hook          #'enable-paredit-mode)
  (global-set-key [(alt right)] 'paredit-forward-slurp-sexp)
  (global-set-key [(alt left)] 'paredit-backward-slurp-sexp)
  (global-set-key [(alt meta right)] 'paredit-forward-barf-sexp)
  (global-set-key [(alt meta left)] 'paredit-backward-barf-sexp))

(defun abingham/init-virtualenvwrapper ()
  (use-package virtualenvwrapper))

(defun abingham/init-traad ()
  (use-package traad
    :bind
    (([(ctrl x) (t) (r)] . traad-rename)
     ([(ctrl x) (t) (u)] . traad-undo)
     ([(ctrl x) (t) (d)] . traad-goto-definition)
     ([(ctrl x) (t) (o)] . traad-display-doc)
     ([(ctrl x) (t) (c)] . traad-display-calltip))
    :config
    (progn
      (set-variable 'traad-server-port 0)
      (set-variable 'traad-server-args '("-V" "2")))))

(defun abingham/init-elog ()
  "initialize elog"
  (use-package elog :ensure t))

(defun abingham/init-codesearch ()
  "Initialize codesearch"
  (use-package codesearch
    :bind
    (("M-\"" . codesearch-update-index)))

  (use-package listing-codesearch
    :bind
    (("M-'" . listing-codesearch-search)))


  ;; These work for my normal installation, but you can set them to something
  ;; else in `dotspacemacs/user-config' if needed.
  (set-variable 'codesearch-cindex (expand-file-name "~/go/bin/cindex"))
  (set-variable 'codesearch-csearch (expand-file-name "~/go/bin/csearch")))

(defun abingham/init-projectile-codesearch ()
  (use-package projectile-codesearch))

(defun abingham/init-counsel-codesearch ()
  "Initialize counsel codesearch"
  (use-package counsel-codesearch))

(defun abingham/init-helm-codesearch ()
  "Initialize helm-codesearch"
  (use-package helm-codesearch
    :config
    (progn
      (evil-leader/set-key "hc" 'helm-codesearch-find-pattern))))

(defun abingham/init-wilt ()
  "Initialize wilt."
  (use-package wilt
    :config
    ;; Prevent wilt-mode displaying a highligher. We use a spaceline segment instead.
    (setq wilt-mode-line-template "")

    :init
    (add-hook 'prog-mode-hook 'wilt-mode)))

(defun abingham/init-outline-toc ()
  "Initialize outline-toc"
  (use-package outline-toc))
