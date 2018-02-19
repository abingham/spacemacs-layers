(setq abingham-packages
      '(
        ;; We want to use ycmd for python and c# completion

        (anaconda-mode :excluded t)
        (company-anaconda :excluded t)
        cc-mode
        (omnisharp :excluded t)
        csharp-mode
        elog
        (emacs-codesearch :location local)
        feature-mode
        flycheck-vale
        company
        company-ycmd
        dired-sidebar
        elm-mode
        elm-yasnippets
        helm-codesearch
        imenu-list
        js2-mode
        markdown-mode
        mmm-mode
        org
        (outline-toc :location local)
        paredit
        python
        py-autopep8
        spaceline
        tox
        (traad :location local)
        virtualenvwrapper
        web-mode
        (wilt :location local)
        ycmd))

(setq abingham-excluded-packages '(rainbow-delimiters))

;; For each package, define a function abingham/init-<package-name>
;;

(defun abingham/init-dired-sidebar ()
  (use-package dired-sidebar
    :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
    :ensure t
    :commands (dired-sidebar-toggle-sidebar)
    :config
    (setq dired-sidebar-subtree-line-prefix " .")
    (setq dired-sidebar-theme 'nerd)
    ;; (cond
    ;;  ((eq system-type 'darwin)
    ;;   (if (display-graphic-p)
    ;;       (setq dired-sidebar-theme 'icons)
    ;;     (setq dired-sidebar-theme 'nerd))
    ;;   (setq dired-sidebar-face '(:family "Helvetica" :height 140)))
    ;;  ((eq system-type 'windows-nt)
    ;;   (setq dired-sidebar-theme 'nerd)
    ;;   (setq dired-sidebar-face '(:family "Lucida Sans Unicode" :height 110)))
    ;;  (:default
    ;;   (setq dired-sidebar-theme 'nerd)
    ;;   (setq dired-sidebar-face '(:family "Arial" :height 140))))

    (setq dired-sidebar-use-term-integration t)
    ;; (setq dired-sidebar-use-custom-font t)

    (use-package all-the-icons-dired
      ;; M-x all-the-icons-install-fonts
      :ensure t
      :commands (all-the-icons-dired-mode))))

(defun abingham/init-imenu-list ()
  (use-package imenu-list))

(defun abingham/init-flycheck-vale ()
  (use-package flycheck-vale
    :ensure t
    :init
    (progn
      (require 'flycheck-vale)
      (flycheck-vale-setup))))

(defun abingham/post-init-cc-mode ()
  (setq-default c-default-style "bsd")
  (setq-default c-basic-offset 4)
  (setq-default tab-width 4)
  (add-hook 'c++-mode-hook 'ycmd-mode))

(defun abingham/post-init-markdown-mode ()
  (add-hook 'markdown-mode-hook
            (lambda ()
              (flyspell-mode 1)))
  (add-hook 'markdown-mode-hook 'abingham-use-outline-for-imenu))

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
  (abingham-mode-line-theme))

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
  (set-variable 'ycmd-request-message-level -1))

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
              (unless (tramp-tramp-file-p (buffer-file-name (current-buffer)))
                (ycmd-mode))))

  ;; (setq-default python-indent-offset 4)
  ;; (add-hook 'inferior-python-mode-hook
  ;;           (lambda ()
  ;;             ;; This makes TAB behave sensibly in repls
  ;;             (setq tab-width 4 indent-tabs-mode nil)
  ;;             ;; nicer repl clearing
  ;;             (local-set-key "\C-cl" 'abingham-clear-comint-buffer)))
  )

(defun abingham/post-init-js2-mode ()
  (add-hook 'js2-mode-hook 'ycmd-mode))

(defun abingham/post-init-csharp-mode ()
  (add-hook 'csharp-mode-hook 'ycmd-mode))

(defun abingham/post-init-company-ycmd ()
  (push 'company-ycmd company-backends-js2-mode)
  (push 'company-ycmd company-backends-python-mode)
  (push 'company-ycmd company-backends-csharp-mode)
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
  (use-package virtualenvwrapper :defer t))

(defun abingham/init-traad ()
  (use-package traad
    :bind
    (([(ctrl x) (t) (r)] . traad-rename)
     ([(ctrl x) (t) (u)] . traad-undo)
     ([(ctrl x) (t) (d)] . traad-goto-definition)
     ([(ctrl x) (t) (o)] . traad-display-doc)
     ([(ctrl x) (t) (c)] . traad-display-calltip))
    :commands traad-open
    :config
    (progn
      (require 'traad)
      (set-variable 'traad-server-port 0)
      (set-variable 'traad-server-args '("-V" "2")))))

(defun abingham/init-elog ()
  "initialize elog"
  (use-package elog :ensure t))

(defun abingham/init-emacs-codesearch ()
  "Initialize codesearch"
  (use-package codesearch
    :bind
    (("M-\"" . codesearch-update-index)))

  (use-package listing-codesearch
    :bind
    (("M-'" . listing-codesearch-search)))

  (use-package projectile-codesearch))

(defun abingham/init-helm-codesearch ()
  "Initialize helm-codesearch"
  (use-package helm-codesearch
    :config
    (progn
      (evil-leader/set-key "hc" 'helm-codesearch-find-pattern))))

(defun abingham/init-wilt ()
  "Initialize wilt."
  (use-package wilt
    :init
    (add-hook 'python-mode-hook 'wilt-mode)
    )
  )

(defun abingham/init-outline-toc ()
  "Initialize outline-toc"
  (use-package outline-toc))
