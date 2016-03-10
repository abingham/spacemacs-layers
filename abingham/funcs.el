(setq abingham-root-dir (file-name-directory (or (buffer-file-name)
                                                 load-file-name)))

(defun abingham-load-snippets ()
  (let ((snip-dir (expand-file-name "snippets" abingham-root-dir)))
    (add-to-list 'yas-snippet-dirs snip-dir t)
    (yas/load-directory snip-dir)))

(eval-after-load 'yasnippet
  '(abingham-load-snippets))

(defun abingham-clear-comint-buffer ()
  "Improved repl/comint clearing procedure."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun helm-my-buffers ()
  (interactive)
  (let* ((sources '(helm-source-projectile-projects
                    helm-source-buffers-list
                    helm-source-files-in-current-dir
                    helm-source-recentf
                    helm-source-buffer-not-found))
         (sources (if (projectile-project-p)
                      (append '(helm-source-projectile-files-list
                                helm-source-projectile-buffers-list)
                              sources)
                    sources)))
    (helm-other-buffer sources "*helm-my-buffers*")))

(defun line-to-top-of-window ()
  "Scroll current line to top of window.

Replaces three keystroke sequence C-u 0 C-l."
  (interactive)
  (recenter 0))

(defun abingham-find-executables (options)
  "Find available executables from OPTIONS."
  (delq nil (mapcar 'executable-find options)))

(defun abingham-setup-pyflakes-executable (executable-name)
  (if (and executable-name
           (executable-find executable-name))
      (setq flycheck-python-pyflakes-executable executable-name)
    (warn "No python flake8 executable found. Flycheck will be disabled for Python!")))

(defun abingham--activate-python (version python-executables pyflakes-executables)
  "Common python 2/3activation routine."
  (message "Activating Python %s toolset." version)
  (let ((pyex (first (abingham-find-executables python-executables))))
    (if pyex
        (set-variable 'python-shell-interpreter pyex)
      (warn "No Python executable found!")))

  ;; (ab-setup-therapy-tests)
  (abingham-setup-pyflakes-executable
   (car (abingham-find-executables pyflakes-executables))))

(defun abingham-activate-python2 ()
  "Hook run when entering python2 environment."
  (abingham--activate-python
   "2"
   '("ipython" "python")
   '("flake8")))

(defun abingham-activate-python3 ()
  "Hook run when entering python3 environment."
  (abingham--activate-python
   "3"
   '("ipython3" "python3" "python")
   '("flake8-3" "flake8")))

(defun abingham-pyvenv-hook ()
  "Hook for when we change virtual environments."

  ;; Do this so that we're sure to pick up the venv's interpreter.
  (therapy-set-python-interpreter (executable-find "python"))

  ;; Activate the right toolset based on the detected major version.
  (therapy-interpreter-changed))

(defun abingham-toggle-org-babel-confirm ()
  "En/disable confirmation of evaluation of org-mode source blocks."
  (interactive)
  (setq org-confirm-babel-evaluate (not org-confirm-babel-evaluate)))

(defun abingham-user-config ()
  "Called by docspacemacs/user-config at the end of everything."
  (global-set-key [(ctrl x) (ctrl k)] 'kill-region)
  (global-set-key [(ctrl x) (ctrl j)] 'copy-region-as-kill)
  (set-face-background 'show-paren-match "moccasin")
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (eval-after-load 'smartparens
    '(progn
       (sp-pair "(" nil :actions :rem)
       (sp-pair "[" nil :actions :rem)
       (sp-pair "'" nil :actions :rem)
       (sp-pair "\"" nil :actions :rem)))

  ;; Ensure that all therapy hooks are run...
  (therapy-interpreter-changed))

(defface abingham-powerline-active1 '((t (:foreground "black"
                                          :background "goldenrod1"
                                          :inherit mode-line)))
  "My Powerline face 1."
  :group 'powerline)

(defface abingham-powerline-active2 '((t (:foreground "black"
                                          :background "goldenrod1"
                                          :inherit mode-line)))
  "My Powerline face 2."
  :group 'powerline)

(defface abingham-powerline-inactive1 '((t (:foreground "black"
                                            :background "steel blue"
                                            :inherit mode-line)))
  "My inactive Powerline face 1."
  :group 'powerline)

(defface abingham-powerline-inactive2 '((t (:foreground "black"
                                            :background "steel blue"
                                            :inherit mode-line)))
  "My inactive Powerline face 2."
  :group 'powerline)

(defun abingham-mode-line-theme ()
    "A custom mode-line theme.

One goal here is to make sure that critical information (like
line number) is visible as often as possible, so it's pulled all
of the way to the left. Also, removed some of the sillier
decorations.

Based off of powerline-default-theme."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face1 (if active 'abingham-powerline-active1 'abingham-powerline-inactive1))
                            (face2 (if active 'abingham-powerline-active2 'abingham-powerline-inactive2))
                            (lhs (list (powerline-raw "%*" face1 'l)
                                       (powerline-raw "%4l" face1 'l)
                                       (powerline-raw ":" face1 'l)
                                       (powerline-raw "%3c" face1 'r)
                                       (powerline-raw " ")
                                       (powerline-raw "%6p" nil 'r)

                                       (when powerline-display-buffer-size
                                         (powerline-buffer-size nil 'l))
                                       (when powerline-display-mule-info
                                         (powerline-raw mode-line-mule-info nil 'l))
                                       (powerline-buffer-id nil 'l)
                                       (when (and (boundp 'which-func-mode) which-func-mode)
                                         (powerline-raw which-func-format nil 'l))
                                       (powerline-raw " ")
                                       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                         (powerline-raw erc-modified-channels-object face1 'l))
                                       (powerline-major-mode face1 'l)
                                       (powerline-process face1)
                                       (powerline-minor-modes face1 'l)
                                       (powerline-narrow face1 'l)
                                       (powerline-raw " " face1)
                                       (powerline-vc face2 'r)
                                       (when (bound-and-true-p nyan-mode)
                                         (powerline-raw (list (nyan-create)) face2 'l))))
                            (rhs (list (powerline-raw global-mode-string face2 'r)
                                       (unless window-system
                                         (powerline-raw (char-to-string #xe0a1) face1 'l))
                                       (when powerline-display-hud
                                         (powerline-hud face2 face1)))))
                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs)))))))
