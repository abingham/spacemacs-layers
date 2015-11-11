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
  (if (executable-find executable-name)
      (setq flycheck-python-pyflakes-executable executable-name)
    (warn "No python flake8 executable found. Flycheck will be disabled for Python!")))

(defun abingham-activate-python2 ()
  "Hook run when entering python2 environment."
  (message "Activating Python 2 toolset.")
  (set-variable 'python-shell-interpreter (first (abingham-find-executables '("ipython" "python"))))
  (unless python-shell-interpreter (warn "No Python executable found!"))
  ;; (ab-setup-therapy-tests)
  (abingham-setup-pyflakes-executable "flake8"))

(defun abingham-activate-python3 ()
  "Hook run when entering python3 environment."
  (message "Activating Python 3 toolset.")
  (set-variable 'python-shell-interpreter (first (abingham-find-executables '("ipython3" "python3" "python"))))
  (unless python-shell-interpreter (warn "No Python executable found!"))
  ;; (ab-setup-therapy-tests)
  (abingham-setup-pyflakes-executable
   (car (abingham-find-executables
           '("flake8-3" "flake8")))))

(defun abingham-pyvenv-hook ()
  "Hook for when we change virtual environments."

  ;; Do this so that we're sure to pick up the venv's interpreter.
  (therapy-set-python-interpreter (executable-find "python"))

  ;; Activate the right toolset based on the detected major version.
  (therapy-interpreter-changed))
