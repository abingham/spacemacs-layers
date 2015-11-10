;; (require )

(setq abingham-pre-extensions
      '(
        ))

(setq abingham-post-extensions
      '(
        codesearch
        therapy
        wilt
        ))

(defun abingham/init-codesearch ()
  "Initialize codesearch"
  (use-package codesearch
    :bind
    (("M-'" . codesearch-search)
     ("M-." . projectile-codesearch-search))
    :config
    (set-variable 'codesearch-cindex "~/go/bin/cindex")
    (set-variable 'codesearch-csearch "~/go/bin/csearch")
    (set-variable 'codesearch-cindex-flags '())))

(defun abingham/init-therapy ()
  (use-package therapy
    :config
    (progn
      (add-hook
       'therapy-python3-hooks
       'abingham-activate-python3)

      (add-hook
       'therapy-python2-hooks
       'abingham-activate-python2)

      ;; run the appropriate hooks
      (therapy-interpreter-changed))))

(defun abingham/init-wilt ()
  "Initialize wilt."
  (use-package wilt
    :init
    (add-hook 'python-mode-hook 'wilt-mode)
    )
  )
