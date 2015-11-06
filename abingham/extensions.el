(setq abingham-pre-extensions
      '(
        ))

(setq abingham-post-extensions
      '(
        codesearch
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

(defun abingham/init-wilt ()
  "Initialize wilt."
  (use-package wilt
    (add-hook 'python-mode-hook 'wilt-mode)
    )
  )
