(setq abingham-root-dir (file-name-directory (or (buffer-file-name)
                                                 load-file-name)))

(defun abingham-load-snippets ()
  (let ((snip-dir (expand-file-name "snippets" abingham-root-dir)))
    (add-to-list 'yas-snippet-dirs snip-dir t)
    (yas/load-directory snip-dir)))

(eval-after-load 'yasnippet
  '(abingham-load-snippets))

(defun line-to-top-of-window ()
  "Scroll current line to top of window.

Replaces three keystroke sequence C-u 0 C-l."
  (interactive)
  (recenter 0))

(defun lsp-action-retrieve-and-run ()
  "Retrieve, select, and run code action."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-request-async (lsp--make-request
                            "textDocument/codeAction"
                            (lsp--text-document-code-action-params))
                           (lambda (actions)
                             (setq lsp-code-actions actions)
                             (condition-case nil
                                 (call-interactively 'lsp-execute-code-action)
                               (quit "Quit")))))

(defun abingham-user-init ()
  ;; In prog-mode we want to use hasklig with ligatures.
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq buffer-face-mode-face '(:family "Hasklig" :height 130))
              (buffer-face-mode 1)))
  (setq exec-path-from-shell-check-startup-files nil))

(defun abingham-user-config ()
  "Called by docspacemacs/user-config at the end of everything."
  ;; (global-set-key [(ctrl x) (ctrl k)] 'kill-region)
  ;; (global-set-key [(ctrl x) (ctrl j)] 'copy-region-as-kill)

  ;; Making neotree work...https://github.com/jaypei/emacs-neotree/issues/226
  ;; (helm-autoresize-mode 1)
  ;; (setq helm-split-window-in-side-p t)

  ;; This tries to address the issue mentioned here: https://github.com/syl20bnr/spacemacs/issues/5435
  (add-hook 'spacemacs-buffer-mode-hook
            (lambda ()
              (set (make-local-variable 'mouse-1-click-follows-link) nil)))

  (set-face-background 'show-paren-match "moccasin")

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; (if (fboundp 'mac-auto-operator-composition-mode)
  ;;     (mac-auto-operator-composition-mode))
  )

(defun abingham-ycmd-semantic-completion ()
  "Do a semantic completion with YCMD."
  (interactive)
  (company-cancel)
  (let ((ycmd-force-semantic-completion (not (company-ycmd--in-include))))
    (setq company-backend 'company-ycmd)
    (company-manual-begin)))

(defun abingham-use-outline-for-imenu ()
  (require 'outline)
  (setq imenu-create-index-function
        'imenu-default-create-index-function
        imenu-generic-expression
        (list (list nil (concat "^\\(?:" outline-regexp "\\).*$") 0))))

(defun abingham-show-point ()
  (interactive)
  (message "Point: %s" (point)))
