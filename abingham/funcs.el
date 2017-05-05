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

(defun abingham-user-config ()
  "Called by docspacemacs/user-config at the end of everything."
  ;; (global-set-key [(ctrl x) (ctrl k)] 'kill-region)
  ;; (global-set-key [(ctrl x) (ctrl j)] 'copy-region-as-kill)

  ;; This tries to address the issue mentioned here: https://github.com/syl20bnr/spacemacs/issues/5435
  (add-hook 'spacemacs-buffer-mode-hook
            (lambda ()
              (set (make-local-variable 'mouse-1-click-follows-link) nil)))

  (set-face-background 'show-paren-match "moccasin")
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (if (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode)))

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
