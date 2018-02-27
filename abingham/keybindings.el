;; (global-set-key [(ctrl x) (i)] 'windmove-up)
;; (global-set-key [(ctrl x) (m)] 'windmove-down)
;; (global-set-key [(ctrl x) (j)] 'windmove-left)
;; (global-set-key [(ctrl x) (l)] 'windmove-right)

;; (global-set-key (kbd "<f5>") 'magit-status)

;; (global-set-key [(ctrl h) (f)] 'helm-apropos)
;; (global-set-key [(ctrl h) (v)] 'helm-apropos)
;; (global-set-key [(ctrl c) (h)] 'helm-command-prefix)
;; (global-set-key [(ctrl c) (ctrl f)] 'helm-find-files)
;; (global-set-key [(ctrl x) (b)] 'helm-mini)
;; ;; (global-set-key [(ctrl c) (h) (o)] 'helm-occur)
;; (global-set-key [(meta x)] 'helm-M-x)



;; (global-set-key [(meta /)] 'company-complete)

(global-set-key [(control l)] 'line-to-top-of-window)
(global-set-key [(control tab)] 'abingham-ycmd-semantic-completion)



(spacemacs/set-leader-keys "ww" 'ace-window)
(spacemacs/set-leader-keys "wec" 'eyebrowse-create-window-config)
(spacemacs/set-leader-keys "weC" 'eyebrowse-close-window-config)
(spacemacs/set-leader-keys "wen" 'eyebrowse-next-window-config)
(spacemacs/set-leader-keys "wep" 'eyebrowse-prev-window-config)
(spacemacs/set-leader-keys "wer" 'eyebrowse-rename-window-config)
(spacemacs/set-leader-keys "wes" 'eyebrowse-switch-to-window-config)

;; (global-set-key [(ctrl .)] 'comment-or-uncomment-region)

;; (global-set-key [(ctrl x) (y) (i)] 'yas-insert-snippet)

;; (global-set-key [(ctrl shift c) (ctrl shift l)] 'mc/mark-next-like-this)
