;; (global-set-key [(ctrl x) (i)] 'windmove-up)
;; (global-set-key [(ctrl x) (m)] 'windmove-down)
;; (global-set-key [(ctrl x) (j)] 'windmove-left)
;; (global-set-key [(ctrl x) (l)] 'windmove-right)

(global-set-key (kbd "<f5>") 'magit-status)

;; (global-set-key [(ctrl h) (f)] 'helm-apropos)
;; (global-set-key [(ctrl h) (v)] 'helm-apropos)
;; (global-set-key [(ctrl c) (h)] 'helm-command-prefix)
;; (global-set-key [(ctrl c) (ctrl f)] 'helm-find-files)
;; (global-set-key [(ctrl x) (b)] 'helm-mini)
;; ;; (global-set-key [(ctrl c) (h) (o)] 'helm-occur)
;; (global-set-key [(meta x)] 'helm-M-x)

(evil-leader/set-key "bb" 'helm-my-buffers)

;; (global-set-key [(meta /)] 'company-complete)

(global-set-key [(control l)] 'line-to-top-of-window)

;; (global-set-key [(ctrl .)] 'comment-or-uncomment-region)

;; (global-set-key [(ctrl x) (y) (i)] 'yas-insert-snippet)

;; (global-set-key [(ctrl shift c) (ctrl shift l)] 'mc/mark-next-like-this)
