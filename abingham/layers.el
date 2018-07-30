(configuration-layer/declare-layers
 '(
   ;; (auto-completion
   ;; :variables
   ;; auto-completion-enable-help-tooltip t
   ;; auto-completion-enable-snippets-in-popup t)
   c-c++
   emacs-lisp
   git
   html
   ivy
   javascript
   markdown
   org
   (python
    ;; We do our own lsp-backend stuff for now.
    :variables python-backend nil)
   restclient
   shell-scripts
   (syntax-checking
    :variables
    syntax-checking-enable-tooltips nil)
   terraform
   ;; themes-megapack
   version-control
   yaml
   ycmd
   )
 )
