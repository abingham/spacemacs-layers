(configuration-layer/declare-layers
 '(
   ;; (auto-completion
   ;; :variables
   ;; auto-completion-enable-help-tooltip t
   ;; auto-completion-enable-snippets-in-popup t)
   auto-completion
   c-c++
   elm
   emacs-lisp
   git
   html
   ivy
   javascript
   lsp
   markdown
   org
   (python
    ;; We do our own lsp-backend stuff for now.
    :variables python-backend 'lsp)
   restclient
   restructuredtext
   rust
   shell-scripts
   spell-checking
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
