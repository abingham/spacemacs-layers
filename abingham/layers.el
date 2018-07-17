(configuration-layer/declare-layers
 '(
   asciidoc
   (auto-completion
    :variables
    auto-completion-enable-help-tooltip t
    auto-completion-enable-snippets-in-popup t)
   c-c++
   clojure
   csharp
   docker
   elm
   emacs-lisp
   git
   go
   html
   ivy
   javascript
   ;; lsp
   markdown
   nginx
   org
   purescript
   (python
    ;; We do our own lsp-backend stuff for now.
    :variables python-backend nil)
   restclient
   ruby
   rust
   shell-scripts
   (syntax-checking
    :variables
    syntax-checking-enable-tooltips nil)
   terraform
   themes-megapack
   version-control
   yaml
   ycmd)
 )
