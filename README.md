This repo is intended to be dropped directly into the `.emacs.d/private` of a
spacemacs configuration.

In short, to bootstrap a new spacemacs installation:
```
cd .emacs.d/private
git clone --recursive git@github.com:abingham/spacemacs-layers.git +abingham
cp +abingham/dot.spacemacs ~/.spacemacs
...edit ~/.spacemacs as necessary, e.g. for ycmd paths, etc...
emacs
```

Note that this repo has submodules, hence the `--recursive` above.
