This repo is intended to be dropped directly into the `.emacs.d/private` of a
spacemacs configuration. 

## Bootstrapping a new installation
```
# go your home directory, or wherever you want .emacs.d
cd ~

# (optional) move any existing .emacs.d out of the way
mv .emacs.d .emacs.d.BACKUP

git clone https://github.com/syl20bnr/spacemacs.git .emacs.d
cd .emacs.d
cd private
git clone --recursive git@github.com:abingham/spacemacs-layers.git +abingham
cp +abingham/dot.spacemacs ~/.spacemacs
# ...edit ~/.spacemacs as necessary, e.g. for ycmd paths, etc...
```

Note that this repo has submodules, hence the `--recursive` above.
