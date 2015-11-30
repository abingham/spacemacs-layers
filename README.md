This repo is intended to be dropped directly into the `.emacs.d/private` of a
spacemacs configuration. This works somewhat hand-in-hand with my own fork of spacemacs (though in principle it should work with vanilla spacemacs).

## Bootstrapping a new installation
```
# go your home directory, or wherever you want .emacs.d
cd ~

# (optional) move any existing .emacs.d out of the way
mv .emacs.d .emacs.d.BACKUP

git clone https://github.com/abingham/spacemacs.git .emacs.d
cd .emacs.d
git checkout -b sixty-north origin/sixty-north
cd .emacs.d/private
git clone --recursive git@github.com:abingham/spacemacs-layers.git +abingham
cp +abingham/dot.spacemacs ~/.spacemacs
# ...edit ~/.spacemacs as necessary, e.g. for ycmd paths, etc...
```

Note that this repo has submodules, hence the `--recursive` above.
