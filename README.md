# emacs-config

## wip has moved

Future work on this repository has moved to the monorepo [valera-rozuvan/dotfiles](https://github.com/valera-rozuvan/dotfiles). This repo is archived for historic purposes (to preserve commit history). Navigate over to [dotfiles/emacs/emacs-config](https://github.com/valera-rozuvan/dotfiles/tree/main/emacs/emacs-config) to see updates (if any).

## introduction

My configuration for emacs.

Requirements
------------

It is assumed that Emacs version [24.3](http://lists.gnu.org/archive/html/info-gnu-emacs/2013-03/msg00001.html) or later is installed on your system.

Setup
-----

    cd ~/
    rm -rf .emacs*
    mkdir tmp
    cd tmp
    git clone git@github.com:valera-rozuvan/emacs_config.git
    cd emacs_config
    mv _emacs.d .emacs.d
    mv .emacs ~/
    mv .emacs.d ~/
    cd ~/
    rm -rf tmp

Author
------

[Valera Rozuvan](http://valera.rozuvan.net)
