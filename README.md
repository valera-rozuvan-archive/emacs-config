emacs_config
============

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
