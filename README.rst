.. -*-rst-*-

==============
 lobstermacs2
==============

:name:        lobstermacs2
:description: My Emacs Configuration
:copyright:   © 2011 Justine Alexandra Roberts Tunney
:license:     Licensed MIT


Introduction
============

Build dependencies if you want *all* the emacs 24 bells and whistles::

    sudo apt-get install \
        bzr ispell git-core libgtk-3-dev libxpm-dev texinfo librsvg2-dev \
        libgif-dev libjpeg62-dev libpng12-dev libtiff4-dev imagemagick \
        libdbus-1-dev libasound2 libgconf2-dev libfreetype6-dev libotf-dev \
        libm17n-dev libgnutls-dev libgpm-dev libxft-dev libxml2-dev \
        libmagickwand-dev

Install bleeding edge emacs from repository::

    bzr branch bzr://bzr.savannah.gnu.org/emacs/trunk emacs
    ./configure --with-x-toolkit=gtk3
    make -j4
    sudo make install

Install this project::

    git clone 
