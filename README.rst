.. -*-rst-*-

=============
 justinemacs
=============

:name:        justinemacs
:description: My Emacs Configuration
:copyright:   © 2012 Justine Alexandra Roberts Tunney
:license:     Licensed MIT


Introduction
============

Here's how to build emacs24::

    sudo apt-get install \
        ispell git-core libgtk-3-dev libxpm-dev texinfo librsvg2-dev \
        libgif-dev libjpeg62-dev libpng12-dev libtiff4-dev imagemagick \
        libdbus-1-dev libasound2 libgconf2-dev libfreetype6-dev libotf-dev \
        libm17n-dev libgnutls-dev libgpm-dev libxft-dev libxml2-dev \
        libmagickwand-dev
    wget http://ftp.gnu.org/pub/gnu/emacs/emacs-24.2.tar.gz
    tar -xvzf emacs-24.2.tar.gz
    cd emacs-24.2
    ./configure
    make -j2
    sudo make install

Install this project::

    git clone git://github.com/jart/justinemacs ~/.emacs.d
    cd ~/.emacs.d
    ./vendor.sh
    emacs
