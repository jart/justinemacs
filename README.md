# justinemacs

```sh
sudo apt-get install -y aspell aspell-en build-essential libacl1-dev libgnutls28-dev libgpm-dev libncurses5-dev libncursesw5-dev libxml2-dev zlib1g-dev
wget https://ftp.gnu.org/gnu/emacs/emacs-25.3.tar.gz
tar -xf emacs-25.3.tar.gz
pushd emacs-25.3
CFLAGS='-O3 -march=native' ./configure --disable-silent-rules --with-x-toolkit=no --with-xpm=no --with-tiff=no --with-x=no
make -j16
sudo make install
popd
git clone https://github.com/jart/justinemacs.git .emacs.d
```
