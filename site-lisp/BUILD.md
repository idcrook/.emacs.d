Build emacs 27.1 on Raspberry Pi OS
-------------

```shell
# SOURCE
mkdir -p ~/projects/build
cd ~/projects/build
git clone --depth 1 --branch emacs-27 https://git.savannah.gnu.org/git/emacs.git emacs-27

# DEPENDENCIES
sudo apt install build-essential git stow

# apt build-dep command below requires deb-src to be uncommented in /etc/apt/sources*
sudo vi /etc/apt/sources.list
sudo vi /etc/apt/sources.list.d/raspi.list 

sudo apt update
sudo apt build-dep emacs
# native JSON support available in emacs27 with jansson
sudo apt install libjansson-dev

# BUILD
cd ~/projects/build/
cd emacs-27/

./autogen.sh
./configure
# uses prefix=/usr/local as default, aligns with make install + stow below
# TODO: investigate --with-xwidgets

# BUILD
# this main build takes about 45 minutes on a Model 3 B
make bootstrap

# INSTALL
sudo make install prefix=/usr/local/stow/emacs

# SYMLINK with stow
sudo su -
cd /usr/local/stow
stow -vv emacs
```

now should be available at `/usr/local/bin/emacs`

