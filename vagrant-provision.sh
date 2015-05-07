#!/usr/bin/env bash

pacman -Sy --needed --noconfirm
pacman -S --needed --noconfirm archlinux-keyring
pacman -Su --needed --noconfirm
pacman -S --needed --noconfirm base-devel
pacman -S --needed --noconfirm git
pacman -S --needed --noconfirm ocaml
pacman -S --needed --noconfirm clang
pacman -S --needed --noconfirm colordiff
pacman -S --needed --noconfirm net-tools
pacman -S --needed --noconfirm wget
pacman -S --needed --noconfirm ocaml-findlib

# IF FINDLIB IS OUT OF DATE HERE IS A WAY TO CREATE IT
# (mkdir -p ocaml-findlib; cd ocaml-findlib; curl -o PKGBUILD https://projects.archlinux.org/svntogit/community.git/plain/trunk/PKGBUILD?h=packages/ocaml-findlib; makepkg -i --asroot --noconfirm)

(cd /tmp && wget https://github.com/ocaml-batteries-team/batteries-included/archive/v2.3.0.tar.gz && echo "649eb2dca1f51bf9125ea87465e24e0ddcea9138  v2.3.0.tar.gz" | sha1sum -c --status && tar xfz v2.3.0.tar.gz && cd batteries-included-2.3.0 && make && make install)

git clone https://github.com/kerneis/cil; cd cil; ./configure; make; make install
cp -pr /vagrant/compiler /tmp/compiler
(cd /tmp/compiler; make; make install)

mkdir -p /home/vagrant/gowork/src/github.com/tjim
ln -s /vagrant /home/vagrant/gowork/src/github.com/tjim/smpcc
export GOPATH=/home/vagrant/gowork
go get golang.org/x/crypto/sha3
go get github.com/tjim/fatchan
cat >>/home/vagrant/.bashrc <<EOF
export GOPATH=/home/vagrant/gowork
EOF
