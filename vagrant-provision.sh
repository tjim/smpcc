#!/usr/bin/env bash

echo "pacman -Sy --needed --noconfirm base-devel git llvm-ocaml clang go"
pacman -Sy --needed --noconfirm base-devel git llvm-ocaml clang go colordiff
echo "cp -pr /vagrant/arch/ocaml-findlib /tmp/ocaml-findlib"
cp -pr /vagrant/arch/ocaml-findlib /tmp/ocaml-findlib
echo "cd /tmp/ocaml-findlib; makepkg --asroot --noconfirm -i -s"
cd /tmp/ocaml-findlib; makepkg --asroot --noconfirm -i -s
echo "git clone https://github.com/kerneis/cil; cd cil; ./configure; make; make install"
git clone https://github.com/kerneis/cil; cd cil; ./configure; make; make install
echo "cp -pr /vagrant/cilext /tmp/cilext"
cp -pr /vagrant/cilext /tmp/cilext
echo "cd /tmp/cilext; ocamlbuild -use-ocamlfind -package cil flattener.cma flattener.cmxs; ocamlfind install flattener META _build/flattener.cma _build/flattener.cmxs"
cd /tmp/cilext; ocamlbuild -use-ocamlfind -package cil flattener.cma flattener.cmxs; ocamlfind install flattener META _build/flattener.cma _build/flattener.cmxs
echo "cp -pr /vagrant/compiler /tmp/compiler"
cp -pr /vagrant/compiler /tmp/compiler
echo "cd /tmp/compiler; make; make install"
cd /tmp/compiler; make; make install
echo "cd"
cd
