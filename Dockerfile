FROM l3iggs/archlinux

RUN pacman -Sy --noconfirm
RUN pacman -S --noconfirm archlinux-keyring
RUN pacman -Su --noconfirm
RUN pacman -S --needed --noconfirm base-devel git ocaml clang colordiff net-tools

RUN pacman -S --noconfirm ocaml-findlib
# IF FINDLIB IS OUT OF DATE, HERE IS A WAY TO CREATE IT FROM THE SOURCE
# RUN useradd -ms /bin/bash stuff
# RUN (mkdir -p /tmp/ocaml-findlib; chown -R stuff /tmp/ocaml-findlib; cd /tmp/ocaml-findlib; curl -o PKGBUILD https://projects.archlinux.org/svntogit/community.git/plain/trunk/PKGBUILD?h=packages/ocaml-findlib; su stuff -c 'makepkg --noconfirm')

RUN cd /tmp; git clone https://github.com/kerneis/cil; cd cil; ./configure; make clean; make; make install
ADD ./cilext /tmp/cilext
RUN cd /tmp/cilext; ocamlbuild -use-ocamlfind -package cil flattener.cma flattener.cmxs; ocamlfind install flattener META _build/flattener.cma _build/flattener.cmxs
ADD ./compiler /tmp/compiler
RUN cd /tmp/compiler; make clean; make; make install
WORKDIR /root
ENTRYPOINT ["/bin/bash"]
