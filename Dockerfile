FROM l3iggs/archlinux

RUN echo "pacman -Sy --noconfirm"
RUN pacman -Sy --noconfirm
RUN echo "pacman -S --noconfirm archlinux-keyring"
RUN pacman -S --noconfirm archlinux-keyring
RUN echo "pacman -Su --noconfirm"
RUN pacman -Su --noconfirm
RUN echo "pacman -S --needed --noconfirm base-devel git ocaml clang go colordiff mercurial"
RUN pacman -S --needed --noconfirm base-devel git ocaml clang go colordiff mercurial net-tools
# FINDLIB IS OUT OF DATE SO HERE IS A WAY TO CREATE IT
RUN useradd -ms /bin/bash stuff
# RUN (mkdir -p /tmp/ocaml-findlib; chown -R stuff /tmp/ocaml-findlib; cd /tmp/ocaml-findlib; curl -o PKGBUILD https://projects.archlinux.org/svntogit/community.git/plain/trunk/PKGBUILD?h=packages/ocaml-findlib; su stuff -c 'makepkg --noconfirm')
RUN pacman -S --noconfirm ocaml-findlib
RUN echo "git clone https://github.com/kerneis/cil; cd cil; ./configure; make; make install"
RUN git clone https://github.com/kerneis/cil; cd cil; ./configure; make; make install
RUN echo "cp -pr /vagrant/cilext /tmp/cilext"
ADD ./cilext /tmp/cilext
RUN echo "cd /tmp/cilext; ocamlbuild -use-ocamlfind -package cil flattener.cma flattener.cmxs; ocamlfind install flattener META _build/flattener.cma _build/flattener.cmxs"
RUN cd /tmp/cilext; ocamlbuild -use-ocamlfind -package cil flattener.cma flattener.cmxs; ocamlfind install flattener META _build/flattener.cma _build/flattener.cmxs
RUN echo "cp -pr /vagrant/compiler /tmp/compiler"
ADD ./compiler /tmp/compiler
RUN echo "cd /tmp/compiler; make; make install"
RUN cd /tmp/compiler; make; make install

ADD ./gowork /root/gowork
ENV GOPATH /root/gowork
RUN go get golang.org/x/crypto/sha3
RUN go get github.com/tjim/fatchan
WORKDIR /root/examples
ENTRYPOINT ["smpcc", "-circuitlib", "gmw"]
CMD ["vickrey.c"]