FROM l3iggs/archlinux

RUN curl "https://www.archlinux.org/mirrorlist/?country=CA&country=US&protocol=http&ip_version=4&use_mirror_status=on" | sed 's/#//' > /etc/pacman.d/mirrorlist

RUN pacman -Sy --noconfirm
RUN pacman -S --noconfirm archlinux-keyring
RUN pacman -Su --noconfirm
RUN pacman -S --needed --noconfirm base-devel git ocaml clang colordiff net-tools

RUN pacman -S --noconfirm ocaml-findlib
# IF FINDLIB IS OUT OF DATE, HERE IS A WAY TO CREATE IT FROM THE SOURCE
# RUN useradd -ms /bin/bash stuff
# RUN (mkdir -p /tmp/ocaml-findlib; chown -R stuff /tmp/ocaml-findlib; cd /tmp/ocaml-findlib; curl -o PKGBUILD https://projects.archlinux.org/svntogit/community.git/plain/trunk/PKGBUILD?h=packages/ocaml-findlib; su stuff -c 'makepkg --noconfirm')

RUN cd /tmp; git clone https://github.com/kerneis/cil; cd cil; ./configure; make clean; make; make install
ADD ./compiler /tmp/compiler
RUN cd /tmp/compiler; make clean; make; make install
WORKDIR /root
ENTRYPOINT ["/bin/bash"]
