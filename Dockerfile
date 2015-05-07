FROM dock0/arch

RUN curl "https://www.archlinux.org/mirrorlist/?country=CA&country=US&protocol=http&ip_version=4&use_mirror_status=on" | sed 's/#//' > /etc/pacman.d/mirrorlist

RUN pacman -Sy --needed --noconfirm
RUN pacman -S --needed --noconfirm archlinux-keyring
RUN pacman -Su --needed --noconfirm
RUN pacman -S --needed --noconfirm base-devel
RUN pacman -S --needed --noconfirm git
RUN pacman -S --needed --noconfirm ocaml
RUN pacman -S --needed --noconfirm clang
RUN pacman -S --needed --noconfirm colordiff
RUN pacman -S --needed --noconfirm net-tools
RUN pacman -S --needed --noconfirm wget

RUN pacman -S --noconfirm ocaml-findlib
# IF FINDLIB IS OUT OF DATE, HERE IS A WAY TO CREATE IT FROM THE SOURCE
# RUN useradd -ms /bin/bash stuff
# RUN (mkdir -p /tmp/ocaml-findlib; chown -R stuff /tmp/ocaml-findlib; cd /tmp/ocaml-findlib; curl -o PKGBUILD https://projects.archlinux.org/svntogit/community.git/plain/trunk/PKGBUILD?h=packages/ocaml-findlib; su stuff -c 'makepkg --noconfirm')

RUN cd /tmp && wget https://github.com/ocaml-batteries-team/batteries-included/archive/v2.3.0.tar.gz && echo "649eb2dca1f51bf9125ea87465e24e0ddcea9138  v2.3.0.tar.gz" | sha1sum -c --status && tar xfz v2.3.0.tar.gz && cd batteries-included-2.3.0 && make && make install

RUN cd /tmp; git clone https://github.com/kerneis/cil; cd cil; ./configure; make clean; make; make install
ADD ./compiler /tmp/compiler
RUN cd /tmp/compiler; make clean; make; make install
WORKDIR /root
ENTRYPOINT ["/bin/bash"]
