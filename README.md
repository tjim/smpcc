smpcc
=====

smpcc is a compiler for secure multiparty computation.

Authors: Trevor Jim and Yevgeniy Vahlis

## Installation

smpcc compiles C programs to [go](http://golang.org/) programs.
You'll want to set up a go workspace to run your compiled programs and
install the required packages:

    mkdir gowork
    cd gowork
    export GOPATH=`pwd`:$GOPATH
    go get github.com/tjim/smpcc/runtime/base

To build the compiler itself you'll need ocaml and clang.  Their setup
is a little complicated, so we recommend that you install them using a
virtual machine.  We recommend installing
[VirtualBox](https://www.virtualbox.org/).  Then you can either use
[Vagrant](http://www.vagrantup.com/) [Packer](http://www.packer.io/),
or [docker](http://docker.com/) to finish the build.

### Installation using Vagrant and Packer

Once you've installed VirtualBox, Vagrant, and Packer, follow the
instructions at <https://github.com/elasticdog/packer-arch> to build
an Arch Linux virtual machine and add it to Vagrant.

Now cd into this directory (containing this file README.md) and do

    vagrant up

This will start up the box and provision it with the necessary software.

After this, you can ssh into the box with

    vagrant ssh

Your username will be vagrant and your home directory will be
/home/vagrant.  This directory (containing this file INSTALL) is
accessible from /vagrant.  Any changes you make to that directory will
be shared outside the box.  Compiling the software must be done inside
the box, but editing can take place either inside or outside the box.

You can enter and exit the box with ssh whenever you like.  To halt
the box while preserving its state

    vagrant halt

To start up the box again with its state

    vagrant up

To destroy the box completely, including its state,

    vagrant destroy

Once destroyed you can bring it up again to a fresh state with

    vagrant up

### Installation using Docker 

Install [docker](http://docker.com/),
[boot2docker](https://github.com/boot2docker/boot2docker), or another
tool that allows you to run docker.  Once docker is running on your
system, run this from the top level of the smpcc source directory:

    docker build -t smpcc .

Then run

    docker run -itv `pwd`:/root smpcc

This brings up a shell with the smpcc compiler built, installed, and
available.  The current directory on the host (outside the container)
is mounted as your directory inside the container; any files outside
of your current directory on the host are not available.  (See the
docker documentation for more details.)

You can compile C programs to go programs from within the container
(see below).  Go is not installed in the container so you should run
the compiled output outside of the container.

## Using the compiler

smpcc can compile a self-contained C file.  For example put the
following in foo.c:

    int main() {
        return 5;
    }

Then you can compile and run the program as follows:

    $ smpcc foo.c

This produces the output file foo.go.

To run the output file you need to have go installed (which may be
outside of your virtual machine, if you used a virtual machine to
install smpcc).

    $ go run foo.go -sim
    eval: 5
    gen: 5
    Done
    $

This runs the program in a single process; the generator and evaluator
run as different (sets of) goroutines.  You can also run in two
separate processes:

    $ go run foo.go -id 1 &
    $ go run foo.go
    eval: 5
    gen: 5
    Done

You can supply input to the program over the command line.  Put the
following in foo.c:

    extern int input(int);

    int main() {
      int x = 2 * input(0);
      int y = input(1);
      return x+y;
    }

Here input() is a function that reads input from one of the two
parties.  The argument to input() identifies the party; for a
two-party back end it should be 0 or 1; for an n-party back end it
should be between 0 and n-1.

Then you can do

    $ smpcc foo.c
    $ go run foo.go -sim 9 2
    eval: 20
    gen: 20
    Done
    $

Here 9 is an input supplied by the generator (party 0) and 2 is an
input supplied by the evaluator (party 1).

See the examples directory for some more complicated examples.

## Garbled circuit back ends

The compiler has several back ends for garbled circuits:

* yao: classic Yao-style garbled circuits
* yaor: Yao with row reduction
* gax: from the paper "Efficient Garbling from a Fixed-Key Blockcipher," by Bellare, Hoang, Keelveedhi, Rogaway. IEEE Security and Privacy, 2013.
* gaxr: GaX with row reduction

The default is yao.  To compile for another back end, use the
-circuitlib flag, e.g.:

    $ smpcc foo.c -circuitlib gaxr

## GMW

We have an implementation of GMW using boolean circuits.
To use, compile like this:

    $ smpcc foo.c -circuitlib gmw

Within the c file you can read an input by declaring

    extern unsigned int input(unsigned int);

Then you read from party n with input(n).  Obtain the number of parties with

    extern unsigned int num_peers();
