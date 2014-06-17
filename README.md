smpcc
=====

smpcc is a compiler for secure multiparty computation.

Authors: Trevor Jim and Yevgeniy Vahlis

## Installation

smpcc compiles C programs to go programs.  You'll want to set up a go
workspace to run your compiled programs and install the required
packages:

    mkdir gowork
    cd gowork
    export GOPATH=`pwd`:$GOPATH
    go get github.com/tjim/smpcc/runtime/base

To build the compiler itself you'll need ocaml and clang.  Their setup
is a little complicated, so we recommend that you install them using
Vagrant, Packer, and VirtualBox.

Begin by installing [Vagrant](http://www.vagrantup.com/),
[Packer](http://www.packer.io/), and
[VirtualBox](https://www.virtualbox.org/).

Follow the instructions at <https://github.com/elasticdog/packer-arch>
to build an Arch Linux virtual machine and add it to Vagrant.

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

## Usage

smpcc can compile a self-contained C file.  For example put the
following in foo.c:

    int main() {
        return 5;
    }

Then you can compile and run the program as follows:

    $ smpcc foo.c
    $ go run *.go
    eval: 5
    gen: 5
    Done
    $ 

This runs the program in a single process; the generator and evaluator
run as different (sets of) goroutines.  smpcc can also produce go
programs that run the generator and evaluator on different machines.

You can supply input to the program over the command line.  Put the
following in foo.c:

    extern int gen_int();
    extern int eval_int();

    int main() {
      int x = gen_int();
      int y = eval_int();
      return x+y;
    }

Then you can do

    $ smpcc foo.c
    $ go run *.go 9 2
    eval: 11
    gen: 11
    Done
    $ 

Here 9 is an input supplied by the generator and 2 is an input
supplied by the evaluator.

See the examples directory for some more complicated examples.

## GMW

We have a simulated (non-networked) implementation of GMW using boolean circuits.
To use compile like this:

    $ smpcc foo.c -circuitlib gmw

Within the c file you can read an input by declaring

    extern unsigned int input(unsigned int);

Then you read from party n with input(n).

