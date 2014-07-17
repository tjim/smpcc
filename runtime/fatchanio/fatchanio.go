package fatchanio

import "github.com/tjim/smpcc/runtime/base"
import "log"
import "net"

//import "github.com/kylelemons/fatchan"
import "github.com/tjim/fatchan"

func NewGenio(nu chan base.Chanio) base.Genio {
	io := base.NewChanio()
	//	defer close(io.Tchan)
	//	defer close(io.Kchan)
	log.Printf("Writing to nu...\n")
	nu <- *io
	log.Printf("DONE writing to nu\n")
	return base.NewGenX(io)
}

func EvalServer(addr string, eval func(nu chan base.Chanio)) {
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		log.Fatalf("listen(%q): %s", addr, err)
	}

	conn, err := listener.Accept()
	if err != nil {
		log.Fatalf("accept(): %s", err)
	}
	xport := fatchan.New(conn, nil)
	nu := make(chan base.Chanio)
	xport.ToChan(nu)

	eval(nu)
}

func GenClient(addr string, gen func(nu chan base.Chanio)) {
	server, err := net.Dial("tcp", addr)
	if err != nil {
		log.Fatalf("dial(%q): %s", addr, err)
	}

	xport := fatchan.New(server, nil)
	nu := make(chan base.Chanio)
	xport.FromChan(nu)

	gen(nu)
}
