package fatchanio

import "github.com/tjim/smpcc/runtime/gc"
import "log"
import "net"

//import "github.com/kylelemons/fatchan"
import "github.com/tjim/fatchan"

func NewGenio(nu chan gc.Chanio) gc.Genio {
	io := gc.NewChanio()
	//	defer close(io.Tchan)
	//	defer close(io.Kchan)
	nu <- *io
	return gc.NewGenX(io)
}

func EvalServer(addr string, eval func(nu chan gc.Chanio)) {
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		log.Fatalf("listen(%q): %s", addr, err)
	}

	conn, err := listener.Accept()
	if err != nil {
		log.Fatalf("accept(): %s", err)
	}
	xport := fatchan.New(conn, nil)
	nu := make(chan gc.Chanio)
	xport.ToChan(nu)

	eval(nu)
}

func GenClient(addr string, gen func(nu chan gc.Chanio)) {
	server, err := net.Dial("tcp", addr)
	if err != nil {
		log.Fatalf("dial(%q): %s", addr, err)
	}

	xport := fatchan.New(server, nil)
	nu := make(chan gc.Chanio)
	xport.FromChan(nu)

	gen(nu)
}
