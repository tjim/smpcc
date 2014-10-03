package eval

import (
	"github.com/tjim/fatchan"
	. "github.com/tjim/smpcc/runtime/gc"
	"github.com/tjim/smpcc/runtime/ot"
	"log"
	"net"
)

type IO interface {
	ot.Receiver
	RecvT() GarbledTable
	RecvK() Key
	SendK2(t Key)
}

/* TODO: instead of exposing IOX make it private and use IO externally */
type IOX struct {
	CircuitChans
	ot.Receiver
}

func NewIOX(io Chanio) *IOX {
	return &IOX{
		io.CircuitChans,
		ot.NewOTChansReceiver(io.NPChans, io.ExtChans),
	}
}

func Server(addr string, main func([]VM), numBlocks int, newVM func(io IO, id ConcurrentId) VM) {
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		log.Fatalf("listen(%q): %s", addr, err)
	}

	conn, err := listener.Accept()
	if err != nil {
		log.Fatalf("accept(): %s", err)
	}
	xport := fatchan.New(conn, nil)
	nu := make(chan Chanio)
	xport.ToChan(nu)

	vms := make([]VM, numBlocks)
	for i := range vms {
		io := <-nu
		vms[i] = newVM(NewIOX(io), ConcurrentId(i))
	}
	main(vms)
}
