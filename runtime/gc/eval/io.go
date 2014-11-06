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

func Server2(addr string, main func([]VM), numBlocks int, newVM func(io IO, id ConcurrentId) VM) {
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		log.Fatalf("listen(%q): %s", addr, err)
	}

	conn, err := listener.Accept()
	if err != nil {
		log.Fatalf("accept(): %s", err)
	}
	xport := fatchan.New(conn, nil)
	nu := make(chan PerNodePair)
	xport.ToChan(nu)

	x := <-nu
	if numBlocks != len(x.BlockChans) {
		panic("Block mismatch")
	}

	baseSender := ot.NewNPSender(x.NPChans.ParamChan, x.NPChans.NpRecvPk, x.NPChans.NpSendEncs)
	receiver0 := ot.NewStreamReceiver(baseSender, x.BlockChans[0].CAS.R2S, x.BlockChans[0].CAS.S2R)
	ios := make([]IO, numBlocks)
	for i := 0; i < numBlocks; i++ {
		tchan := x.BlockChans[i].Tchan
		kchan := x.BlockChans[i].Kchan
		kchan2 := x.BlockChans[i].Kchan2
		if i == 0 {
			ios[i] = IOX{CircuitChans{tchan, kchan, kchan2}, receiver0}
		} else {
			ios[i] = IOX{CircuitChans{tchan, kchan, kchan2}, receiver0.Fork(x.BlockChans[i].CAS.R2S, x.BlockChans[i].CAS.S2R)}
		}
	}

	vms := make([]VM, numBlocks)
	for i := range vms {
		vms[i] = newVM(ios[i], ConcurrentId(i))
	}
	main(vms)
}
