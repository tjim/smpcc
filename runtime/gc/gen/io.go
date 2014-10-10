package gen

import (
	"github.com/tjim/fatchan"
	. "github.com/tjim/smpcc/runtime/gc"
	"github.com/tjim/smpcc/runtime/ot"
	"log"
	"math/big"
	"net"
	"fmt"
)

type IO interface {
	ot.Sender
	SendT(t GarbledTable)
	SendK(t Key)
	RecvK2() Key
}

/* TODO: instead of exposing IOX make it private and use IO externally */
type IOX struct {
	CircuitChans
	ot.Sender
}

func NewIOX(io Chanio) *IOX {
	result := &IOX{
		io.CircuitChans,
		ot.NewOTChansSender(io.NPChans, io.ExtChans),
	}
	return result
}

func NewIO(nu chan Chanio) IO {
	io := NewChanio()
	//	defer close(io.Tchan)
	//	defer close(io.Kchan)
	nu <- *io
	return NewIOX(*io)
}

func Client(addr string, main func([]VM), numBlocks int, newVM func(io IO, id ConcurrentId) VM) {
	server, err := net.Dial("tcp", addr)
	if err != nil {
		log.Fatalf("dial(%q): %s", addr, err)
	}

	xport := fatchan.New(server, nil)
	nu := make(chan Chanio)
	xport.FromChan(nu)

	defer close(nu)
	vms := make([]VM, numBlocks)
	for i := range vms {
		io := NewIO(nu)
		vms[i] = newVM(io, ConcurrentId(i))
	}
	main(vms)
}

func Client2(addr string, main func([]VM), numBlocks int, newVM func(io IO, id ConcurrentId) VM) {
	server, err := net.Dial("tcp", addr)
	if err != nil {
		log.Fatalf("dial(%q): %s", addr, err)
	}

	xport := fatchan.New(server, nil)
	nu := make(chan PerNodePair)
	xport.FromChan(nu)

	defer close(nu)

	k := 80
	m := 1024

	ParamChan := make(chan big.Int)
	NpRecvPk := make(chan big.Int)
	NpSendEncs := make(chan ot.HashedElGamalCiph)
	RefreshCh := make(chan int)
	x := PerNodePair{ot.NPChans{ParamChan, NpRecvPk, NpSendEncs}, ot.PerNodePairMplexChans{RefreshCh}, make([]PerBlock, numBlocks)}

	baseReceiver := ot.NewNPReceiver(ParamChan, NpRecvPk, NpSendEncs)

	fmt.Println("Step 0")

	chS := ot.PrimarySender(baseReceiver, RefreshCh, k, m) // chS for getrequests

	fmt.Println("Step 1")

	ios := make([]IO, len(x.BlockChans))
	for i := 0; i < numBlocks; i++ {
		reqCh := make(chan ot.SendRequest)
		repCh := make(chan []byte)
		sender := ot.NewMplexSender(repCh, reqCh, chS)
		Tchan := make(chan GarbledTable)
		Kchan := make(chan Key)
		Kchan2 := make(chan Key)
		x.BlockChans[i] = PerBlock{ot.PerBlockMplexChans{repCh, reqCh}, CircuitChans{Tchan, Kchan, Kchan2}}
		ios[i] = IOX{CircuitChans{Tchan, Kchan, Kchan2}, sender}
	}
	nu <- x
	fmt.Println("Step 2")

	vms := make([]VM, numBlocks)
	for i := range vms {
		vms[i] = newVM(ios[i], ConcurrentId(i))
	}
	main(vms)
}
