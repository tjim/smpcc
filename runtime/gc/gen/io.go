package gen

import (
	"github.com/tjim/fatchan"
	. "github.com/tjim/smpcc/runtime/gc"
	"github.com/tjim/smpcc/runtime/ot"
	"log"
	"math/big"
	"net"
	"time"
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
	// temporary hack to avoid a fatchan deadlock
	// (this allows the eval side to finish registering channels and start block goroutines before we send on the channels)
	time.Sleep(time.Second)
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

	ParamChan := make(chan big.Int)
	NpRecvPk := make(chan big.Int)
	NpSendEncs := make(chan ot.HashedElGamalCiph)
	x := PerNodePair{ot.NPChans{ParamChan, NpRecvPk, NpSendEncs}, make([]PerBlock, numBlocks)}

	baseReceiver := ot.NewNPReceiver(ParamChan, NpRecvPk, NpSendEncs)

	ios := make([]IO, len(x.BlockChans))
	for i := 0; i < numBlocks; i++ {
		S2R := make(chan ot.MessagePair)
		R2S := make(chan []byte)
		Tchan := make(chan GarbledTable)
		Kchan := make(chan Key)
		Kchan2 := make(chan Key)
		x.BlockChans[i] = PerBlock{ClientAsSender{S2R, R2S}, CircuitChans{Tchan, Kchan, Kchan2}}
	}
	nu <- x
	sender0 := ot.NewStreamSender(baseReceiver, x.BlockChans[0].CAS.S2R, x.BlockChans[0].CAS.R2S)
	for i := 0; i < numBlocks; i++ {
		var sender ot.Sender
		if i == 0 {
			sender = sender0
		} else {
			sender = sender0.Fork(x.BlockChans[i].CAS.S2R, x.BlockChans[i].CAS.R2S)
		}
		ios[i] = IOX{x.BlockChans[i].CircuitChans, sender}
	}

	vms := make([]VM, numBlocks)
	for i := range vms {
		vms[i] = newVM(ios[i], ConcurrentId(i))
	}
	// temporary hack to avoid a fatchan deadlock
	// (this allows the eval side to finish registering channels and start block goroutines before we send on the channels)
	time.Sleep(time.Second)
	main(vms)
}
