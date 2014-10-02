package gen

import (
	"github.com/tjim/fatchan"
	"github.com/tjim/smpcc/runtime/ot"
	. "github.com/tjim/smpcc/runtime/gc"
	"log"
	"net"
)

type IO interface {
	ot.Sender
	SendT(t GarbledTable)
	SendK(t Key)
	RecvK2() Key
}

/* TODO: instead of exposing IOX make it private and use IO externally */
type IOX struct {
	channels *Chanio
	otSender ot.Sender
}

func NewIOX(io *Chanio) *IOX {
	result := &IOX{
		io,
		ot.NewOTChansSender(&io.OtChans),
	}
	return result
}

func (io *IOX) SendT(x GarbledTable) {
	io.channels.Tchan <- x
}

func (io *IOX) SendK(x Key) {
	io.channels.Kchan <- x
}

func (io *IOX) RecvK2() Key {
	result := <-io.channels.Kchan2
	return result
}

func (io *IOX) Send(m0, m1 ot.Message) {
	io.otSender.Send(m0, m1)
}

func NewIO(nu chan Chanio) IO {
	io := NewChanio()
	//	defer close(io.Tchan)
	//	defer close(io.Kchan)
	nu <- *io
	return NewIOX(io)
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
	vms := make([]VM, 14)
	for i := range vms {
		io := NewIO(nu)
		vms[i] = newVM(io, ConcurrentId(i))
	}
	main(vms)
}
