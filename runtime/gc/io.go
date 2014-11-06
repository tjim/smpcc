package gc

import (
	"github.com/tjim/smpcc/runtime/ot"
	"math/big"
)

type CircuitChans struct {
	Tchan  chan GarbledTable `fatchan:"request"`
	Kchan  chan Key          `fatchan:"request"`
	Kchan2 chan Key          `fatchan:"reply"`
}

func (io CircuitChans) SendT(x GarbledTable) {
	io.Tchan <- x
}

func (io CircuitChans) SendK(x Key) {
	io.Kchan <- x
}

func (io CircuitChans) RecvK2() Key {
	result := <-io.Kchan2
	return result
}

func (io CircuitChans) SendK2(x Key) {
	io.Kchan2 <- x
}

func (io CircuitChans) RecvT() GarbledTable {
	result := <-io.Tchan
	return result
}

func (io CircuitChans) RecvK() Key {
	result := <-io.Kchan
	return result
}

type Chanio struct {
	CircuitChans
	ot.NPChans
	ot.ExtChans
}

func NewChanio() (io *Chanio) {
	io = &Chanio{
		CircuitChans{
			make(chan GarbledTable, 50),
			make(chan Key, 50),
			make(chan Key, 50),
		},
		ot.NPChans{
			make(chan big.Int, 100),
			make(chan big.Int, 1),
			make(chan ot.HashedElGamalCiph, 100),
		},
		ot.ExtChans{
			make(chan []byte, 100),
			make(chan ot.Selector, 100),
		},
	}
	return io
}

// Stream OT version
type ClientAsSender struct {
	S2R chan ot.MessagePair `fatchan:"request"` // One per sender/receiver pair, sender->receiver
	R2S chan []byte         `fatchan:"reply"`   // One per sender/receiver pair, receiver->sender
}

type PerBlock struct {
	CAS ClientAsSender
	CircuitChans
}

type PerNodePair struct {
	ot.NPChans
	BlockChans []PerBlock
}
