package gc

import (
	"github.com/tjim/smpcc/runtime/ot"
	"math/big"
)

type Chanio struct {
	Tchan   chan GarbledTable `fatchan:"request"`
	Kchan   chan Key          `fatchan:"request"`
	Kchan2  chan Key          `fatchan:"reply"`
	OtChans ot.OTChans
}

func NewChanio() (io *Chanio) {
	io = &Chanio{
		make(chan GarbledTable, 50),
		make(chan Key, 50),
		make(chan Key, 50),
		ot.OTChans{
			make(chan big.Int, 100),
			make(chan ot.HashedElGamalCiph, 100),

			make(chan []byte, 100),
			make(chan ot.Selector, 100),
			make(chan big.Int, 1),
		},
	}
	return io
}
