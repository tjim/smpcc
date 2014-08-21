package gc

import (
	//	"log"

	"math/big"

	"github.com/tjim/smpcc/runtime/ot"
)

type Genio interface {
	ot.Sender
	SendT(t GarbledTable)
	SendK(t Key)
	RecvK2() Key
}

type Evalio interface {
	ot.Receiver
	RecvT() GarbledTable
	RecvK() Key
	SendK2(t Key)
}

// **********************************************
type Chanio struct {
	Tchan   chan GarbledTable `fatchan:"request"`
	Kchan   chan Key          `fatchan:"request"`
	Kchan2  chan Key          `fatchan:"reply"`
	OtChans ot.OTChans
}

type GenX struct {
	channels *Chanio
	otSender ot.Sender
}

type EvalX struct {
	channels *Chanio
	otRecvr  ot.Receiver
}

func NewGenX(io *Chanio) *GenX {
	//	log.Printf("rec params")
	rcvParams := <-io.OtChans.ParamChan
	//	log.Printf("done rec params")

	npRecvr := ot.NewNPReceiver(rcvParams, io.OtChans.NpSendPk, io.OtChans.NpRecvPk, io.OtChans.NpSendEncs)
	otSender := ot.NewExtendSender(io.OtChans.OtExtChan, io.OtChans.OtExtSelChan, npRecvr, ot.SEC_PARAM, ot.NUM_PAIRS)

	result := &GenX{
		io,
		otSender,
	}
	return result
}

func NewEvalX(io *Chanio) *EvalX {
	sndParams, rcvParams := ot.GenNPParams()
	//	log.Printf("send params")
	io.OtChans.ParamChan <- rcvParams
	//	log.Printf("done send params")

	npSndr := ot.NewNPSender(sndParams, io.OtChans.NpSendPk, io.OtChans.NpRecvPk, io.OtChans.NpSendEncs)
	otRecvr := ot.NewExtendReceiver(io.OtChans.OtExtChan, io.OtChans.OtExtSelChan, npSndr, ot.SEC_PARAM, ot.NUM_PAIRS)

	result := &EvalX{
		io,
		otRecvr,
	}
	return result
}

func (io *GenX) SendT(x GarbledTable) {
	io.channels.Tchan <- x
}

func (io *GenX) SendK(x Key) {
	io.channels.Kchan <- x
}

func (io *EvalX) SendK2(x Key) {
	io.channels.Kchan2 <- x
}

func (io *EvalX) RecvT() GarbledTable {
	result := <-io.channels.Tchan
	return result
}

func (io *EvalX) RecvK() Key {
	result := <-io.channels.Kchan
	return result
}

func (io *GenX) RecvK2() Key {
	result := <-io.channels.Kchan2
	return result
}

func (io *GenX) Send(m0, m1 ot.Message) {
	io.otSender.Send(m0, m1)
}

func (io *EvalX) Receive(s ot.Selector) ot.Message {
	return io.otRecvr.Receive(s)
}

func NewChanio() (io *Chanio) {
	io = &Chanio{
		make(chan GarbledTable, 50),
		make(chan Key, 50),
		make(chan Key, 50),
		ot.OTChans{
			make(chan ot.PublicKey, 100),
			make(chan big.Int, 100),
			make(chan ot.HashedElGamalCiph, 100),

			make(chan []byte, 100),
			make(chan ot.Selector, 100),
			make(chan ot.NPReceiverParams, 1),
		},
	}
	return io
}
