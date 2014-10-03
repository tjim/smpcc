package ot

import "math/big"

type NPChans struct {
	ParamChan  chan big.Int           `fatchan:"reply"`
	NpRecvPk   chan big.Int           `fatchan:"request"`
	NpSendEncs chan HashedElGamalCiph `fatchan:"reply"`
}

type ExtChans struct {
	OtExtChan    chan []byte   `fatchan:"request"`
	OtExtSelChan chan Selector `fatchan:"reply"`
}

func NewOTChansSender(npchans NPChans, extchans ExtChans) Sender {
	C := <-npchans.ParamChan
	baseReceiver := NewNPReceiver(C, npchans.NpRecvPk, npchans.NpSendEncs)
	sender := NewExtendSender(extchans.OtExtChan, extchans.OtExtSelChan, baseReceiver, SEC_PARAM, NUM_PAIRS)
	return sender
}

func NewOTChansReceiver(npchans NPChans, extchans ExtChans) Receiver {
	C := GenNPParam()
	npchans.ParamChan <- C
	baseSender := NewNPSender(C, npchans.NpRecvPk, npchans.NpSendEncs)
	receiver := NewExtendReceiver(extchans.OtExtChan, extchans.OtExtSelChan, baseSender, SEC_PARAM, NUM_PAIRS)
	return receiver
}
