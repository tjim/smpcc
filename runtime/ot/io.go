package ot

import "math/big"

type OTChans struct {
	NpRecvPk     chan big.Int           `fatchan:"request"`
	NpSendEncs   chan HashedElGamalCiph `fatchan:"reply"`
	OtExtChan    chan []byte            `fatchan:"request"`
	OtExtSelChan chan Selector          `fatchan:"reply"`
	ParamChan    chan big.Int           `fatchan:"reply"`
}

func NewOTChansSender(otchans *OTChans) Sender {
	C := <-otchans.ParamChan
	baseReceiver := NewNPReceiver(C, otchans.NpRecvPk, otchans.NpSendEncs)
	sender := NewExtendSender(otchans.OtExtChan, otchans.OtExtSelChan, baseReceiver, SEC_PARAM, NUM_PAIRS)
	return sender
}

func NewOTChansReceiver(otchans *OTChans) Receiver {
	C := GenNPParam()
	otchans.ParamChan <- C
	baseSender := NewNPSender(C, otchans.NpRecvPk, otchans.NpSendEncs)
	receiver := NewExtendReceiver(otchans.OtExtChan, otchans.OtExtSelChan, baseSender, SEC_PARAM, NUM_PAIRS)
	return receiver
}
