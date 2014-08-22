package ot

import "math/big"

type OTChans struct {
	NpSendPk     chan PublicKey         `fatchan:"reply"`
	NpRecvPk     chan big.Int           `fatchan:"request"`
	NpSendEncs   chan HashedElGamalCiph `fatchan:"reply"`
	OtExtChan    chan []byte            `fatchan:"request"`
	OtExtSelChan chan Selector          `fatchan:"reply"`
	ParamChan    chan NPReceiverParams  `fatchan:"reply"`
}

func NewOTChansSender(otchans *OTChans) Sender {
	rcvParams := <-otchans.ParamChan
	baseReceiver := NewNPReceiver(rcvParams, otchans.NpSendPk, otchans.NpRecvPk, otchans.NpSendEncs)
	sender := NewExtendSender(otchans.OtExtChan, otchans.OtExtSelChan, baseReceiver, SEC_PARAM, NUM_PAIRS)
	return sender
}

func NewOTChansReceiver(otchans *OTChans) Receiver {
	sndParams, rcvParams := GenNPParams()
	otchans.ParamChan <- rcvParams
	baseSender := NewNPSender(sndParams, otchans.NpSendPk, otchans.NpRecvPk, otchans.NpSendEncs)
	receiver := NewExtendReceiver(otchans.OtExtChan, otchans.OtExtSelChan, baseSender, SEC_PARAM, NUM_PAIRS)
	return receiver
}
