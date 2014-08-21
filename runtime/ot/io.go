package ot

import "math/big"

type OTChans struct {
	NpSendPk   chan PublicKey         `fatchan:"reply"`
	NpRecvPk   chan big.Int           `fatchan:"request"`
	NpSendEncs chan HashedElGamalCiph `fatchan:"reply"`

	OtExtChan    chan []byte   `fatchan:"request"`
	OtExtSelChan chan Selector `fatchan:"reply"`

	ParamChan chan NPReceiverParams `fatchan:"reply"`
}
