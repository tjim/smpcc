package ot

import (
	"fmt"
	"github.com/tjim/smpcc/runtime/bit"
)

type getRequest struct {
	index  int
	result chan []byte
}

type sendRequest struct {
	s     Selector
	index int
}

// Channels needed for multiplex OT to function
// In gc case, generator is client and sender, evaluator is server and receiver
// We send an MplexChans from client to server, so fatchan notations are relative to generator/client/sender
type MplexChans struct {
	refreshCh chan int         `fatchan:"reply"`   // One per SET of multiplexed sender/receiver, receiver->sender
	repCh     chan []byte      `fatchan:"request"` // One per sender/receiver pair, sender->receiver
	reqCh     chan sendRequest `fatchan:"reply"`   // One per sender/receiver pair, receiver->sender
}

type MplexSender struct {
	repCh chan []byte
	reqCh chan sendRequest // for receiving send requests from MplexReceiver
	getCh chan getRequest  // for making get requests
}

type nextRequest struct {
	r     byte
	row   []byte
	index int
}

type MplexReceiver struct {
	repCh  chan []byte
	reqCh  chan sendRequest // for sending send requests to MplexSender
	nextCh chan nextRequest
}

func NewMplexSender(c chan []byte, reqCh chan sendRequest, getCh chan getRequest) Sender {
	sender := new(MplexSender)
	sender.reqCh = reqCh
	sender.repCh = c
	sender.getCh = getCh
	return sender
}

func NewMplexReceiver(c chan []byte, reqCh chan sendRequest, nextCh chan nextRequest) Receiver {
	receiver := new(MplexReceiver)
	receiver.reqCh = reqCh
	receiver.repCh = c
	receiver.nextCh = nextCh
	return receiver
}

func PrimarySender(R Receiver, refreshCh chan int, k, m int) chan getRequest {
	if k%8 != 0 {
		panic("k must be a multiple of 8")
	}
	if m%8 != 0 {
		panic("m must be a multiple of 8")
	}
	getCh := make(chan getRequest)
	go func() {
		z := make(map[int][][]byte)
		s := make([]byte, k/8)
		for {
			select {
			case index := <-refreshCh:
				randomBitVector(s)
				QT := bit.NewMatrix8(k, m)
				for i := 0; i < QT.NumRows; i++ {
					recvd := R.Receive(Selector(bit.GetBit(s, i)))
					if len(recvd) != m/8 {
						panic(fmt.Sprintf("Incorrect column length received: %d != %d", len(recvd), m/8))
					}
					QT.SetRow(i, recvd)
				}
				Q := QT.Transpose()
				for j := 0; j < m; j++ {
					z0 := make([]byte, k/8)
					copy(z0, Q.GetRow(j))
					z1 := make([]byte, k/8)
					xorBytes(z1, z0, s)
					z[j+index] = make([][]byte, 2)
					z[j+index][0] = z0
					z[j+index][1] = z1
				}
			case getReq := <-getCh:
				value, ok := z[getReq.index]
				if ok {
					delete(z, getReq.index)
				} else {
					panic(fmt.Sprintf("resource not found %d", getReq.index))
				}
				getReq.result <- value[0]
				getReq.result <- value[1]
			}
		}
	}()
	return getCh
}

func (self *MplexSender) Send(m0, m1 Message) {
	if len(m0) != len(m1) {
		panic("(*ot.ExtendSender).Send: messages have different lengths")
	}
	msglen := len(m0)
	y0 := make([]byte, msglen)
	y1 := make([]byte, msglen)
	sendReq := <-self.reqCh
	var getReq getRequest
	getReq.index = sendReq.index
	getReq.result = make(chan []byte)
	self.getCh <- getReq
	z0 := <-getReq.result
	z1 := <-getReq.result
	smod := sendReq.s
	if smod == 0 {
		xorBytes(y0, m0, RO(z0, 8*msglen))
		xorBytes(y1, m1, RO(z1, 8*msglen))
	} else if smod == 1 {
		xorBytes(y0, m1, RO(z0, 8*msglen))
		xorBytes(y1, m0, RO(z1, 8*msglen))
	} else {
		panic("Sender: unexpected smod value")
	}
	self.repCh <- y0
	self.repCh <- y1
	return
}

func PrimaryReceiver(S Sender, refreshCh chan int, k, m int) chan nextRequest {
	if k%8 != 0 {
		panic("k must be a multiple of 8")
	}
	if m%8 != 0 {
		panic("m must be a multiple of 8")
	}
	nextCh := make(chan nextRequest)
	go func() {
		curPair := 0
		r := make([]byte, m/8)
		var T *bit.Matrix8
		for {
			if curPair%m == 0 {
				T = bit.NewMatrix8(m, k) // Create a new T, don't re-use, to avoid race
				refreshCh <- curPair
				randomBitVector(r)
				T.Randomize()
				TT := T.Transpose()
				temp := make([]byte, m/8)
				for i := 0; i < k; i++ {
					xorBytes(temp, r, TT.GetRow(i))
					S.Send(TT.GetRow(i), temp)
				}
			}
			var req nextRequest
			req.r = bit.GetBit(r, curPair%m)
			req.row = T.GetRow(curPair % m) // req.row points into T, so don't re-use T (above) to avoid race
			req.index = curPair
			nextCh <- req
			curPair++
		}
	}()
	return nextCh
}

func (self *MplexReceiver) Receive(s Selector) Message {
	nextReq := <-self.nextCh
	smod := Selector(byte(s) ^ nextReq.r)
	var sendReq sendRequest
	sendReq.s = smod
	sendReq.index = nextReq.index
	self.reqCh <- sendReq
	y0 := <-self.repCh
	y1 := <-self.repCh
	if len(y0) != len(y1) {
		panic("(*ot.ExtendReceiver).Receive: messages have different length")
	}
	msglen := len(y0)
	w := make([]byte, msglen)
	if nextReq.r == 0 {
		xorBytes(w, y0, RO(nextReq.row, 8*msglen))
	} else if nextReq.r == 1 {
		xorBytes(w, y1, RO(nextReq.row, 8*msglen))
	}
	return w
}
