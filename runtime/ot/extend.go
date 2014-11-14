package ot

// extend.go
//
// Extending Oblivious Transfers Efficiently
// Yuval Ishai, Joe Kilian, Kobbi Nissim, Erez Petrank
// CRYPTO 2003
// http://link.springer.com/chapter/10.1007/978-3-540-45146-4_9
//
// Modified with preprocessing step

import (
	"crypto/rand"
	"fmt"
	"github.com/tjim/smpcc/runtime/bit"
	"golang.org/x/crypto/sha3"
	"io"
)

type ExtendSender struct {
	R            Receiver
	z0, z1       [][]byte
	m            int
	k            int
	otExtChan    chan []byte
	otExtSelChan chan Selector
	curPair      int
	started      bool
	sendCalls    int
}

type ExtendReceiver struct {
	S            Sender
	r            []byte
	m            int
	k            int
	otExtChan    chan []byte
	otExtSelChan chan Selector
	curPair      int
	T            *bit.Matrix8
}

func NewExtendSender(c chan []byte, otExtSelChan chan Selector, R Receiver, k, m int) Sender {
	if k%8 != 0 {
		panic("k must be a multiple of 8")
	}
	if m%8 != 0 {
		panic("m must be a multiple of 8")
	}
	sender := new(ExtendSender)
	sender.otExtSelChan = otExtSelChan
	sender.k = k
	sender.R = R
	sender.otExtChan = c
	sender.m = m
	sender.curPair = m
	sender.started = false
	sender.sendCalls = 0
	return sender
}

func NewExtendReceiver(c chan []byte, otExtSelChan chan Selector, S Sender, k, m int) Receiver {
	if k%8 != 0 {
		panic("k must be a multiple of 8")
	}
	if m%8 != 0 {
		panic("m must be a multiple of 8")
	}
	receiver := new(ExtendReceiver)
	receiver.otExtSelChan = otExtSelChan
	receiver.k = k
	receiver.S = S
	receiver.m = m
	receiver.curPair = m
	receiver.otExtChan = c
	return receiver
}

func (self *ExtendSender) preProcessSender(m int) {
	if m%8 != 0 {
		panic("m must be a multiple of 8")
	}
	self.started = true
	self.m = m
	self.curPair = 0
	s := make([]byte, self.k/8)
	randomBitVector(s)

	QT := bit.NewMatrix8(self.k, self.m)
	for i := 0; i < QT.NumRows; i++ {
		recvd := self.R.Receive(Selector(bit.GetBit(s, i)))
		if len(recvd) != self.m/8 {
			panic(fmt.Sprintf("Incorrect column length received: %d != %d", len(recvd), self.m/8))
		}
		QT.SetRow(i, recvd)
	}
	Q := QT.Transpose()
	self.z0 = make([][]byte, m)
	self.z1 = make([][]byte, m)
	temp := make([]byte, self.k/8)
	for j := 0; j < m; j++ {
		self.z0[j] = Q.GetRow(j)
		xorBytes(temp, Q.GetRow(j), s)
		self.z1[j] = make([]byte, len(temp))
		copy(self.z1[j], temp)
	}
}

func (self *ExtendReceiver) preProcessReceiver(m int) {
	self.curPair = 0
	self.m = m
	self.r = make([]byte, self.m/8)
	randomBitVector(self.r)
	T := bit.NewMatrix8(self.m, self.k)
	T.Randomize()
	self.T = T
	TT := T.Transpose()
	temp := make([]byte, self.m/8)
	for i := 0; i < self.k; i++ {
		xorBytes(temp, self.r, TT.GetRow(i))
		self.S.Send(TT.GetRow(i), temp)
	}
}

// hash function instantiating a random oracle
func RO(input []byte, outBits int) []byte {
	if outBits <= 0 {
		panic("output size <= 0")
	}
	if outBits%8 != 0 {
		panic("output size must be a multiple of 8")
	}
	output := make([]byte, outBits/8)
	sha3.ShakeSum256(output, input)
	return output
}

func (self *ExtendSender) Send(m0, m1 Message) {
	if self.curPair == self.m {
		self.preProcessSender(self.m)
	}
	if len(m0) != len(m1) {
		panic("(*ot.ExtendSender).Send: messages have different lengths")
	}
	msglen := len(m0)
	y0 := make([]byte, msglen)
	y1 := make([]byte, msglen)
	smod := <-self.otExtSelChan
	if smod == 0 {
		xorBytes(y0, m0, RO(self.z0[self.curPair], 8*msglen))
		xorBytes(y1, m1, RO(self.z1[self.curPair], 8*msglen))
	} else if smod == 1 {
		xorBytes(y0, m1, RO(self.z0[self.curPair], 8*msglen))
		xorBytes(y1, m0, RO(self.z1[self.curPair], 8*msglen))
	} else {
		panic("Sender: unexpected smod value")
	}
	self.otExtChan <- y0
	self.otExtChan <- y1
	self.curPair++
	return
}

func (self *ExtendReceiver) Receive(s Selector) Message {
	if self.curPair == self.m {
		self.preProcessReceiver(self.m)
	}
	smod := Selector(byte(s) ^ bit.GetBit(self.r, self.curPair))
	self.otExtSelChan <- smod
	y0 := <-self.otExtChan
	y1 := <-self.otExtChan
	if len(y0) != len(y1) {
		panic("(*ot.ExtendReceiver).Receive: messages have different length")
	}
	msglen := len(y0)
	w := make([]byte, msglen)
	if bit.GetBit(self.r, self.curPair) == 0 {
		xorBytes(w, y0, RO(self.T.GetRow(self.curPair), 8*msglen))
	} else if bit.GetBit(self.r, self.curPair) == 1 {
		xorBytes(w, y1, RO(self.T.GetRow(self.curPair), 8*msglen))
	}
	self.curPair++
	return w
}

func randomBitVector(pool []byte) {
	n, err := io.ReadFull(rand.Reader, pool)
	if err != nil || n != len(pool) {
		panic("randomness allocation failed")
	}
}

// Send m message pairs in one call
func (S *ExtendSender) SendM(a, b []Message) {
	m := len(a)
	if m%8 != 0 {
		panic("SendM: must send a multiple of 8 messages at a time") // force compatibility with stream OT
	}
	if len(b) != m {
		panic("SendM: must send pairs of messages")
	}
	for i := range a {
		S.Send(a[i], b[i])
	}
}
func (R *ExtendReceiver) ReceiveM(r []byte) []Message { // r is a packed vector of selections
	result := make([]Message, 8*len(r))
	for i := range r {
		for bit := 0; bit < 8; bit++ {
			selector := Selector((r[i] >> uint(7-bit)) & 1)
			result[i+bit] = R.Receive(selector)
		}
	}
	return result
}

// Send m pairs of bits (1-bit messages) in one call
func (S *ExtendSender) SendMBits(a, b []byte) { // messages are packed in bytes
	m := 8 * len(a)
	if 8*len(b) != m {
		panic("SendMBits: must send pairs of messages")
	}
	for i := range a {
		for bit := 0; bit < 8; bit++ {
			mask := byte(0x80 >> uint(bit))
			S.Send([]byte{a[i] & mask}, []byte{b[i] & mask})
		}
	}
}
func (R *ExtendReceiver) ReceiveMBits(r []byte) []byte { // r is a packed vector of selections and result is packed as well
	result := make([]byte, len(r))
	for i := range r {
		for bit := 0; bit < 8; bit++ {
			mask := byte(0x80 >> uint(bit))
			selector := Selector((r[i] >> uint(7-bit)) & 1)
			result[i] |= mask & R.Receive(selector)[0]
		}
	}
	return result
}
