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
	"io"

	"bitbucket.org/ede/sha3"
	"github.com/tjim/smpcc/runtime/bit"
)

type ExtendSender struct {
	R            Receiver
	z0, z1       [][]byte
	m            int
	k            int
	otExtChan    chan []byte
	otExtSelChan chan Selector
	l            int
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
	l            int
	curPair      int
	T            *bit.Matrix8
}

func NewExtendSender(c chan []byte, otExtSelChan chan Selector, R Receiver, k, l, m int) Sender {
	if k%8 != 0 {
		panic("k must be a multiple of 8")
	}
	if l%8 != 0 {
		panic("l must be a multiple of 8")
	}
	if m%8 != 0 {
		panic("m must be a multiple of 8")
	}
	sender := new(ExtendSender)
	sender.otExtSelChan = otExtSelChan
	sender.k = k
	sender.l = l
	sender.R = R
	sender.otExtChan = c
	sender.m = m
	sender.curPair = m
	sender.started = false
	sender.sendCalls = 0
	return sender
}

func NewExtendReceiver(c chan []byte, otExtSelChan chan Selector, S Sender, k, l, m int) Receiver {
	if k%8 != 0 {
		panic("k must be a multiple of 8")
	}
	if l%8 != 0 {
		panic("l must be a multiple of 8")
	}
	if m%8 != 0 {
		panic("m must be a multiple of 8")
	}
	receiver := new(ExtendReceiver)
	receiver.otExtSelChan = otExtSelChan
	receiver.k = k
	receiver.l = l
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
	// fmt.Printf("pre-processing sender: start\n")
	self.started = true
	self.m = m
	//	log.Printf("preProcessSender: m=%d", m)
	self.curPair = 0
	s := make([]byte, self.k/8)
	randomBitVector(s)

	QT := bit.NewMatrix8(self.k, self.m)
	for i := 0; i < QT.NumRows; i++ {
		recvd := self.R.Receive(Selector(bit.GetBit(s, i)))
		//		log.Printf("preProcessSender: len(recvd)=%d", len(recvd))
		if len(recvd) != self.m/8 {
			panic(fmt.Sprintf("Incorrect column length received: %d != %d", len(recvd), self.m/8))
		}
		QT.SetRow(i, recvd)
	}
	Q := QT.Transpose()
	self.z0 = make([][]byte, m)
	// fmt.Printf("pre-processing sender: len(z0)=%d\n", len(self.z0))
	self.z1 = make([][]byte, m)
	temp := make([]byte, self.k/8)
	for j := 0; j < m; j++ {
		self.z0[j] = Q.GetRow(j)
		xorBytesExact(temp, Q.GetRow(j), s)
		self.z1[j] = make([]byte, len(temp))
		copy(self.z1[j], temp)
	}

}

func (self *ExtendReceiver) preProcessReceiver(m int) {
	// fmt.Printf("pre-processing receiver\n")
	self.curPair = 0
	self.m = m
	//	log.Printf("preProcessReceiver: m=%d", m)
	self.r = make([]byte, self.m/8)
	for i := range self.r { // TJIM: Since we randomize isn't this unnecessary?
		self.r[i] = byte(255)
	}
	randomBitVector(self.r)
	//	fmt.Printf("preProcessReceiver: len(self.r)=%d\n", len(self.r))
	T := bit.NewMatrix8(self.m, self.k)
	T.Randomize()
	TT := T.Transpose()
	self.T = T
	in1 := make([][]byte, self.k) // TJIM: no need to keep in[i] around after Send, just compute on demand
	for i := range in1 {
		in1[i] = make([]byte, self.m/8)
		xorBytesExact(in1[i], self.r, TT.GetRow(i))
	}
	for i := 0; i < self.k; i++ {
		self.S.Send(TT.GetRow(i), in1[i])
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
	h := sha3.NewCipher(input, nil)
	output := make([]byte, outBits/8)
	h.XORKeyStream(output, output)
	return output
}

func (self *ExtendSender) Send(m0, m1 Message) {
	// self.sendCalls++
	if self.curPair == self.m {
		self.preProcessSender(self.m)
	}
	if len(m0) != len(m1) {
		panic("(*ot.ExtendSender).Send: messages have different lengths")
	}
	msglen := len(m0)
	y0 := make([]byte, msglen)
	y1 := make([]byte, msglen)
	// fmt.Printf("Send: self.l=%d, self.l%%8=%d, len(y0)=%d, len(y1)=%d\n", self.l, self.l/8, len(y0), len(y1))
	smod := <-self.otExtSelChan
	// log.Printf("Send: self.curPair=%d, len(z0)=%d, smod=%d, m=%d, started=%v, sendCalls=%d\n", self.curPair, len(self.z0), smod, self.m,
	// self.started, self.sendCalls)
	// fmt.Printf("Send: z0=%v\nSend: z1=%v\n", self.z0[self.curPair], self.z1[self.curPair])
	if smod == 0 {
		// fmt.Println("smod = 0")
		xorBytesExact(y0, m0, RO(self.z0[self.curPair], 8*msglen))
		xorBytesExact(y1, m1, RO(self.z1[self.curPair], 8*msglen))
	} else if smod == 1 {
		// fmt.Println("smod = 1")
		xorBytesExact(y0, m1, RO(self.z0[self.curPair], 8*msglen))
		xorBytesExact(y1, m0, RO(self.z1[self.curPair], 8*msglen))
	} else {
		panic("Sender: unexpected smod value")
	}
	// fmt.Printf("Send: self.z0[%d]=%v self.z1[%d]=%v\n",self.curPair,self.z0[self.curPair],self.curPair,self.z1[self.curPair])
	// fmt.Printf("Send:    y0=%v y1=%v\n",y0,y1)
	// fmt.Printf("Send:    m0=%v m1=%v\n",m0,m1)
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
	// fmt.Printf("receive smod = %d\n", smod)
	//	log.Printf("Receive: self.curPair=%d, len(self.r)=%d\n", self.curPair, len(self.r))
	self.otExtSelChan <- smod
	y0 := <-self.otExtChan
	y1 := <-self.otExtChan
	if len(y0) != len(y1) {
		panic("(*ot.ExtendReceiver).Receive: messages have different length")
	}
	msglen := len(y0)
	w := make([]byte, msglen)
	// fmt.Printf("Receive: Trow=%v\n", self.T.GetRow(self.curPair))
	if bit.GetBit(self.r, self.curPair) == 0 {
		xorBytesExact(w, y0, RO(self.T.GetRow(self.curPair), 8*msglen))
	} else if bit.GetBit(self.r, self.curPair) == 1 {
		xorBytesExact(w, y1, RO(self.T.GetRow(self.curPair), 8*msglen))
	}
	self.curPair++
	return w
}

func xorBytesExact(a, b, c []byte) {
	if len(a) != len(b) || len(b) != len(c) {
		panic(fmt.Sprintf("xorBytesExact: wrong lengths (%d,%d,%d)\n", len(a), len(b), len(c)))
	}
	xorBytes(a, b, c)
}

func randomBitVector(pool []byte) {
	n, err := io.ReadFull(rand.Reader, pool)
	if err != nil || n != len(pool) {
		panic("randomness allocation failed")
	}
}
