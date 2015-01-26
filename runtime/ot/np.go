package ot

// np.go
// Naor-Pinkas oblivious transfer
//
// Efficient oblivious transfer protocols
// Moni Naor and Benny Pinkas
// SODA 2001
// https://dl.acm.org/citation.cfm?id=365502
//
// This is the "basic oblivious transfer protocol" of section 2.3

import (
	"crypto/aes"
	"log"
	"math/big"
	"time"
)

const (
	KEY_SIZE     = aes.BlockSize
	NUM_PAIRS    = 1024 * 64
	SEC_PARAM    = 80
	primeHex     = "B10B8F96A080E01DDE92DE5EAE5D54EC52C99FBCFB06A3C69A6A9DCA52D23B616073E28675A23D189838EF1E2EE652C013ECB4AEA906112324975C3CD49B83BFACCBDD7D90C4BD7098488E9C219A73724EFFD6FAE5644738FAA31A4FF55BCCC0A151AF5F0DC8B4BD45BF37DF365C1A65E68CFDA76D4DA708DF1FB2BC2E4A4371"
	generatorHex = "A4D1CBD5C3FD34126765A442EFB99905F8104DD258AC507FD6406CFF14266D31266FEA1E5C41564B777E690F5504F213160217B4B01B886A5E91547F9E2749F4D7FBD7D3B9A92EE1909D0D2263F80A76A6A24C087A091F531DBF0A0169B6A28AD662A4D18E73AFA32D779D5918D08BC8858F4DCEF97C2A24855E6EEB22B3B2E5"
)

type HashedElGamalCiph struct {
	C0 *big.Int
	C1 []byte
}

type NPSender struct {
	npRecvPk   chan *big.Int
	npSendEncs chan HashedElGamalCiph
	npC        chan *big.Int
	C          *big.Int
}

type NPReceiver struct {
	npRecvPk   chan *big.Int
	npSendEncs chan HashedElGamalCiph
	npC        chan *big.Int
	C          *big.Int
}

var publicParams = PublicKey{G: fromHex(generatorHex), P: fromHex(primeHex)}

func timeTrack(start time.Time, name string) {
	elapsed := time.Since(start)
	log.Printf("%s took %s", name, elapsed)
}

func GenNPParam() *big.Int {
	X := generateNumNonce(publicParams.P)
	C := new(big.Int).Exp(publicParams.G, X, publicParams.P)
	return C
}

func NewNPSender(npC chan *big.Int,
	npRecvPk chan *big.Int,
	npSendEncs chan HashedElGamalCiph) *NPSender {

	sender := new(NPSender)
	sender.npRecvPk = npRecvPk
	sender.npSendEncs = npSendEncs
	sender.npC = npC

	return sender
}

func NewNPReceiver(npC chan *big.Int,
	npRecvPk chan *big.Int,
	npSendEncs chan HashedElGamalCiph) *NPReceiver {

	receiver := new(NPReceiver)
	receiver.npRecvPk = npRecvPk
	receiver.npSendEncs = npSendEncs
	receiver.npC = npC

	return receiver
}

func NewNP() (*NPSender, *NPReceiver) {
	npC := make(chan *big.Int)
	npRecvPk := make(chan *big.Int)
	npSendEncs := make(chan HashedElGamalCiph)

	return NewNPSender(npC, npRecvPk, npSendEncs),
		NewNPReceiver(npC, npRecvPk, npSendEncs)
}

func (self *NPSender) Send(m0, m1 Message) {
	//	log.Println("Starting run of OTNPSender.Send")
	if len(m0) != len(m1) {
		panic("(*ot.NPSender).Send: messages have different lengths")
	}
	if self.npC != nil {
		self.C = GenNPParam()
		self.npC <- self.C
		self.npC = nil
	}
	msglen := len(m0)
	pks := make([]*big.Int, 2)
	pks[0] = <-self.npRecvPk
	pks[1] = new(big.Int).ModInverse(pks[0], publicParams.P)
	pks[1].Mul(pks[1], self.C).Mod(pks[1], publicParams.P)
	r0 := generateNumNonce(publicParams.P)
	r1 := generateNumNonce(publicParams.P)

	maskedVal0 := make([]byte, msglen)
	xorBytes(maskedVal0, RO(expModP(pks[0], r0).Bytes(), 8*msglen), m0)
	maskedVal1 := make([]byte, msglen)
	xorBytes(maskedVal1, RO(expModP(pks[1], r1).Bytes(), 8*msglen), m1)
	e0 := HashedElGamalCiph{C0: gExpModP(r0), C1: maskedVal0}
	e1 := HashedElGamalCiph{C0: gExpModP(r1), C1: maskedVal1}
	self.npSendEncs <- e0
	self.npSendEncs <- e1
	//	log.Println("Completed run of OTNPSender.Send")
}

func (self *NPReceiver) Receive(s Selector) Message {
	//	log.Printf("Starting run of OTNPSender.Receive.\n")
	if self.npC != nil {
		self.C = <-self.npC
		self.npC = nil
	}
	pks := make([]*big.Int, 2)
	k := generateNumNonce(publicParams.P)
	pks[s] = gExpModP(k)
	pks[1-s] = new(big.Int).ModInverse(pks[s], publicParams.P)
	pks[1-s].Mul(pks[1-s], self.C).Mod(pks[1-s], publicParams.P)
	self.npRecvPk <- pks[0]
	ciphs := []HashedElGamalCiph{<-self.npSendEncs, <-self.npSendEncs}
	if len(ciphs[0].C1) != len(ciphs[1].C1) {
		panic("(*ot.NPReceiver).Receive: messages have different lengths")
	}
	msglen := len(ciphs[0].C1)
	res := make([]byte, msglen)
	xorBytes(res, RO(expModP(ciphs[s].C0, k).Bytes(), 8*msglen), ciphs[s].C1)
	//	log.Printf("Completed run of OTNPSender.Receive. len(res)=%d\n", len(res))
	return res
}

// Send m message pairs in one call
func (S *NPSender) SendM(a, b []Message) {
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
func (R *NPReceiver) ReceiveM(r []byte) []Message { // r is a packed vector of selections
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
func (S *NPSender) SendMBits(a, b []byte) { // messages are packed in bytes
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
func (R *NPReceiver) ReceiveMBits(r []byte) []byte { // r is a packed vector of selections and result is packed as well
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

func xorBytes(a, b, c []byte) {
	if len(a) != len(b) || len(b) != len(c) {
		panic("xorBytes: length mismatch")
	}
	for i := range a {
		a[i] = b[i] ^ c[i]
	}
}

func expModP(g, x *big.Int) *big.Int {
	return new(big.Int).Exp(g, x, publicParams.P)
}

func gExpModP(x *big.Int) *big.Int {
	return expModP(publicParams.G, x)
}

func fromHex(hex string) *big.Int {
	n, ok := new(big.Int).SetString(hex, 16)
	if !ok {
		panic("failed to parse hex number")
	}
	return n
}
