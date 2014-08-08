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
	//	"log"
	"crypto/aes"
	"math/big"
)

const (
	KEY_SIZE     = aes.BlockSize
	HASH_SIZE    = 32
	MSG_LEN      = KEY_SIZE * 8
	NUM_PAIRS    = 1024 * 64
	NP_MSG_LEN   = NUM_PAIRS / 8
	SEC_PARAM    = 80
	primeHex     = "B10B8F96A080E01DDE92DE5EAE5D54EC52C99FBCFB06A3C69A6A9DCA52D23B616073E28675A23D189838EF1E2EE652C013ECB4AEA906112324975C3CD49B83BFACCBDD7D90C4BD7098488E9C219A73724EFFD6FAE5644738FAA31A4FF55BCCC0A151AF5F0DC8B4BD45BF37DF365C1A65E68CFDA76D4DA708DF1FB2BC2E4A4371"
	generatorHex = "A4D1CBD5C3FD34126765A442EFB99905F8104DD258AC507FD6406CFF14266D31266FEA1E5C41564B777E690F5504F213160217B4B01B886A5E91547F9E2749F4D7FBD7D3B9A92EE1909D0D2263F80A76A6A24C087A091F531DBF0A0169B6A28AD662A4D18E73AFA32D779D5918D08BC8858F4DCEF97C2A24855E6EEB22B3B2E5"
)

type HashedElGamalCiph struct {
	C0 big.Int
	C1 []byte
}

type NPSender struct {
	npSendPk   chan PublicKey
	npRecvPk   chan big.Int
	npSendEncs chan HashedElGamalCiph
	NPSenderParams
}

type NPSenderParams struct {
	pri    *PrivateKey
	MsgLen int
}

type NPReceiver struct {
	npSendPk   chan PublicKey
	npRecvPk   chan big.Int
	npSendEncs chan HashedElGamalCiph
	NPReceiverParams
}

type NPReceiverParams struct {
	PkSender PublicKey
	MsgLen   int
}

func GenNPParams(MsgLen int) (snd NPSenderParams, rcv NPReceiverParams) {
	pri := new(PrivateKey)
	pri.PublicKey = publicParams
	pri.X = generateNumNonce(publicParams.P)
	pri.Y = new(big.Int).Exp(pri.G, pri.X, pri.P)
	snd.pri = pri
	snd.MsgLen = MsgLen
	rcv.PkSender = pri.PublicKey
	rcv.MsgLen = MsgLen
	return
}

func NewNPSender(MsgLen int,
	snd NPSenderParams,
	npSendPk chan PublicKey,
	npRecvPk chan big.Int,
	npSendEncs chan HashedElGamalCiph) *NPSender {

	sender := new(NPSender)
	sender.npSendPk = npSendPk
	sender.npRecvPk = npRecvPk
	sender.npSendEncs = npSendEncs
	sender.NPSenderParams = snd

	return sender
}

func NewNPReceiver(MsgLen int, rcv NPReceiverParams,
	npSendPk chan PublicKey,
	npRecvPk chan big.Int,
	npSendEncs chan HashedElGamalCiph) *NPReceiver {

	receiver := new(NPReceiver)
	receiver.npSendPk = npSendPk
	receiver.npRecvPk = npRecvPk
	receiver.npSendEncs = npSendEncs
	receiver.NPReceiverParams = rcv

	return receiver
}

func NewNP(MsgLen int) (*NPSender, *NPReceiver) {
	snd, rcv := GenNPParams(MsgLen)
	npSendPk := make(chan PublicKey)
	npRecvPk := make(chan big.Int)
	npSendEncs := make(chan HashedElGamalCiph)

	return NewNPSender(MsgLen, snd, npSendPk, npRecvPk, npSendEncs),
		NewNPReceiver(MsgLen, rcv, npSendPk, npRecvPk, npSendEncs)
}

func (self *NPSender) Send(m0, m1 Message) {
	//	log.Println("Starting run of OTNPSender.Send")
	pks := make([]big.Int, 2)
	pks[0] = <-self.npRecvPk
	pks[1].ModInverse(&pks[0], publicParams.P)
	pks[1].Mul(&pks[1], self.pri.PublicKey.Y).Mod(&pks[1], publicParams.P)
	r0 := generateNumNonce(publicParams.P)
	r1 := generateNumNonce(publicParams.P)

	maskedVal0 := make([]byte, len(m0))
	xorBytes(maskedVal0, RO(expModP(&pks[0], r0).Bytes(), 8*len(m0)), m0)
	maskedVal1 := make([]byte, len(m0))
	xorBytes(maskedVal1, RO(expModP(&pks[1], r1).Bytes(), 8*len(m0)), m1)
	e0 := HashedElGamalCiph{C0: *gExpModP(r0), C1: maskedVal0}
	e1 := HashedElGamalCiph{C0: *gExpModP(r1), C1: maskedVal1}
	self.npSendEncs <- e0
	self.npSendEncs <- e1
	//	log.Println("Completed run of OTNPSender.Send")
}

func (self *NPReceiver) Receive(s Selector) Message {
	//	log.Printf("Starting run of OTNPSender.Receive. self.MsgLen=%d\n", self.MsgLen)
	pks := make([]*big.Int, 2)
	k := generateNumNonce(publicParams.P)
	pks[s] = gExpModP(k)
	pks[1-s] = new(big.Int).ModInverse(pks[s], publicParams.P)
	pks[1-s].Mul(pks[1-s], self.PkSender.Y).Mod(pks[1-s], publicParams.P)
	self.npRecvPk <- *pks[0]
	ciphs := []HashedElGamalCiph{<-self.npSendEncs, <-self.npSendEncs}
	res := make([]byte, self.MsgLen)
	xorBytes(res, RO(expModP(&ciphs[s].C0, k).Bytes(), 8*self.MsgLen), ciphs[s].C1)
	//	log.Printf("Completed run of OTNPSender.Receive. len(res)=%d\n", len(res))
	return res
}

func xorBytes(a, b, c []byte) {
	for i := range a {
		switch {
		case i < len(b) && i < len(c):
			a[i] = b[i] ^ c[i]
		case i < len(b):
			a[i] = b[i]
		case i < len(c):
			a[i] = c[i]
		}
	}
}

func expModP(g, x *big.Int) *big.Int {
	return new(big.Int).Exp(g, x, publicParams.P)
}

func gExpModP(x *big.Int) *big.Int {
	return expModP(publicParams.G, x)
}

var publicParams = PublicKey{G: fromHex(generatorHex), P: fromHex(primeHex)}

func fromHex(hex string) *big.Int {
	n, ok := new(big.Int).SetString(hex, 16)
	if !ok {
		panic("failed to parse hex number")
	}
	return n
}
