package ot

// simple.go
//
// Simple (but slow) oblivious transfer

import (
	"crypto/rand"
	"crypto/rsa"
	"errors"
	"fmt"
	"math/big"
)

const (
	RSA_KEYSIZE = 1024
	NONCE_SIZE  = 117 // size of public modulus (==128) - 11 == 117 is max size that can be applyPermed
)

type SimpleSender struct {
	otSendPk    chan rsa.PublicKey
	otSendNonce chan big.Int
	otSendByte  chan byte
	otRecv      chan big.Int
	otSize      chan int
	privatekey  *rsa.PrivateKey
}

type SimpleReceiver struct {
	otSendPk    chan rsa.PublicKey
	otSendNonce chan big.Int
	otSendByte  chan byte
	otRecv      chan big.Int
	otSize      chan int
	publickey   rsa.PublicKey
}

func NewSimple() (*SimpleSender, *SimpleReceiver) {
	sender := new(SimpleSender)
	sender.privatekey = generatePKPair(RSA_KEYSIZE)
	sender.otSendPk = make(chan rsa.PublicKey)
	sender.otSendNonce = make(chan big.Int)
	sender.otSendByte = make(chan byte)
	sender.otRecv = make(chan big.Int)
	sender.otSize = make(chan int)

	receiver := new(SimpleReceiver)
	receiver.publickey = sender.privatekey.PublicKey
	receiver.otSendPk = sender.otSendPk
	receiver.otSendNonce = sender.otSendNonce
	receiver.otSendByte = sender.otSendByte
	receiver.otRecv = sender.otRecv
	receiver.otSize = sender.otSize

	return sender, receiver
}

func (self SimpleSender) Send(a0, a1 Message) {
	size := len(a0)
	if size != len(a1) {
		panic(fmt.Sprintf("Send: messages have different sizes, %d bytes != %d bytes", len(a0), len(a1)))
	}
	self.otSendPk <- self.privatekey.PublicKey
	self.otSize <- size
	N := self.privatekey.PublicKey.N
	for i := 0; i < size; i++ {
		x0 := generateNumNonce(N)
		x1 := generateNumNonce(N)
		self.otSendNonce <- *x0
		self.otSendNonce <- *x1
		y := <-self.otRecv
		//fmt.Printf("Received y: %d\n", &y)
		k0 := new(big.Int)
		k1 := new(big.Int)
		SubMod(k0, &y, x0, N)
		SubMod(k1, &y, x1, N)

		z0, err := invertPerm(self.privatekey, k0)
		onError(err, "failed decrypt 0")
		z1, err := invertPerm(self.privatekey, k1)
		onError(err, "failed decrypt 1")

		//fmt.Printf("z0: %d\nz1: %d\n", z0, z1)

		m0 := a0[i] ^ z0.Bytes()[0]
		m1 := a1[i] ^ z1.Bytes()[0]
		self.otSendByte <- m0
		self.otSendByte <- m1
	}
}

func (self SimpleReceiver) Receive(b Selector) Message {
	pk := <-self.otSendPk
	size := <-self.otSize
	res := make(Message, size)
	for i := 0; i < size; i++ {
		x0 := <-self.otSendNonce
		x1 := <-self.otSendNonce
		k := generateNumNonce(pk.N)
		ciph := applyPerm(new(big.Int), &pk, k)
		// onError(err, "failed applyPerm")
		y := new(big.Int)
		if b == 0 {
			AddMod(y, ciph, &x0, pk.N)
		} else {
			AddMod(y, ciph, &x1, pk.N)
		}
		//fmt.Printf("y: %d\nk: %d\n", y, k)
		self.otRecv <- *y
		m0 := <-self.otSendByte
		m1 := <-self.otSendByte

		if b == 0 {
			res[i] = m0 ^ k.Bytes()[0]
		} else {
			res[i] = m1 ^ k.Bytes()[0]
		}
	}
	return res
}

func generatePKPair(keySize int) *rsa.PrivateKey {
	priv, err := rsa.GenerateKey(rand.Reader, keySize)
	onError(err, "failed to generate key")
	return priv
}

func applyPerm(c *big.Int, pub *rsa.PublicKey, input *big.Int) *big.Int {
	m := input

	e := big.NewInt(int64(pub.E))
	c.Exp(m, e, pub.N)
	out := c

	return out
}

func invertPerm(priv *rsa.PrivateKey, input *big.Int) (out *big.Int, err error) {
	var m *big.Int
	c := input
	// TODO(agl): can we get away with reusing blinds?
	if c.Cmp(priv.N) >= 0 {
		err = errors.New(fmt.Sprintf("Modulus is %d, c is %d", priv.N, c)) //rsa.ErrDecryption
		return
	}

	var ir *big.Int

	if priv.Precomputed.Dp == nil {
		m = new(big.Int).Exp(c, priv.D, priv.N)
	} else {
		// We have the precalculated values needed for the CRT.
		m = new(big.Int).Exp(c, priv.Precomputed.Dp, priv.Primes[0])
		m2 := new(big.Int).Exp(c, priv.Precomputed.Dq, priv.Primes[1])
		m.Sub(m, m2)
		if m.Sign() < 0 {
			m.Add(m, priv.Primes[0])
		}
		m.Mul(m, priv.Precomputed.Qinv)
		m.Mod(m, priv.Primes[0])
		m.Mul(m, priv.Primes[1])
		m.Add(m, m2)

		for i, values := range priv.Precomputed.CRTValues {
			prime := priv.Primes[2+i]
			m2.Exp(c, values.Exp, prime)
			m2.Sub(m2, m)
			m2.Mul(m2, values.Coeff)
			m2.Mod(m2, prime)
			if m2.Sign() < 0 {
				m2.Add(m2, prime)
			}
			m2.Mul(m2, values.R)
			m.Add(m, m2)
		}
	}

	if ir != nil {
		// Unblind.
		m.Mul(m, ir)
		m.Mod(m, priv.N)
	}
	out = m

	return
}

func AddMod(z, x, y, n *big.Int) *big.Int {
	return z.Add(x, y).Mod(z, n)
}

func SubMod(z, x, y, n *big.Int) *big.Int {
	return z.Sub(x, y).Mod(z, n)
}
