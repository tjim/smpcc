package ot

// ot.go
// Oblivious transfer basic types and functions

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

type Message []byte
type Selector byte

type Sender interface {
	Send(Message, Message)
}

type Receiver interface {
	Receive(Selector) Message
}

// PublicKey represents an ElGamal public key.
type PublicKey struct {
	G, P, Y *big.Int
}

// PrivateKey represents an ElGamal private key.
type PrivateKey struct {
	PublicKey
	X *big.Int
}

func onError(err error, message string) {
	if err != nil {
		panic(fmt.Sprintf("%v: %s", err, message))
	}
}

func generateNumNonce(max *big.Int) *big.Int {
	result, err := rand.Int(rand.Reader, max)
	onError(err, "random number generation failed")
	return result
}
