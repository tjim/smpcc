package gmw

import (
	"crypto/cipher"
	"github.com/tjim/smpcc/runtime/bit"
	"github.com/tjim/smpcc/runtime/ot"
)

type TripleState struct {
	RandomStreams []cipher.Stream
}

func InitTripleState(t *TripleState) {
	for i, _ := range t.RandomStreams {
		seed := ot.RandomBytes(ot.SeedBytes)
		t.RandomStreams[i] = ot.NewPRG(seed)
	}
}

// A multiplication triple (a,b,c) satisfies (a AND b) = c
// In GMW each party is supposed to have an XOR share of a triple
// In the commodity model each party has a random number generator known to the server
// Each party (but one) generates its share of (a,b,c) randomly
// A designated party generates a random share of (a,b,c) but also receives a **correction** from the commodity server
// The server can calculate the correction because it knows the randomness of each party
// The correction is XORed by the designated party with their c component, resulting in a true multiplication triple
func (t *TripleState) TripleCorrection() []byte {
	numBytes := NUM_TRIPLES * 4
	a := make([]byte, numBytes)
	b := make([]byte, numBytes)
	c := make([]byte, numBytes)
	// calculate and combine the shares of all the parties
	for _, v := range t.RandomStreams {
		v.XORKeyStream(a, a)
		v.XORKeyStream(b, b)
		v.XORKeyStream(c, c)
	}
	// calculate the desired value for c
	desired := AndBytes(a, b)
	// desired = (c XOR correction) so correction = (desired XOR c)
	return ot.XorBytes(desired, c)
}

// A mask triple (a,B,C) satisfies (a AND B) = C
// Here a is a bit (represented as a byte 0 or 1) and C = 0 if a = 0, and C = B if a = 1
// In GMW each party is supposed to have an XOR share of a mask triple (the share of a is a byte, 0 or 1)
// In the commodity model each party has a random number generator known to the server
// Each party (but one) generates its share of (a,B,C) randomly
// A designated party generates a random share of (a,B,C) but also receives a **correction** from the commodity server
// The server can calculate the correction because it knows the randomness of each party
// The correction is XORed by the designated party with their c component, resulting in a true mask triple

// NB we use the same TripleState for mask triples and multiplication triples

func (t *TripleState) MaskTripleCorrection(numTriples, numBytes int) [][]byte {

	A := make([]byte, numTriples/8)
	for _, v := range t.RandomStreams {
		v.XORKeyStream(A, A)
	}
	B := make([][]byte, numTriples)
	C := make([][]byte, numTriples)
	correction := make([][]byte, numTriples)
	for j := range B {
		B[j] = make([]byte, numBytes)
		C[j] = make([]byte, numBytes)
		for _, v := range t.RandomStreams {
			v.XORKeyStream(B[j], B[j])
			v.XORKeyStream(C[j], C[j])
		}
		if bit.GetBit(A, j) == 1 {
			correction[j] = ot.XorBytes(B[j], C[j]) // B is desired output
		} else { // 0
			correction[j] = C[j] // all 0 is desired output
		}

	}
	return correction
}
