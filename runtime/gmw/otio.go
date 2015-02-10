package gmw

import (
	"crypto/rand"
	"github.com/tjim/smpcc/runtime/bit"
	"github.com/tjim/smpcc/runtime/ot"
)

type OtState struct {
	id        int
	senders   []*ot.StreamSender
	receivers []*ot.StreamReceiver
}

func NewOtState(id, numParties int) *OtState {
	senders := make([]*ot.StreamSender, numParties)
	receivers := make([]*ot.StreamReceiver, numParties)
	return &OtState{id, senders, receivers}
}

func piMulRMask(val []byte, receiver *ot.StreamReceiver) []ot.Message {
	return receiver.ReceiveM(val)
}

func piMulSMask(val [][]byte, sender *ot.StreamSender) []ot.Message {
	x0 := make([]ot.Message, len(val))
	x1 := make([]ot.Message, len(val))
	for i := range x0 {
		B := val[i]
		x0[i] = randomBytes(len(B))
		x1[i] = ot.XorBytes(x0[i], B)
	}
	sender.SendM(x0, x1)
	return x0
}

func (s *OtState) maskTriple(numTriples, numBytes int) []MaskTriple {
	if numTriples%8 != 0 {
		panic("maskTriple: can only generate mask triples in multiples of 8")
	}
	id := s.id
	senders := s.senders
	receivers := s.receivers
	n := len(senders)
	result := make([]MaskTriple, numTriples)
	// Notation is from Figure 9 (p11) of
	//
	// "Multiparty Computation for Dishonest Majority: from Passive to Active Security at Low Cost"
	// by Damgard and Orlandi
	// http://eprint.iacr.org/2010/318

	// use i to range over parties (0...n-1)
	// use j to range over triples (0...numTriples-1)
	A := randomBytes(numTriples / 8)
	B := make([][]byte, numTriples)
	for j := range B {
		B[j] = randomBytes(numBytes)
	}
	C := make([][]byte, numTriples)
	D := make([][]ot.Message, n)
	E := make([][]ot.Message, n)
	for i := range D {
		if i == id {
			continue
		}
		if id > i {
			D[i] = piMulRMask(A, receivers[i])
			E[i] = piMulSMask(B, senders[i])
		} else {
			E[i] = piMulSMask(B, senders[i])
			D[i] = piMulRMask(A, receivers[i])
		}
	}
	for j := range C {
		a := bit.GetBit(A, j)
		c := AndVector(a, B[j])
		for i := range D {
			if i == id {
				continue
			}
			c = ot.XorBytes(c,
				ot.XorBytes(D[i][j], E[i][j]))
		}
		C[j] = c
	}
	for j := range result {
		a := bit.GetBit(A, j)
		result[j] = MaskTriple{a, B[j], C[j]}
	}
	return result
}

func piMulR(val []byte, receiver *ot.StreamReceiver) []byte {
	return receiver.ReceiveMBits(val)
}

func piMulS(val []byte, sender *ot.StreamSender) []byte {
	x0 := randomBytes(len(val))
	x1 := ot.XorBytes(x0, val)
	sender.SendMBits(x0, x1)
	return x0
}

func randomBytes(numBytes int) []byte {
	result := make([]byte, numBytes)
	_, err := rand.Read(result)
	if err != nil {
		panic("random number generation")
	}
	return result
}

func (s *OtState) triple32() []Triple {
	id := s.id
	senders := s.senders
	receivers := s.receivers
	n := len(senders)
	result := make([]Triple, NUM_TRIPLES)
	numBytes := NUM_TRIPLES * 4
	// Notation is from Figure 9 (p11) of
	//
	// "Multiparty Computation for Dishonest Majority: from Passive to Active Security at Low Cost"
	// by Damgard and Orlandi
	// http://eprint.iacr.org/2010/318
	//
	// and Algorithm 1 from
	//
	// "More Efficient Oblivious Transfer and Extensions for Faster Secure Computation"
	// Gilad Asharov and Yehuda Lindell and Thomas Schneider and Michael Zohner
	// http://eprint.iacr.org/2013/552

	a := randomBytes(numBytes)
	b := randomBytes(numBytes)

	d := make([][]byte, n)
	e := make([][]byte, n)

	for i := 0; i < n; i++ {
		if i == id {
			continue
		}
		if id > i {
			d[i] = piMulR(a, receivers[i])
			e[i] = piMulS(b, senders[i])
		} else {
			e[i] = piMulS(b, senders[i])
			d[i] = piMulR(a, receivers[i])
		}
	}

	c := AndBytes(a, b)
	for i := 0; i < n; i++ {
		if i == id {
			continue
		}
		c = ot.XorBytes(c,
			ot.XorBytes(d[i], e[i]))
	}
	for i := range result {
		result[i] = Triple{combine(a[:4]), combine(b[:4]), combine(c[:4])}
		a = a[4:]
		b = b[4:]
		c = c[4:]
	}
	return result
}
