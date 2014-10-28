package ot

/*

Notation: If t is a matrix,
    then t^i is the ith column of t,
    and  t_j is the jth row of t,
    and  t_j^i is the bit in the ith column and jth row of t.

Setup phase:

Let k be the security parameter.

Receiver picks random matrices t and v with k columns.
Let R be the matrix (t XOR v).
Sender picks random bits s = s^1 ... s^k   (a matrix with one row and k columns)
Receiver transfers t^i, v^i to Sender by OT according to s^i
Result is that Sender has a matrix w.

Note w = (R AND_by_row s) XOR t

OT extension phase:

Let r = r_1 ... be the Receiver's choices.  (A matrix with one column)
Let u = r XOR_by_column R

Receiver sends u to Sender
Sender sets q = (u AND_by_row s) XOR w
              = (u AND_by_row s) XOR (R AND_by_row s) XOR t
              = ((u XOR R) AND_by_row s) XOR t
              = (((r XOR_by_column R) XOR R) AND_by_row s) XOR t
              = (r AND_cross s) XOR t

Therefore we have

        q_j^i = (r_j AND s^i) XOR t_j^i

just as in the original OT extension paper.  So if the Sender wants to
send a_j, b_j it sets

        m0 = a_j ^ H(q_j)
        m1 = a_j ^ H(q_j ^ s)

and sends m0, m1 to the Receiver.

If r_j = 0 the receiver calculates a_j = m0 ^ H(t_j)
If r_j = 1 the receiver calculates b_j = m1 ^ H(t_j)


Implementation notes:

Most of the time in the implementation it is convenient to keep the
matrices transposed in comparison to the notes above.  So, a "row" in
the implementation might correspond to a "column" in the notes above.

*/

import (
	"bitbucket.org/ede/sha3"
	"crypto/cipher"
	"crypto/rand"
	"fmt"
	//	"time"
	"github.com/tjim/smpcc/runtime/bit"
)

const (
	SeedBytes  = 16
	NumStreams = 80 // the constant formerly known as k.  Must be a multiple of 8
)

func bytesFromTo(h cipher.Stream, output []byte) {
	h.XORKeyStream(output, output) // on entry output should be initialized to zero
}

func bytesFrom(h cipher.Stream, outBytes int) []byte {
	if outBytes <= 0 {
		panic("bytesFrom: output size <= 0")
	}
	output := make([]byte, outBytes)
	h.XORKeyStream(output, output)
	return output
}

// return numBits random bits packed into numBits/8 bytes
func randomBits(numBits int) []byte {
	if numBits%8 != 0 {
		panic("randomBits: number of bits must be a multiple of 8")
	}
	return RandomBytes(numBits / 8)
}

func RandomBytes(numBytes int) []byte {
	result := make([]byte, numBytes)
	_, err := rand.Read(result)
	if err != nil {
		panic("random number generation")
	}
	return result
}

type MessagePair struct {
	m0, m1 []byte
}

type StreamReceiver struct {
	tStream []cipher.Stream
	vStream []cipher.Stream
	to      chan<- []byte
	from    <-chan MessagePair
}

func NewStreamReceiver(sender Sender, to chan<- []byte, from <-chan MessagePair) StreamReceiver {
	k := NumStreams
	tStream := make([]cipher.Stream, k)
	vStream := make([]cipher.Stream, k)
	for i := range tStream {
		tSeed := RandomBytes(SeedBytes)
		vSeed := RandomBytes(SeedBytes)
		sender.Send(tSeed, vSeed)
		tStream[i] = sha3.NewCipher(tSeed, nil)
		vStream[i] = sha3.NewCipher(vSeed, nil)
	}
	return StreamReceiver{tStream, vStream, to, from}
}

type StreamSender struct {
	sPacked []byte // one byte per 8 bits of s
	sWide   []byte // one byte per bit of s, either 0x00 or 0xff
	wStream []cipher.Stream
	to      chan<- MessagePair
	from    <-chan []byte
}

func NewStreamSender(receiver Receiver, to chan<- MessagePair, from <-chan []byte) StreamSender {
	k := NumStreams
	sPacked := randomBits(k)
	sWide := make([]byte, k)
	for i := range sWide {
		if bit.GetBit(sPacked, i) == 0 {
			sWide[i] = 0x00
		} else {
			sWide[i] = 0xff
		}
	}
	wStream := make([]cipher.Stream, k)
	for i := range wStream {
		wSeed := receiver.Receive(Selector(bit.GetBit(sPacked, i)))
		wStream[i] = sha3.NewCipher(wSeed, nil)
	}
	return StreamSender{sPacked, sWide, wStream, to, from}
}

type PerBlockStreamChans struct {
	S2R chan MessagePair `fatchan:"request"` // One per sender/receiver pair, sender->receiver
	R2S chan []byte      `fatchan:"reply"`   // One per sender/receiver pair, receiver->sender
}

// Bitwise MUX of byte sequences a and b, according to byte sequence c.
// Each bit of the result is the corresponding bit of a if the corresponding bit of c is 0,
// or the corresponding bit of b if the corresponding bit of c is 1.
func MuxBytes(c, a, b []byte) []byte {
	if len(a) != len(b) || len(a) != len(c) {
		panic("MuxBytes")
	}
	result := make([]byte, len(a))
	for i, v := range c {
		result[i] = ((0xff ^ v) & a[i]) | (v & b[i])
	}
	return result
}

// Bitwise XOR of byte sequences a and b, which must have the same length
func XorBytesTo(a, b, result []byte) {
	if len(a) != len(b) || len(a) != len(result) {
		panic("XorBytesTo: mismatched lengths")
	}
	for i, v := range a {
		result[i] = v ^ b[i]
	}
}

func XorBytes(a, b []byte) []byte {
	result := make([]byte, len(a))
	XorBytesTo(a, b, result)
	return result
}

// Send m message pairs at once
func (S StreamSender) SendM(a, b [][]byte) {
	k := NumStreams
	m := len(a)
	if m%8 != 0 {
		panic("SendM: must send a multiple of 8 messages at a time")
	}
	if len(b) != m {
		panic("SendM: must send pairs of messages")
	}
	u := &bit.Matrix8{k, m, <-S.from} // k rows, m columns
	if len(u.Data) != k*(m/8) {
		panic("SendM: wrong size matrix u")
	}
	q := u // q starts off as u
	for i := 0; i < k; i++ {
		q_i := q.GetRow(i) // u_i
		s_i_wide := S.sWide[i]
		for jByte := range q_i {
			q_i[jByte] &= s_i_wide // & s_i
		}
		S.wStream[i].XORKeyStream(q_i, q_i) // XOR w_i
	}
	q = q.Transpose() // m rows, k columns
	for j := 0; j < m; j++ {
		l := 8 * len(a[j])
		if l != 8*len(b[j]) {
			panic("SendM: pairs must have the same length")
		}
		m0 := XorBytes(a[j], RO(q.GetRow(j), l))
		m1 := XorBytes(b[j],
			RO(XorBytes(q.GetRow(j), S.sPacked), l))
		S.to <- MessagePair{m0, m1}
	}
}

func (R StreamReceiver) ReceiveM(r []byte) [][]byte { // r is a packed vector of selections
	k := NumStreams
	m := 8 * len(r)
	t := bit.NewMatrix8(k, m) // k rows, m columns
	for i := 0; i < k; i++ {
		bytesFromTo(R.tStream[i], t.GetRow(i))
	}
	// save t in u
	u := t
	// transpose t for later use
	t = t.Transpose() // m rows, k columns

	// compute final value for u
	for i := 0; i < k; i++ {
		R.vStream[i].XORKeyStream(u.GetRow(i), u.GetRow(i)) // u = t XOR v
		XorBytesTo(r, u.GetRow(i), u.GetRow(i))             // u = (t XOR v) XOR r
	}
	R.to <- u.Data
	result := make([][]byte, m)
	for j := 0; j < m; j++ {
		msgs := <-R.from
		m0 := msgs.m0
		m1 := msgs.m1
		l := 8 * len(m0)
		if l != 8*len(m1) {
			panic("ReceiveM: pairs must have the same length")
		}
		if bit.GetBit(r, j) == 0 {
			result[j] = XorBytes(m0, RO(t.GetRow(j), l))
		} else {
			result[j] = XorBytes(m1, RO(t.GetRow(j), l))
		}
	}
	return result
}

// Send m pairs of bits (1-bit messages) at once
func (S StreamSender) SendMBits(a, b []byte) { // messages are packed in bytes
	k := NumStreams
	m := 8 * len(a)
	if 8*len(b) != m {
		panic("SendMBits: must send pairs of messages")
	}
	u := &bit.Matrix8{k, m, <-S.from} // k rows, m columns
	if 8*len(u.Data) != k*m {
		panic("SendMBits: wrong size matrix u")
	}
	q := u // q starts off as u
	for i := 0; i < k; i++ {
		q_i := q.GetRow(i) // u_i
		s_i_wide := S.sWide[i]
		for jByte := range q_i {
			q_i[jByte] &= s_i_wide // & s_i
		}
		S.wStream[i].XORKeyStream(q_i, q_i) // XOR w_i
	}
	q = q.Transpose() // m rows, k columns
	m0 := make([]byte, len(a))
	copy(m0, a)
	m1 := make([]byte, len(b))
	copy(m1, b)
	for jByte := range m0 {
		// instead of unpacking and repacking each message bit we just xor in place, using an appropriate bit of the hash
		for jBit := 0; jBit < 8; jBit++ {
			j := 8*jByte + jBit
			q_j := q.GetRow(j)
			mask := byte(0x80 >> uint(jBit))
			m0[jByte] ^= mask & RO(q_j, 8)[0]
			m1[jByte] ^= mask & RO(XorBytes(q_j, S.sPacked), 8)[0]
		}
	}
	S.to <- MessagePair{m0, m1}
}

func (R StreamReceiver) ReceiveMBits(r []byte) []byte { // r is a packed vector of selections and result is packed as well
	k := NumStreams
	m := 8 * len(r)
	t := bit.NewMatrix8(k, m) // k rows, m columns
	for i := 0; i < k; i++ {
		bytesFromTo(R.tStream[i], t.GetRow(i))
	}
	// save t in u
	u := t
	// transpose t for later use
	t = t.Transpose() // m rows, k columns

	// compute final value for u
	for i := 0; i < k; i++ {
		R.vStream[i].XORKeyStream(u.GetRow(i), u.GetRow(i)) // u = t XOR v
		XorBytesTo(r, u.GetRow(i), u.GetRow(i))             // u = (t XOR v) XOR r
	}
	R.to <- u.Data
	result := make([]byte, m/8)
	msgs := <-R.from
	m0 := msgs.m0
	m1 := msgs.m1
	for jByte := range m0 {
		// instead of unpacking and packing each message bit we just xor in place, using an appropriate bit of the hash
		for jBit := 0; jBit < 8; jBit++ {
			j := 8*jByte + jBit
			t_j := t.GetRow(j)
			mask := byte(0x80 >> uint(jBit))
			if r[jByte]&mask == 0 {
				result[jByte] |= mask & (m0[jByte] ^ RO(t_j, 8)[0])
			} else {
				result[jByte] |= mask & (m1[jByte] ^ RO(t_j, 8)[0])
			}
		}
	}
	return result
}

// Create a new StreamSender that can operate independently of the parent StreamSender (concurrent operation).
// It must be paired (via to/from) with a StreamReceiver forked from the original StreamSender's StreamReceiver.
func (S StreamSender) Fork(to chan<- MessagePair, from chan []byte) StreamSender {
	sPacked := S.sPacked
	sWide := S.sWide
	wStream := make([]cipher.Stream, len(S.wStream))
	for i, v := range S.wStream {
		wSeed := bytesFrom(v, SeedBytes)
		wStream[i] = sha3.NewCipher(wSeed, nil)
	}
	return StreamSender{sPacked, sWide, wStream, to, from}
}

// Create a new StreamReceiver that can operate independently of the parent StreamReceiver (concurrent operation).
// It must be paired (via to/from) with a StreamSender forked from the original StreamReceiver's StreamSender.
func (R StreamReceiver) Fork(to chan []byte, from chan MessagePair) StreamReceiver {
	tStream := make([]cipher.Stream, len(R.tStream))
	vStream := make([]cipher.Stream, len(R.vStream))
	for i, v := range R.tStream {
		tSeed := bytesFrom(v, SeedBytes)
		tStream[i] = sha3.NewCipher(tSeed, nil)
	}
	for i, v := range R.vStream {
		vSeed := bytesFrom(v, SeedBytes)
		vStream[i] = sha3.NewCipher(vSeed, nil)
	}
	return StreamReceiver{tStream, vStream, to, from}
}

func PrintBytes(r []byte) {
	for _, b := range r {
		fmt.Printf("%02x", b)
	}
	fmt.Println("")
}

func PrintBits(r []byte) {
	for _, b := range r {
		fmt.Printf("%08b", b)
	}
	fmt.Println("")
}
