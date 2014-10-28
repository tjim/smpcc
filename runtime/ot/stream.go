package ot

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
	return randomBytes(numBits / 8)
}

func randomBytes(numBytes int) []byte {
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
		tSeed := randomBytes(SeedBytes)
		vSeed := randomBytes(SeedBytes)
		sender.Send(tSeed, vSeed)
		tStream[i] = sha3.NewCipher(tSeed, nil)
		vStream[i] = sha3.NewCipher(vSeed, nil)
	}
	return StreamReceiver{tStream, vStream, to, from}
}

type StreamSender struct {
	s       []byte
	sWide   []byte
	wStream []cipher.Stream
	to      chan<- MessagePair
	from    <-chan []byte
}

func NewStreamSender(receiver Receiver, to chan<- MessagePair, from <-chan []byte) StreamSender {
	k := NumStreams
	s := randomBits(k)
	sWide := make([]byte, k)
	for i := range sWide {
		if bit.GetBit(s, i) == 0 {
			sWide[i] = 0x00
		} else {
			sWide[i] = 0xff
		}
	}
	wStream := make([]cipher.Stream, k)
	for i := range wStream {
		wSeed := receiver.Receive(Selector(bit.GetBit(s, i)))
		wStream[i] = sha3.NewCipher(wSeed, nil)
	}
	return StreamSender{s, sWide, wStream, to, from}
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
	q := bit.NewMatrix8(k, m) // k rows, m columns
	for i := 0; i < k; i++ {
		u_i := u.GetRow(i)
		w_i := bytesFrom(S.wStream[i], m/8)
		q_i := q.GetRow(i)
		s_i_wide := S.sWide[i]
		for j := range q_i {
			q_i[j] = (u_i[j] & s_i_wide) ^ w_i[j]
		}
	}
	q = q.Transpose() // m rows, k columns
	for j := 0; j < m; j++ {
		l := 8 * len(a[j])
		if l != 8*len(b[j]) {
			panic("SendM: pairs must have the same length")
		}
		m0 := XorBytes(a[j], RO(q.GetRow(j), l))
		m1 := XorBytes(b[j],
			RO(XorBytes(q.GetRow(j), S.s), l))
		S.to <- MessagePair{m0, m1}
	}
}

func (R StreamReceiver) ReceiveM(r []byte) [][]byte { // r is a packed vector of selections
	k := NumStreams
	m := 8 * len(r)
	t := bit.NewMatrix8(k, m) // k rows, m columns
	u := bit.NewMatrix8(k, m) // k rows, m columns
	for i := 0; i < k; i++ {
		bytesFromTo(R.tStream[i], t.GetRow(i))
		XorBytesTo(r, XorBytes(t.GetRow(i), bytesFrom(R.vStream[i], m/8)), u.GetRow(i))
	}
	R.to <- u.Data
	t = t.Transpose() // m rows, k columns
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

// Create a new StreamSender that can operate independently of the parent StreamSender (concurrent operation).
// It must be paired (via to/from) with a StreamReceiver forked from the original StreamSender's StreamReceiver.
func (S StreamSender) Fork(to chan<- MessagePair, from chan []byte) StreamSender {
	s := S.s
	sWide := S.sWide
	wStream := make([]cipher.Stream, len(S.wStream))
	for i, v := range S.wStream {
		wSeed := bytesFrom(v, SeedBytes)
		wStream[i] = sha3.NewCipher(wSeed, nil)
	}
	return StreamSender{s, sWide, wStream, to, from}
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
