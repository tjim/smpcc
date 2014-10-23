package ot

import (
	"bitbucket.org/ede/sha3"
	"crypto/cipher"
	"crypto/rand"
	"fmt"
	//	"time"
)

const (
	SeedBytes = 16
)

func bytesFrom(h cipher.Stream, outBytes int) []byte {
	if outBytes <= 0 {
		panic("bytesFrom: output size <= 0")
	}
	output := make([]byte, outBytes)
	h.XORKeyStream(output, output)
	return output
}

func randomBit() byte {
	result := make([]byte, 1)
	_, err := rand.Read(result)
	if err != nil {
		panic("randomBit")
	}
	result[0] = result[0] % 2
	return result[0]
}

func randomSeed() []byte {
	seed := make([]byte, SeedBytes)
	_, err := rand.Read(seed)
	if err != nil {
		panic("random number generation")
	}
	return seed
}

type MessagePair struct {
	m0, m1 []byte
}

type StreamReceiver struct {
	tStream  cipher.Stream
	rtStream cipher.Stream
	to       chan<- []byte
	from     <-chan MessagePair
}

func NewStreamReceiver(sender Sender, to chan<- []byte, from <-chan MessagePair) StreamReceiver {
	tSeed := randomSeed()
	rtSeed := randomSeed()
	sender.Send(tSeed, rtSeed)
	tStream := sha3.NewCipher(tSeed, nil)
	rtStream := sha3.NewCipher(rtSeed, nil)
	return StreamReceiver{tStream, rtStream, to, from}
}

type StreamSender struct {
	s         byte // 0 or 1
	rstStream cipher.Stream
	to        chan<- MessagePair
	from      <-chan []byte
}

func NewStreamSender(receiver Receiver, to chan<- MessagePair, from <-chan []byte) StreamSender {
	s := randomBit()
	rstSeed := receiver.Receive(Selector(s))
	rstStream := sha3.NewCipher(rstSeed, nil)
	return StreamSender{s, rstStream, to, from}
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
func XorBytes(a, b []byte) []byte {
	if len(a) != len(b) {
		panic("XorBytes")
	}
	result := make([]byte, len(a))
	for i, v := range a {
		result[i] = v ^ b[i]
	}
	return result
}

func spreadBytes(b byte, n int) []byte {
	if n <= 0 {
		panic("spreadBytes")
	}
	result := make([]byte, n)
	switch b {
	case 0:
		// these assignments are noops but prevent timing attack
		for i := range result {
			result[i] = 0x00
		}
	case 1:
		for i := range result {
			result[i] = 0xff
		}
	default:
		panic("spreadBytes")
	}
	return result
}

func (S StreamSender) SendBitwise(a, b []byte) {
	L := len(a)
	if len(b) != L {
		panic("SendBitwise message length")
	}
	rc := <-S.from
	if len(rc) != L {
		panic("SendBitwise selector length")
	}
	spreadS := spreadBytes(S.s, L)
	rst := bytesFrom(S.rstStream, L)
	m0 := MuxBytes(rc,
		XorBytes(a, rst),
		XorBytes(b, rst))
	m1 := MuxBytes(rc,
		XorBytes(XorBytes(b, rst), spreadS),
		XorBytes(XorBytes(a, rst), spreadS))
	S.to <- MessagePair{m0, m1}
}

func (R StreamReceiver) ReceiveBitwise(c []byte) []byte {
	L := len(c)
	t := bytesFrom(R.tStream, L)
	rt := bytesFrom(R.rtStream, L)
	r := XorBytes(t, rt)
	rc := XorBytes(r, c)
	R.to <- rc
	msgs := <-R.from
	m0 := msgs.m0
	m1 := msgs.m1
	t0 := XorBytes(t, m0)
	t1 := XorBytes(t, m1)
	result := MuxBytes(r, t0, t1)
	return result
}

// Create a new StreamSender that can operate independently of the parent StreamSender (concurrent operation).
// It must be paired (via to/from) with a StreamReceiver forked from the original StreamSender's StreamReceiver.
func (S StreamSender) Fork(to chan<- MessagePair, from chan []byte) StreamSender {
	s := S.s
	rstSeed := bytesFrom(S.rstStream, SeedBytes)
	rstStream := sha3.NewCipher(rstSeed, nil)
	return StreamSender{s, rstStream, to, from}
}

// Create a new StreamReceiver that can operate independently of the parent StreamReceiver (concurrent operation).
// It must be paired (via to/from) with a StreamSender forked from the original StreamReceiver's StreamSender.
func (R StreamReceiver) Fork(to chan []byte, from chan MessagePair) StreamReceiver {
	tSeed := bytesFrom(R.tStream, SeedBytes)
	rtSeed := bytesFrom(R.rtStream, SeedBytes)
	tStream := sha3.NewCipher(tSeed, nil)
	rtStream := sha3.NewCipher(rtSeed, nil)
	return StreamReceiver{tStream, rtStream, to, from}
}

func printBytes(r []byte) {
	for _, b := range r {
		fmt.Printf("%02x", b)
	}
	fmt.Println("")
}

func main() {
	r2s := make(chan []byte)
	s2r := make(chan MessagePair)
	BaseS, BaseR := NewNP()
	var S StreamSender
	go func() {
		S = NewStreamSender(BaseR, s2r, r2s)
	}()
	R := NewStreamReceiver(BaseS, r2s, s2r)
	hello := []byte("hello")
	world := []byte("world")
	go func() {
		S.SendBitwise(hello, world)
	}()
	c := spreadBytes(1, 5)
	x := R.ReceiveBitwise(c)
	fmt.Printf("%s\n", x)

	r2s = make(chan []byte)
	s2r = make(chan MessagePair)
	R2 := R.Fork(r2s, s2r)
	S2 := S.Fork(s2r, r2s)
	go func() {
		S2.SendBitwise(hello, world)
	}()
	x = R2.ReceiveBitwise(spreadBytes(0, 5))
	fmt.Printf("%s\n", x)
	//	// works out to about 64MB/1.23s
	//	compute_start_time := time.Now()
	//	numBytes := (2<<20)*64
	//	bytesFrom(R.tStream, numBytes)
	//	fmt.Printf("Computation took %s\n", time.Since(compute_start_time).String())
}
