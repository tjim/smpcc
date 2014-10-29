/* Oblivious transfer benchmarks

   go test -cpu 4 -bench . github.com/tjim/smpcc/runtime/ot

*/

package ot

import "bytes"
import "fmt"
import "testing"

const (
	PAIRS = 4
	//	PAIRS      = 20
	ITERATIONS = 1024
	//	ITERATIONS = 1032 // runs past the chunk size of extend.go
)

var done chan bool = make(chan bool)

var hello []byte = []byte("hello")
var world []byte = []byte("world")

func senderBench(x Sender, b *testing.B) {
	for i := 0; i < ITERATIONS; i++ {
		x.Send(hello, world)
	}
	done <- true
}

func receiverBench(x Receiver, b *testing.B) {
	for i := 0; i < ITERATIONS; i++ {
		s := i % 2
		rslt := x.Receive(Selector(s))
		switch {
		case s == 0:
			if !bytes.Equal(rslt, hello) {
				fmt.Printf("Error: expected hello, got %s\n", rslt)
				b.Fail()
			}
		case s == 1:
			if !bytes.Equal(rslt, world) {
				fmt.Printf("Error: expected world, got %s\n", rslt)
				b.Fail()
			}
		default:
			fmt.Printf("Error: s == %d\n", s)
			b.Fail()
		}
	}
	done <- true
}

// Naor-Pinkas OT
func pairNaorPinkas(b *testing.B) {
	s, r := NewNP()
	go senderBench(s, b)
	go receiverBench(r, b)
}

func BenchmarkNaorPinkas(b *testing.B) {
	for i := 0; i < PAIRS; i++ {
		pairNaorPinkas(b)
	}
	for i := 0; i < PAIRS; i++ {
		<-done
		<-done
	}
}

// OT extension
func pairExtend(b *testing.B) {
	k := 80
	m := 1024
	baseSender, baseReceiver := NewNP()
	OtExtChan := make(chan []byte)
	OtExtSelChan := make(chan Selector)

	s := NewExtendSender(OtExtChan, OtExtSelChan, baseReceiver, k, m)
	r := NewExtendReceiver(OtExtChan, OtExtSelChan, baseSender, k, m)

	go senderBench(s, b)
	go receiverBench(r, b)
}

func BenchmarkExtend(b *testing.B) {
	for i := 0; i < PAIRS; i++ {
		pairExtend(b)
	}
	for i := 0; i < PAIRS; i++ {
		<-done
		<-done
	}
}

// Multiplexed OT extension
func pairMplex(chS chan getRequest, chR chan nextRequest, b *testing.B) {
	chA1 := make(chan []byte)
	chA2 := make(chan SendRequest)
	s := NewMplexSender(chA1, chA2, chS)
	r := NewMplexReceiver(chA1, chA2, chR)
	go senderBench(s, b)
	go receiverBench(r, b)
}

func BenchmarkMplex(b *testing.B) {
	k := 80
	m := 1024
	baseSender, baseReceiver := NewNP()
	refreshCh := make(chan int)

	chS := PrimarySender(baseReceiver, refreshCh, k, m)
	chR := PrimaryReceiver(baseSender, refreshCh, k, m)

	for i := 0; i < PAIRS; i++ {
		pairMplex(chS, chR, b)
	}
	for i := 0; i < PAIRS; i++ {
		<-done
		<-done
	}
}

// Stream OT
var hello8 [][]byte = [][]byte{hello, hello, hello, hello, hello, hello, hello, hello}
var world8 [][]byte = [][]byte{world, world, world, world, world, world, world, world}

func senderBenchStream(x StreamSender, b *testing.B) {
	for i := 0; i < ITERATIONS/8; i++ {
		x.SendM(hello8, world8)
	}
	done <- true
}

func receiverBenchStream(x StreamReceiver, b *testing.B) {
	for i := 0; i < ITERATIONS/8; i++ {
		r := []byte{0xaa} // receive 8 messages, alternating hello and world, 0xaa == 0b10101010
		rslt := x.ReceiveM(r)
		if len(rslt) != 8 {
			fmt.Printf("Error: expected 8 messages, received %d\n", len(rslt))
			b.Fail()
		}
		for i, v := range rslt {
			switch r[0] & (0x80 >> uint(i)) {
			case 0:
				if !bytes.Equal(v, hello) {
					fmt.Printf("Error: expected hello, got %s\n", rslt)
					b.Fail()
				}
			default:
				if !bytes.Equal(v, world) {
					fmt.Printf("Error: expected world, got %s\n", rslt)
					b.Fail()
				}
			}
		}
	}
	done <- true
}

func pairStream(b *testing.B, S StreamSender, R StreamReceiver) {
	r2s := make(chan []byte)
	s2r := make(chan MessagePair)
	s := S.Fork(s2r, r2s)
	r := R.Fork(r2s, s2r)
	go senderBenchStream(s, b)
	go receiverBenchStream(r, b)
}

func BenchmarkStream(b *testing.B) {
	// create sender and receiver
	r2s := make(chan []byte)
	s2r := make(chan MessagePair)
	BaseS, BaseR := NewNP()
	var S StreamSender
	go func() {
		S = NewStreamSender(BaseR, s2r, r2s)
		done <- true
	}()
	R := NewStreamReceiver(BaseS, r2s, s2r)
	<-done

	for i := 0; i < PAIRS; i++ {
		pairStream(b, S, R)
	}
	for i := 0; i < PAIRS; i++ {
		<-done
		<-done
	}
}

// Stream OT with 1-bit messages
func senderBenchStreamBits(x StreamSender, b *testing.B) {
	teeth0 := make([]byte, ITERATIONS/8)
	teeth1 := make([]byte, ITERATIONS/8)
	for i := range teeth0 {
		teeth0[i] = 0xaa // 0b10101010
		teeth1[i] = 0x55 // 0b01010101
	}
	x.SendMBits(teeth0, teeth1)
	done <- true
}

func receiverBenchStreamBits(x StreamReceiver, b *testing.B) {
	r := make([]byte, ITERATIONS/8)
	for i := range r {
		r[i] = 0xaa // 0b10101010
	}
	rslt := x.ReceiveMBits(r)
	for i := range rslt {
		if rslt[i] != 0x00 {
			fmt.Printf("Error: expected 0x00, received 0x%02x\n", rslt[i])
			b.Fail()
		}
	}
	done <- true
}

func pairStreamBits(b *testing.B, S StreamSender, R StreamReceiver) {
	r2s := make(chan []byte)
	s2r := make(chan MessagePair)
	s := S.Fork(s2r, r2s)
	r := R.Fork(r2s, s2r)
	go senderBenchStreamBits(s, b)
	go receiverBenchStreamBits(r, b)
}

func BenchmarkStreamBits(b *testing.B) {
	// create sender and receiver
	r2s := make(chan []byte)
	s2r := make(chan MessagePair)
	BaseS, BaseR := NewNP()
	var S StreamSender
	go func() {
		S = NewStreamSender(BaseR, s2r, r2s)
		done <- true
	}()
	R := NewStreamReceiver(BaseS, r2s, s2r)
	<-done

	for i := 0; i < PAIRS; i++ {
		pairStreamBits(b, S, R)
	}
	for i := 0; i < PAIRS; i++ {
		<-done
		<-done
	}
}

// Stream OT with random 1-bit messages
func senderBenchRandomBits(x StreamSender, resultChan chan []byte, b *testing.B) {
	A, B := x.SendMRandomBits(ITERATIONS)
	resultChan <- A
	resultChan <- B
	done <- true
}

func receiverBenchRandomBits(x StreamReceiver, resultChan chan []byte, b *testing.B) {
	r := make([]byte, ITERATIONS/8)
	for i := range r {
		r[i] = 0xaa // 0b10101010
	}
	rslt := x.ReceiveMRandomBits(r)
	A := <-resultChan
	B := <-resultChan
	expected := MuxBytes(r, A, B)
	for i := range rslt {
		if rslt[i] != expected[i] {
			fmt.Printf("Error: expected 0x%02x, received 0x%02x\n", expected[i], rslt[i])
			b.Fail()
		}
	}
	done <- true
}

func pairRandomBits(b *testing.B, S StreamSender, R StreamReceiver) {
	r2s := make(chan []byte)
	s2r := make(chan MessagePair)
	s := S.Fork(s2r, r2s)
	r := R.Fork(r2s, s2r)
	var resultChan chan []byte = make(chan []byte)
	go senderBenchRandomBits(s, resultChan, b)
	go receiverBenchRandomBits(r, resultChan, b)
}

func BenchmarkRandomBits(b *testing.B) {
	// create sender and receiver
	r2s := make(chan []byte)
	s2r := make(chan MessagePair)
	BaseS, BaseR := NewNP()
	var S StreamSender
	go func() {
		S = NewStreamSender(BaseR, s2r, r2s)
		done <- true
	}()
	R := NewStreamReceiver(BaseS, r2s, s2r)
	<-done

	for i := 0; i < PAIRS; i++ {
		pairRandomBits(b, S, R)
	}
	for i := 0; i < PAIRS; i++ {
		<-done
		<-done
	}
}
