/* Benchmark and test multiplex oblivious transfer extension

   go test -cpu 4 -bench . github.com/tjim/smpcc/runtime/ot

*/

package ot

import "bytes"
import "fmt"
import "testing"

var PAIRS int = 4
var ITERATIONS int = 1024

var done chan bool = make(chan bool)

var hello []byte = []byte("hello")
var world []byte = []byte("world")

func sender(repCh chan []byte, reqCh chan SendRequest, getCh chan getRequest, b *testing.B) {
	x := NewMplexSender(repCh, reqCh, getCh)
	for i := 0; i < ITERATIONS; i++ {
		x.Send(hello, world)
	}
	done <- true
}

func receiver(repCh chan []byte, reqCh chan SendRequest, nextCh chan nextRequest, b *testing.B) {
	x := NewMplexReceiver(repCh, reqCh, nextCh)
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

func pair(chS chan getRequest, chR chan nextRequest, b *testing.B) {
	chA1 := make(chan []byte)
	chA2 := make(chan SendRequest)
	go sender(chA1, chA2, chS, b)
	go receiver(chA1, chA2, chR, b)
}

func Benchmark1(b *testing.B) {
	k := 80
	m := 1024
	baseSender, baseReceiver := NewNP()
	refreshCh := make(chan int)

	chS := PrimarySender(baseReceiver, refreshCh, k, m)
	chR := PrimaryReceiver(baseSender, refreshCh, k, m)

	for i := 0; i < PAIRS; i++ {
		pair(chS, chR, b)
	}
	for i := 0; i < PAIRS; i++ {
		<-done
		<-done
	}
}
