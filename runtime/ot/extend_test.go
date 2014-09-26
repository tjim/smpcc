/* Benchmark and test oblivious transfer extension

   go test -cpu 4 -bench . github.com/tjim/smpcc/runtime/ot

*/

package ot

import "bytes"
import "fmt"
import "testing"

func sender2(x Sender, b *testing.B) {
	for i := 0; i < ITERATIONS; i++ {
		x.Send(hello, world)
	}
	done <- true
}

func receiver2(x Receiver, b *testing.B) {
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

func pair2(b *testing.B) {
	s, r := NewNP()
	go sender2(s, b)
	go receiver2(r, b)
}

func Benchmark2(b *testing.B) {
	for i := 0; i < PAIRS; i++ {
		pair2(b)
	}
	for i := 0; i < PAIRS; i++ {
		<-done
		<-done
	}
}
