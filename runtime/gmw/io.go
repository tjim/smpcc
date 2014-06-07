package gmw

import "fmt"
import "math/big"
import "crypto/rand"

var log_mem bool = true
var log_results bool = false
var log_communication bool = false

type Io interface {
	Id() int
	GetInput() uint32

	Open1(bool) bool
	Open8(uint8) uint8
	Open32(uint32) uint32
	Open64(uint64) uint64

	Broadcast1(bool)
	Broadcast8(uint8)
	Broadcast32(uint32)
	Broadcast64(uint64)

	Send1(party int, x bool)
	Send8(party int, x uint8)
	Send32(party int, x uint32)
	Send64(party int, x uint64)

	Receive1(party int) bool
	Receive8(party int) uint8
	Receive32(party int) uint32
	Receive64(party int) uint64

	Triple1() (a, b, c bool)
	Triple8() (a, b, c uint8)
	Triple32() (a, b, c uint32)
	Triple64() (a, b, c uint64)

	InitRam([]byte)
	Ram() []byte
}

/* mocked-up implementation of Io */
type X struct {
	id        int /* id of party, range is 0..n-1 */
	triples32 []struct{ a, b, c uint32 }
	triples8  []struct{ a, b, c uint8 }
	triples1  []struct{ a, b, c bool }
	rchannels []chan uint32 /* channels for reading from other parties */
	wchannels []chan uint32 /* channels for writing to other parties */
	inputs    []uint32      /* inputs of this party */
	ram       []byte
}

func (x *X) Id() int {
	return x.id
}

func (x *X) Triple1() (a, b, c bool) {
	if len(x.triples1) == 0 {
		a32, b32, c32 := x.Triple32()
		x.triples1 = make([]struct{ a, b, c bool }, 32)
		for i := range x.triples1 {
			ui := uint(i)
			x.triples1[i] = struct{ a, b, c bool }{0 < (1 & (a32 >> ui)), 0 < (1 & (b32 >> ui)), 0 < (1 & (c32 >> ui))}
		}
	}
	result := x.triples1[0]
	x.triples1 = x.triples1[1:]
	return result.a, result.b, result.c
}

func (x *X) Triple8() (a, b, c uint8) {
	if len(x.triples8) == 0 {
		a32, b32, c32 := x.Triple32()
		x.triples8 = []struct{ a, b, c uint8 }{{uint8(a32 >> 0), uint8(b32 >> 0), uint8(c32 >> 0)},
			{uint8(a32 >> 8), uint8(b32 >> 8), uint8(c32 >> 8)},
			{uint8(a32 >> 16), uint8(b32 >> 16), uint8(c32 >> 16)},
			{uint8(a32 >> 24), uint8(b32 >> 24), uint8(c32 >> 24)}}
	}
	result := x.triples8[0]
	x.triples8 = x.triples8[1:]
	return result.a, result.b, result.c
}

func (x *X) Triple32() (a, b, c uint32) {
	result := x.triples32[0]
	x.triples32 = x.triples32[1:]
	return result.a, result.b, result.c
}

func (x *X) Triple64() (a, b, c uint64) {
	a0, b0, c0 := x.Triple32()
	a1, b1, c1 := x.Triple32()
	return (uint64(a0) << 32) | uint64(a1), (uint64(b0) << 32) | uint64(b1), (uint64(c0) << 32) | uint64(c1)
}

func (x *X) Open1(s bool) bool {
	x.Broadcast1(s)
	result := s
	id := x.Id()
	for i := range x.rchannels {
		if i == id {
			continue
		}
		result = xor(result, x.Receive1(i))
	}
	return result
}

func (x *X) Open8(s uint8) uint8 {
	x.Broadcast8(s)
	result := s
	id := x.Id()
	for i := range x.rchannels {
		if i == id {
			continue
		}
		result ^= x.Receive8(i)
	}
	return result
}

func (x *X) Open32(s uint32) uint32 {
	x.Broadcast32(s)
	result := s
	id := x.Id()
	for i := range x.rchannels {
		if i == id {
			continue
		}
		result ^= x.Receive32(i)
	}
	return result
}

func (x *X) Open64(s uint64) uint64 {
	x.Broadcast64(s)
	result := s
	id := x.Id()
	for i := range x.rchannels {
		if i == id {
			continue
		}
		result ^= x.Receive64(i)
	}
	return result
}

func (x *X) Broadcast1(n bool) {
	var n32 uint32 = 0
	if n {
		n32 = 1
	}
	id := x.Id()
	if log_communication {
		fmt.Printf("%d: BROADCAST 0x%1x\n", id, n32)
	}
	for i, ch := range x.wchannels {
		if i == id {
			continue
		}
		ch <- n32
		if log_communication {
			fmt.Printf("%d -- 0x%1x -> %d\n", id, n32, i)
		}
	}
}

func (x *X) Broadcast8(n uint8) {
	id := x.Id()
	if log_communication {
		fmt.Printf("%d: BROADCAST 0x%02x\n", id, n)
	}
	for i, ch := range x.wchannels {
		if i == id {
			continue
		}
		ch <- uint32(n)
		if log_communication {
			fmt.Printf("%d -- 0x%02x -> %d\n", id, n, i)
		}
	}
}

func (x *X) Broadcast32(n uint32) {
	id := x.Id()
	if log_communication {
		fmt.Printf("%d: BROADCAST 0x%08x\n", id, n)
	}
	for i, ch := range x.wchannels {
		if i == id {
			continue
		}
		ch <- n
		if log_communication {
			fmt.Printf("%d -- 0x%08x -> %d\n", id, n, i)
		}
	}
}

func (x *X) Broadcast64(n uint64) {
	n0 := uint32(n >> 32)
	n1 := uint32(n)
	x.Broadcast32(n0)
	x.Broadcast32(n1)
}

func (x *X) Send1(party int, n bool) {
	var n32 uint32 = 0
	if n {
		n32 = 1
	}
	x.Send32(party, n32)
}

func (x *X) Send8(party int, n uint8) {
	var n32 uint32 = uint32(n)
	id := x.Id()
	if party == id {
		return
	}
	x.Send32(party, n32)
}

func (x *X) Send32(party int, n32 uint32) {
	id := x.Id()
	if party == id {
		return
	}
	ch := x.wchannels[party]
	ch <- n32
	if log_communication {
		fmt.Printf("%d -- 0x%1x -> %d\n", id, n32, party)
	}
}

func (x *X) Send64(party int, n uint64) {
	n0 := uint32(n >> 32)
	n1 := uint32(n)
	x.Send32(party, n0)
	x.Send32(party, n1)
}

func (x *X) Receive1(party int) bool {
	result := x.Receive32(party)
	return result > 0
}

func (x *X) Receive8(party int) uint8 {
	result := x.Receive32(party)
	return uint8(result)
}

func (x *X) Receive32(party int) uint32 {
	id := x.Id()
	if party == id {
		return 0
	}
	ch := x.rchannels[party]
	result := <-ch
	if log_communication {
		fmt.Printf("%d <- 0x%08x -- %d\n", id, result, party)
	}
	return result
}

func (x *X) Receive64(party int) uint64 {
	n0 := x.Receive32(party)
	n1 := x.Receive32(party)
	return (uint64(n0) << 32) | uint64(n1)
}

func (x *X) GetInput() uint32 {
	result := x.inputs[0]
	x.inputs = x.inputs[1:]
	return result
}

func (x *X) InitRam(contents []byte) {
	x.ram = contents
}

func (x *X) Ram() []byte {
	return x.ram
}

func rand32() uint32 {
	max := big.NewInt(1 << 32)
	x, err := rand.Int(rand.Reader, max)
	if err != nil {
		panic("Error: random number generation")
	}
	return uint32(x.Int64())
}

/* return a slice of n random uint32 values that ^ to x */
func split_uint32(x uint32, n int) []uint32 {
	x0 := x
	if n <= 0 {
		panic("Error: split")
	}
	result := make([]uint32, n)
	for i := 1; i < n; i++ {
		xi := rand32()
		x ^= xi
		result[i] = xi
	}
	result[0] = x
	var y uint32 = 0
	for _, s := range result {
		y ^= s
	}
	if y != x0 {
		panic("NOT EQUAL")
	}
	return result
}

/* create n shares of a multiplication triple */
func triple32(n int) []struct{ a, b, c uint32 } {
	a, b := rand32(), rand32()
	c := a & b
	a_shares := split_uint32(a, n)
	b_shares := split_uint32(b, n)
	c_shares := split_uint32(c, n)
	result := make([]struct{ a, b, c uint32 }, n)
	for i, _ := range result {
		result[i] = struct{ a, b, c uint32 }{a_shares[i], b_shares[i], c_shares[i]}
	}
	return result
}

func Example(n int) []*X {
	/* triples */
	triples32 := make([][]struct{ a, b, c uint32 }, n)
	num_triples := 8 * 4096
	for i, _ := range triples32 {
		triples32[i] = make([]struct{ a, b, c uint32 }, num_triples)
	}
	for j := 0; j < num_triples; j++ {
		shares_of_a_triple := triple32(n)
		for i, t := range shares_of_a_triple {
			triples32[i][j] = t
		}
	}

	/* channels */
	rchannels := make([] /*n*/ [] /*n*/ chan uint32, n)
	wchannels := make([] /*n*/ [] /*n*/ chan uint32, n)
	for i := range rchannels {
		rchannels[i] = make([] /*n*/ chan uint32, n)
		wchannels[i] = make([] /*n*/ chan uint32, n)
	}
	for i := range rchannels {
		for j := range rchannels {
			if i == j {
				continue
			}
			ch := make(chan uint32, n)
			rchannels[i][j] = ch
			wchannels[j][i] = ch
		}
	}

	/* final result */
	result := make([]*X, n)
	for id := range result {
		inputs := make([]uint32, 1)
		inputs[0] = uint32(id)
		result[id] =
			&X{id,
				triples32[id],
				make([]struct{ a, b, c uint8 }, 0),
				make([]struct{ a, b, c bool }, 0),
				rchannels[id],
				wchannels[id],
				inputs,
				nil}
	}
	return result
}

func RunExample() uint32 {
	xs := Example(3)
	done := make(chan uint32, len(xs))
	results := make([]uint32, len(xs))
	for _, x := range xs {
		go func(x *X) {
			done <- Output8(x, Lshr8(x, Uint8(x, 128), 7))
		}(x)
	}
	for i := range xs {
		results[i] = <-done
	}
	result := results[0]
	for i := range results {
		if results[i] != result {
			panic(fmt.Sprintf("Error: results don't match (0x%08x != 0x%08x)", results[i], result))
		}
	}
	fmt.Printf("Result: 0x%08x\n", result)
	return result
}
