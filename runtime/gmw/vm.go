package gmw

import "fmt"
import "math/big"
import "crypto/rand"

type Io interface {
	Id() int
	GetInput() uint32
	Triple32() (a, b, c uint32)
	Open32(uint32) uint32
	Broadcast32(uint32)
	Receive32(party int) uint32
}

func Xor32(io Io, x, y uint32) uint32 {
	return x ^ y
}

func And32(io Io, x, y uint32) uint32 {
	a, b, c := io.Triple32()
	d := io.Open32(x^a)
	e := io.Open32(y^b)
	if io.Id() == 0 {
		return c ^ d&b ^ e&a ^ d&e
	} else {
		return c ^ d&b ^ e&a
	}
}

/* mocked-up implementation of Io */
type X struct {
	id        int                       /* id of party, range is 0..n-1 */
	triples32 []struct{ a, b, c uint32 }
	rchannels []chan uint32             /* channels for reading from other parties */
	wchannels []chan uint32             /* channels for writing to other parties */
	inputs    []uint32                  /* inputs of this party */
}

func (x X) Id() int {
	return x.id
}

func (x X) Triple32() (a, b, c uint32) {
	result := x.triples32[0]
	x.triples32 = x.triples32[1:]
	return result.a, result.b, result.c
}

func (x X) Open32(s uint32) uint32 {
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

func (x X) GetInput() uint32 {
	result := x.inputs[0]
	x.inputs = x.inputs[1:]
	return result
}

func (x X) Broadcast32(n uint32) {
	id := x.Id()
	fmt.Printf("%d: BROADCAST 0x%08x\n", id, n)
	for i, ch := range x.wchannels {
		if i == id {
			continue
		}
		go func(ch chan<- uint32, i int) {
			ch <- n
			fmt.Printf("%d -- 0x%08x -> %d\n", id, n, i)
		}(ch, i)
	}
}

func (x X) Receive32(party int) uint32 {
	id := x.Id()
	if party == id {
		return 0
	}
	ch := x.rchannels[party]
	result := <-ch
	fmt.Printf("%d <- 0x%08x -- %d\n", id, result, party)
	return result
}

func Input32(io Io, party int) uint32 {
	id := io.Id()
	if id == party {
		X := io.GetInput()
		return X
	} else {
		return 0
	}
}

func Output32(io Io, x uint32) uint32 {
	result := io.Open32(x)
	fmt.Printf("%d: RESULT 0x%08x\n", io.Id(), result)
	return result
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

func Example(n int) []X {
	/* triples */
	triples32 := make([][]struct{ a, b, c uint32 }, n)
	num_triples := 200
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
			ch := make(chan uint32)
			rchannels[i][j] = ch
			wchannels[j][i] = ch
		}
	}

	/* final result */
	result := make([]X, n)
	for id := range result {
		inputs := make([]uint32, 1)
		inputs[0] = uint32(id)
		result[id] =
			X{id,
				triples32[id],
				rchannels[id],
				wchannels[id],
				inputs}
	}
	return result
}

func RunExample() uint32 {
	xs := Example(7)
	done := make(chan uint32, len(xs))
	results := make([]uint32, len(xs))
	for _, x := range xs {
		go func(x X) {
			y := Input32(x, 3)
			z := Input32(x, 6)
			done <- Output32(x, And32(x, y, z))
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
