package spdz

import "crypto/rand"
import "math/big"
import "fmt"

var print_masks bool = false

type Share struct {
	Val uint32
	Mac uint32
}

type Io interface {
	Id() int
	Alpha() uint32
	Triple() (a, b, c Share)
	Mask(int) (r Share)
	MaskOpen() (r Share, R uint32)
	Open(Share) uint32
	GetInput() uint32
	Broadcast(uint32)
	Receive(party int) uint32
	MACCheck() bool
}

type X struct {
	id        int                       /* id of party, range is 0..n-1 */
	alpha     uint32                    /* share of mac as per SPDZ paper */
	triples   []struct{ a, b, c Share } /* multiplication triples */
	masks     [][]Share                 /* input masks, indexed by party providing the input */
	openmasks []uint32                  /* unmasked input mask values for this party */
	rchannels []chan uint32             /* channels for reading from other parties */
	wchannels []chan uint32             /* channels for writing to other parties */
	inputs    []uint32                  /* inputs of this party */
}

/* return a slice of n random uint32 values that sum to x */
func split_uint32(x uint32, n int) []uint32 {
	if n <= 0 {
		panic("Error: split")
	}
	result := make([]uint32, n)
	for i := 1; i < n; i++ {
		xi := rand32()
		x -= xi
		result[i] = xi
	}
	result[0] = x
	var y uint32 = 0
	for _, s := range result {
		y += s
	}
	return result
}

func shares(x uint32, n int, alpha uint32) []Share {
	mx := alpha * x
	x_split := split_uint32(x, n)
	mx_split := split_uint32(mx, n)
	result := make([]Share, n)
	for i, _ := range x_split {
		result[i] = Share{x_split[i], mx_split[i]}
	}
	return result
}

/* create n shares of a multiplication triple */
func triple(n int, alpha uint32) []struct{ a, b, c Share } {
	a, b := rand32(), rand32()
	c := a * b
	a_shares := shares(a, n, alpha)
	b_shares := shares(b, n, alpha)
	c_shares := shares(c, n, alpha)
	result := make([]struct{ a, b, c Share }, n)
	for i, _ := range result {
		result[i] = struct{ a, b, c Share }{a_shares[i], b_shares[i], c_shares[i]}
	}
	return result
}

func mask(n int, alpha uint32) (R uint32, r []Share) {
	R = rand32()
	r = shares(R, n, alpha)
	if print_masks {
		fmt.Printf("Mask 0x%08x = 0", R)
		for _, s := range r {
			fmt.Printf(" + 0x%08x", s.Val)
		}
		fmt.Printf("\n")
	}
	return
}

func rand32() uint32 {
	max := big.NewInt(1 << 32)
	x, err := rand.Int(rand.Reader, max)
	if err != nil {
		panic("Error: random number generation")
	}
	return uint32(x.Int64())
}

func Example(n int) []X {
	alpha := rand32()
	alphas := split_uint32(alpha, n)

	/* triples */
	triples := make([][]struct{ a, b, c Share }, n)
	num_triples := 200
	for i, _ := range triples {
		triples[i] = make([]struct{ a, b, c Share }, num_triples)
	}
	for j := 0; j < num_triples; j++ {
		shares_of_a_triple := triple(n, alpha)
		for i, t := range shares_of_a_triple {
			triples[i][j] = t
		}
	}

	/* masks */
	masks := make([] /*n*/ [] /*n*/ [] /*10*/ Share, n)
	for i := range masks {
		masks[i] = make([] /*n*/ [] /*10*/ Share, n)
		for j := range masks[i] {
			masks[i][j] = make([] /*10*/ Share, 10)
		}
	}
	openmasks := make([] /*n*/ [] /*10*/ uint32, n)
	for i := range openmasks {
		openmasks[i] = make([] /*10*/ uint32, 10)
	}
	for k := range masks[0][0] {
		for i := range masks {
			R, r := mask(n, alpha)
			for j := range r {
				openmasks[i][k] = R
				masks[j][i][k] = r[j]
			}
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
				alphas[id],
				triples[id],
				masks[id],
				openmasks[id],
				rchannels[id],
				wchannels[id],
				inputs}
	}
	return result
}

func (x X) Id() int {
	return x.id
}

func (x X) Alpha() uint32 {
	return x.alpha
}

func (x X) Triple() (a, b, c Share) {
	hd := x.triples[0]
	x.triples = x.triples[1:]
	a, b, c = hd.a, hd.b, hd.c
	return
}

func (x X) Mask(party int) Share {
	r := x.masks[party][0]
	x.masks[party] = x.masks[party][1:]
	return r
}

func (x X) MaskOpen() (Share, uint32) {
	party := x.id
	r := x.masks[party][0]
	x.masks[party] = x.masks[party][1:]
	R := x.openmasks[0]
	x.openmasks = x.openmasks[1:]
	return r, R
}

func (x X) Open(s Share) uint32 {
	x.Broadcast(s.Val)
	result := s.Val
	id := x.Id()
	for i := range x.rchannels {
		if i == id {
			continue
		}
		result += x.Receive(i)
	}
	return result
}

func (x X) GetInput() uint32 {
	result := x.inputs[0]
	x.inputs = x.inputs[1:]
	return result
}

func (x X) Broadcast(n uint32) {
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

func (x X) Receive(party int) uint32 {
	id := x.Id()
	if party == id {
		return 0
	}
	ch := x.rchannels[party]
	result := <-ch
	fmt.Printf("%d <- 0x%08x -- %d\n", id, result, party)
	return result
}

func (x X) MACCheck() bool {
	// TODO
	return true
}

func Input(io Io, party int) Share {
	id := io.Id()
	if id == party {
		X := io.GetInput()
		r, R := io.MaskOpen()
		delta := X - R
		io.Broadcast(delta)
		return Share{r.Val + delta, r.Mac + io.Alpha()*delta}
	} else {
		r := io.Mask(party)
		delta := io.Receive(party)
		return Share{r.Val, r.Mac + io.Alpha()*delta}
	}
}

func Add(io Io, x, y Share) Share {
	return Share{x.Val + y.Val, x.Mac + y.Mac}
}

func Mul(io Io, x, y Share) Share {
	a, b, c := io.Triple()
	e := io.Open(Share{x.Val - a.Val, x.Mac - a.Mac})
	r := io.Open(Share{y.Val - b.Val, y.Mac - b.Mac})
	if io.Id() == 0 {
		return Share{c.Val + e*b.Val + r*a.Val + e*r, c.Mac + e*b.Mac + r*a.Mac + io.Alpha()*e*r}
	} else {
		return Share{c.Val + e*b.Val + r*a.Val, c.Mac + e*b.Mac + r*a.Mac + io.Alpha()*e*r}
	}
}

func Output(io Io, x Share) uint32 {
	result := io.Open(x)
	if io.MACCheck() {
		fmt.Printf("%d: RESULT 0x%08x\n", io.Id(), result)
		return result
	} else {
		panic("Error: MACCheck failed")
	}
}

func RunExample() uint32 {
	xs := Example(3)
	done := make(chan uint32, len(xs))
	results := make([]uint32, len(xs))
	for _, x := range xs {
		go func(x X) {
			y := Input(x, 2)
			//			Output(x, y)
			z := Mul(x, y, y)
			done <- Output(x, z)
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
	return result
}
