package gmw

import "fmt"
import "math/big"
import "crypto/rand"
import "github.com/tjim/smpcc/runtime/ot"
import "github.com/tjim/smpcc/runtime/base"

var log_mem bool = true
var log_results bool = false
var log_communication bool = false
var check_split = false

const (
	NUM_TRIPLES = 10
)

type Io interface {
	Id() int
	N() int /* number of parties */
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

/* Triple */
type Triple struct {
	a, b, c uint32
}

/* mocked-up implementation of Io */
type X struct {
	n           int /* number of parties */
	id          int /* id of party, range is 0..n-1 */
	triples32   []Triple
	triples8    []struct{ a, b, c uint8 }
	triples1    []struct{ a, b, c bool }
	rchannels   []chan uint32 /* channels for reading from other parties */
	wchannels   []chan uint32 /* channels for writing to other parties */
	inputs      []uint32      /* inputs of this party */
	ram         []byte
	otSenders   []ot.Sender
	otReceivers []ot.Receiver
}

func (x *X) N() int {
	return x.n
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

	done := make(chan bool, 10)
	if len(x.triples32) == 0 {
		fmt.Printf("X.id=%d out of triples, making more\n", x.id)
		x.triples32 = make([]Triple, NUM_TRIPLES)
		for triple_i := 0; triple_i < NUM_TRIPLES; triple_i++ {
			go func(triple_i int) {
				// fmt.Printf("triple_i=%d, len(triples32)=%d\n", triple_i, len(x.triples32))
				x.triples32[triple_i] = triple32Secure(x.n, x.id, x.otSenders, x.otReceivers)
				done <- true
			}(triple_i)
			<-done
		}
	}
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
	// fmt.Printf("Receive32: party=%d, len(rchannels)=%d\n", party, len(x.rchannels))
	ch := x.rchannels[party]
	result, ok := <-ch
	if !ok {
		panic("channel closed")
	}
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
	if check_split {
		var y uint32 = 0
		for _, s := range result {
			y ^= s
		}
		if y != x0 {
			panic("NOT EQUAL")
		}
	}
	return result
}

/* Code assumes that only one of (thisPartyId, otherPartyId) and (otherPartyId, thisPartyId) will be queried */
func triples32TwoParties(num_triples, thisPartyId, otherPartyId int,
	thisSender ot.Sender, thisReceiver ot.Receiver) []Triple {

	fmt.Printf("%d, %d: triples32TwoParties\n", thisPartyId, otherPartyId)

	if thisReceiver == nil {
		panic("this receiver is nil")
	}

	if thisSender == nil {
		panic("this sender is nil")
	}

	result := make([]Triple, num_triples)

	// tripleShareSize := 32
	for i := 0; i < num_triples; i++ {
		var a, b, c uint32
		for j := 0; j < 32; j++ {
			if thisPartyId < otherPartyId {
				// invocation 1 of algorithm 1 from ALSZ13
				a = rand32() % 2
				// fmt.Printf("Party %d is waiting to receive from %d\n", thisPartyId, otherPartyId)
				u_bytes := thisReceiver.Receive(ot.Selector(a))
				// fmt.Printf("Party %d received from %d\n", thisPartyId, otherPartyId)
				u := uint32(u_bytes[0])

				// invocation 2 of algorithm 1 from ALSZ13
				x_0_int := rand32() % 2
				x_0 := []byte{byte(x_0_int)}
				x_1_int := rand32() % 2
				x_1 := []byte{byte(x_1_int)}
				thisSender.Send(x_0, x_1)
				v := x_0_int
				b = x_0_int ^ x_1_int

				// Computing the triple
				c = (a * b) ^ u ^ v
			} else {
				// invocation 2 of algorithm 1 from ALSZ13
				x_0_int := rand32() % 2
				x_0 := []byte{byte(x_0_int)}
				x_1_int := rand32() % 2
				x_1 := []byte{byte(x_1_int)}
				// fmt.Printf("Party %d is waiting to send to %d\n", thisPartyId, otherPartyId)
				thisSender.Send(x_0, x_1)
				// fmt.Printf("Party %d sent to %d\n", thisPartyId, otherPartyId)
				v := x_0_int
				b = x_0_int ^ x_1_int

				// invocation 1 of algorithm 1 from ALSZ13
				a = rand32() % 2
				u_bytes := thisReceiver.Receive(ot.Selector(a))
				u := uint32(u_bytes[0])

				// Computing the triple
				c = (a * b) ^ u ^ v
			}
			result[i] = Triple{(result[i].a << 1) | a, (result[i].b << 1) | b, (result[i].c << 1) | c}
		}
	}

	return result
}

func piMulR(val uint32, thisReceiver ot.Receiver) uint32 {
	result := uint32(0)

	for i := 0; i < 32; i++ {
		a_bit := (val >> uint(i)) % 2
		// fmt.Printf("thisReceiver=%+v\n", thisReceiver)
		u_bytes := thisReceiver.Receive(ot.Selector(a_bit))
		u := uint32(u_bytes[0])
		result |= (u << uint(i))
	}

	return result
}

func piMulS(val uint32, thisSender ot.Sender) uint32 {
	result := uint32(0)

	for i := 0; i < 32; i++ {
		b_bit := (val >> uint(i)) % 2
		x_0_int := rand32() % 2
		x_0 := []byte{byte(x_0_int)}
		x_1_int := x_0_int ^ b_bit
		x_1 := []byte{byte(x_1_int)}
		// fmt.Printf("thisSender=%+v\n", thisSender)
		thisSender.Send(x_0, x_1)
		v := x_0_int

		result |= (v << uint(i))
		// fmt.Printf("result=%d\n", result)
	}

	return result
}

func triple32Secure(n int, thisPartyId int, senders []ot.Sender, receivers []ot.Receiver) Triple {
	// Notation is from figure 9, DO10, and Algorithm 1 from ALSZ13
	var a, b uint32
	a = rand32()
	b = rand32()

	d := make([]uint32, n)
	e := make([]uint32, n)
	// fmt.Printf("triple32Secure: n=%d, thisPartyId=%d, a=%d, b=%d\nd=%v\ne=%v\n", n, thisPartyId, a, b, d, e)

	for i := 0; i < n; i++ {
		if i == thisPartyId {
			continue
		}
		// fmt.Printf("secure triple this=%d, other=%d\n", thisPartyId, i)
		if thisPartyId > i {
			d[i] = piMulR(a, receivers[i])
			e[i] = piMulS(b, senders[i])
		} else {
			e[i] = piMulS(b, senders[i])
			d[i] = piMulR(a, receivers[i])
		}
	}

	result := Triple{a, b, a & b}
	// fmt.Printf("secure triple id=%v res=%+v\n", thisPartyId, result)
	for i := 0; i < n; i++ {
		result.c ^= d[i] ^ e[i]
	}
	// fmt.Printf("post secure triple id=%v res=%+v\n", thisPartyId, result)

	return result
}

/* create n shares of a multiplication triple */
func triple32(n int) []Triple {
	a, b := rand32(), rand32()
	c := a & b
	a_shares := split_uint32(a, n)
	b_shares := split_uint32(b, n)
	c_shares := split_uint32(c, n)
	result := make([]Triple, n)
	for i := range result {
		result[i] = Triple{a_shares[i], b_shares[i], c_shares[i]}
	}
	return result
}

// var num_triples int = 16 * 4096

var num_triples int = 10

// var num_triples int = 10

func UsedTriples32(x *X) int {
	return num_triples - len(x.triples32)
}

func Example(n int) []*X {
	/* OT based triples */
	otRChannels := make([][]ot.Receiver, n)
	otSChannels := make([][]ot.Sender, n)
	for i := range otRChannels {
		otRChannels[i] = make([]ot.Receiver, n)
		otSChannels[i] = make([]ot.Sender, n)
	}

	fmt.Println("Creating pairwise OT channels.")
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			if i == j {
				continue
			}
			otChans := base.OTChans{
				make(chan ot.PublicKey, 5),
				make(chan big.Int, 5),
				make(chan ot.HashedElGamalCiph, 5),

				make(chan []byte, 5),
				make(chan ot.Selector, 5),
				make(chan ot.NPReceiverParams, 5),
			}

			sndParams, rcvParams := ot.GenNPParams()
			npRecvr := ot.NewNPReceiver(rcvParams, otChans.NpSendPk, otChans.NpRecvPk, otChans.NpSendEncs)
			otSChannels[i][j] = ot.NewExtendSender(otChans.OtExtChan, otChans.OtExtSelChan, npRecvr, ot.SEC_PARAM, ot.NUM_PAIRS)
			npSndr := ot.NewNPSender(sndParams, otChans.NpSendPk, otChans.NpRecvPk, otChans.NpSendEncs)
			otRChannels[j][i] = ot.NewExtendReceiver(otChans.OtExtChan, otChans.OtExtSelChan, npSndr, ot.SEC_PARAM, ot.NUM_PAIRS)
		}
	}

	// TRIPLE TEST DO NOT REMOVE

	// done := make(chan bool, 10)
	// var t1, t2, t3 Triple
	// go func() {
	// 	t1 = triple32Secure(3, 0, otSChannels[0], otRChannels[0])
	// 	t1.a = t1.a
	// 	t1.b = t1.b
	// 	t1.c = t1.c
	// 	done <- true
	// }()
	// go func() {
	// 	t2 = triple32Secure(3, 1, otSChannels[1], otRChannels[1])
	// 	t2.a = t2.a
	// 	t2.b = t2.b
	// 	t2.c = t2.c
	// 	done <- true
	// }()
	// go func() {
	// 	t3 = triple32Secure(3, 2, otSChannels[2], otRChannels[2])
	// 	t3.a = t3.a
	// 	t3.b = t3.b
	// 	t3.c = t3.c
	// 	done <- true
	// }()
	// <-done
	// <-done
	// <-done

	// for i := 0; i < 32; i++ {
	// 	a1 := (t1.a >> uint(i)) % 2
	// 	b1 := (t1.b >> uint(i)) % 2
	// 	c1 := (t1.c >> uint(i)) % 2

	// 	a2 := (t2.a >> uint(i)) % 2
	// 	b2 := (t2.b >> uint(i)) % 2
	// 	c2 := (t2.c >> uint(i)) % 2

	// 	a3 := (t3.a >> uint(i)) % 2
	// 	b3 := (t3.b >> uint(i)) % 2
	// 	c3 := (t3.c >> uint(i)) % 2

	// 	if ((a1 ^ a2 ^ a3) & (b1 ^ b2 ^ b3)) != (c1 ^ c2 ^ c3) {
	// 		panic("triple check failed")
	// 	}
	// }
	// // fmt.Printf("t1=%+v\nt2=%+v\n", t1, t2)
	// fmt.Println("Triples check passed")

	// END TRIPLE TEST

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
			&X{n,
				id,
				make([]Triple, 0),
				make([]struct{ a, b, c uint8 }, 0),
				make([]struct{ a, b, c bool }, 0),
				rchannels[id],
				wchannels[id],
				inputs,
				nil,
				otSChannels[id],
				otRChannels[id],
			}
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
