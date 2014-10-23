package gmw

import "fmt"
import "math/big"
import "crypto/rand"
import "github.com/tjim/smpcc/runtime/ot"
import "net"
import "log"
import "github.com/tjim/fatchan"

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
		//		fmt.Printf("X.id=%d out of triples, making more\n", x.id)
		x.triples32 = make([]Triple, NUM_TRIPLES)
		for triple_i := 0; triple_i < NUM_TRIPLES; triple_i++ {
			go func(triple_i int) {
				// fmt.Printf("triple_i=%d, len(triples32)=%d\n", triple_i, len(x.triples32))
				x.triples32[triple_i] = triple32Secure(x.n, x.id, x.otSenders, x.otReceivers)
				done <- true
			}(triple_i)
			<-done // NB BUG this forces all the goroutines to run in sequence
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
		ch <- n32 // NB channel is buffered, shouldn't block
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
		ch <- uint32(n) // NB channel is buffered, shouldn't block
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
		ch <- n // NB channel is buffered, shouldn't block
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
	ch <- n32 // NB channel is buffered, shouldn't block
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

	//	fmt.Printf("%d, %d: triples32TwoParties\n", thisPartyId, otherPartyId)

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
	// Notation is from Figure 9 (p11) of
	//
	// "Multiparty Computation for Dishonest Majority: from Passive to Active Security at Low Cost"
	// by Damgard and Orlandi
	// http://eprint.iacr.org/2010/318
	//
	// and Algorithm 1 from
	//
	// "More Efficient Oblivious Transfer and Extensions for Faster Secure Computation"
	// Gilad Asharov and Yehuda Lindell and Thomas Schneider and Michael Zohner
	// http://eprint.iacr.org/2013/552

	var a, b uint32
	a = rand32()
	b = rand32()

	d := make([]uint32, n)
	e := make([]uint32, n)
	//	fmt.Printf("triple32Secure: n=%d, thisPartyId=%d, a=%032b, b=%032b\nd=%v\ne=%v\n", n, thisPartyId, a, b, d, e)

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
	//fmt.Printf("secure triple id=%v res=%+v\n", thisPartyId, result)
	for i := 0; i < n; i++ {
		result.c ^= d[i] ^ e[i]
	}
	//	fmt.Printf("[%d] a = 0x%032b\n", thisPartyId, result.a)
	//	fmt.Printf("[%d] b = 0x%032b\n", thisPartyId, result.b)
	//	fmt.Printf("[%d] c = 0x%032b\n", thisPartyId, result.c)

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

	//	fmt.Println("Creating pairwise OT channels.")
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			if i == j {
				continue
			}
			npchans := ot.NPChans{
				make(chan big.Int),
				make(chan big.Int),
				make(chan ot.HashedElGamalCiph),
			}
			extchans := ot.ExtChans{
				make(chan []byte),
				make(chan ot.Selector),
			}
			baseReceiver := ot.NewNPReceiver(npchans.ParamChan, npchans.NpRecvPk, npchans.NpSendEncs)
			otSChannels[i][j] = ot.NewExtendSender(extchans.OtExtChan, extchans.OtExtSelChan, baseReceiver, ot.SEC_PARAM, ot.NUM_PAIRS)
			baseSender := ot.NewNPSender(npchans.ParamChan, npchans.NpRecvPk, npchans.NpSendEncs)
			otRChannels[j][i] = ot.NewExtendReceiver(extchans.OtExtChan, extchans.OtExtSelChan, baseSender, ot.SEC_PARAM, ot.NUM_PAIRS)
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

type GlobalIO struct {
	n      int      /* number of parties */
	id     int      /* id of party, range is 0..n-1 */
	inputs []uint32 /* inputs of this party */
	ram    []byte
}

type BlockIO struct {
	*GlobalIO
	triples32   []Triple
	triples8    []struct{ a, b, c uint8 }
	triples1    []struct{ a, b, c bool }
	rchannels   []chan uint32
	wchannels   []chan uint32
	otSenders   []ot.StreamSender
	otReceivers []ot.StreamReceiver
}

type PeerIO struct {
	*GlobalIO // The GlobalIO of the peer and all of its blocks must be the same
	blocks    []BlockIO
}

/*
Each peer runs a server, it accepts requests that set up Receiver and rchannel.
Each peer connects to all other peers and establishes Sender and wchannel.

Port strategy:
Each peer listens to n-1 ports: base port + n*id, plus next n-1 ports, ignoring own port.
Each peer connects to n-1 ports: for i<n, base port + n*i + id
*/

// for establishing connections
type PerBlock struct {
	ot.PerBlockStreamChans
	rchannel chan uint32
}

type PerNodePair struct {
	ot.NPChans
	BlockChans []PerBlock
}

const (
	base_port int = 3042
)

func (io PeerIO) connect(party int) chan<- PerNodePair {
	if io.id == party {
		panic("connect0")
	}
	addr := fmt.Sprintf("127.0.0.1:%d", base_port+(io.n*party)+io.id)
	server, err := net.Dial("tcp", addr)
	if err != nil {
		log.Fatalf("dial(%q): %s", addr, err)
	}

	xport := fatchan.New(server, nil)
	nu := make(chan PerNodePair)
	xport.FromChan(nu)
	return nu
}

func clientSideIOSetup(blocks []BlockIO, party int, nu chan<- PerNodePair) {
	numBlocks := len(blocks)

	ParamChan := make(chan big.Int)
	NpRecvPk := make(chan big.Int)
	NpSendEncs := make(chan ot.HashedElGamalCiph)
	x := PerNodePair{ot.NPChans{ParamChan, NpRecvPk, NpSendEncs}, make([]PerBlock, numBlocks)}

	baseReceiver := ot.NewNPReceiver(ParamChan, NpRecvPk, NpSendEncs)

	for i := 0; i < numBlocks; i++ {
		S2R := make(chan ot.MessagePair)
		R2S := make(chan []byte)
		rchannel := make(chan uint32)
		x.BlockChans[i] = PerBlock{ot.PerBlockStreamChans{S2R, R2S}, rchannel}
		blocks[i].rchannels[party] = rchannel
		//		fmt.Printf("blocks[%d].rchannels[%d] = %v\n", i, party, rchannel)
	}
	nu <- x
	sender0 := ot.NewStreamSender(baseReceiver, nil, nil)
	for i := 0; i < numBlocks; i++ {
		blocks[i].otSenders[party] = sender0.Fork(x.BlockChans[i].S2R, x.BlockChans[i].R2S)
	}
}

var done chan struct{} = make(chan struct{})

func (io PeerIO) listen(party int) <-chan PerNodePair {
	if io.id == party {
		panic("listen0")
	}
	addr := fmt.Sprintf("127.0.0.1:%d", base_port+(io.n*io.id)+party)
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		log.Fatalf("listen(%q): %s", addr, err)
	}
	conn, err := listener.Accept()
	if err != nil {
		log.Fatalf("accept(): %s", err)
	}
	xport := fatchan.New(conn, nil)
	nu := make(chan PerNodePair)
	xport.ToChan(nu)
	return nu
}

func serverSideIOSetup(blocks []BlockIO, party int, nu <-chan PerNodePair) {
	numBlocks := len(blocks)

	x := <-nu
	if numBlocks != len(x.BlockChans) {
		panic("Block mismatch")
	}
	baseSender := ot.NewNPSender(x.NPChans.ParamChan, x.NPChans.NpRecvPk, x.NPChans.NpSendEncs)
	receiver0 := ot.NewStreamReceiver(baseSender, nil, nil)

	for i, v := range x.BlockChans {
		receiver := receiver0.Fork(v.PerBlockStreamChans.R2S, v.PerBlockStreamChans.S2R)
		blocks[i].otReceivers[party] = receiver
		blocks[i].wchannels[party] = v.rchannel
		//		fmt.Printf("blocks[%d].wchannels[%d] = %v\n", i, party, v.rchannel)
	}
	done <- struct{}{}
}

func NewPeerIO(numBlocks int, numParties int, id int) PeerIO {
	var gio GlobalIO
	gio.n = numParties
	gio.id = id
	var io PeerIO
	io.GlobalIO = &gio
	io.blocks = make([]BlockIO, numBlocks+1) // one extra BlockIO for main loop
	for i := range io.blocks {
		io.blocks[i].GlobalIO = io.GlobalIO
		io.blocks[i].rchannels = make([]chan uint32, numParties)
		io.blocks[i].wchannels = make([]chan uint32, numParties)
		io.blocks[i].otSenders = make([]ot.StreamSender, numParties)
		io.blocks[i].otReceivers = make([]ot.StreamReceiver, numParties)
	}
	return io
}

func SetupPeer(numBlocks int, numParties int, id int) {
	io := NewPeerIO(numBlocks, numParties, id)
	for i := 0; i < numParties; i++ {
		if io.id != i {
			go serverSideIOSetup(io.blocks, i, io.listen(i))
		}
	}
	for i := 0; i < numParties; i++ {
		if io.id != i {
			clientSideIOSetup(io.blocks, i, io.connect(i))
		}
	}
	for i := 0; i < numParties; i++ {
		if io.id != i {
			<-done
		}
	}
	// May need to sleep here to avoid fatchan deadlock
}

func Simulation(numParties int, numBlocks int, runPeer func(Io, []Io), peerDone <-chan bool) {
	ios := make([]PeerIO, numParties)
	for i := 0; i < numParties; i++ {
		ios[i] = NewPeerIO(numBlocks, numParties, i)
	}
	nus := make([]chan PerNodePair, numParties*numParties)
	for i := 0; i < numParties; i++ {
		for j := 0; j < numParties; j++ {
			if i != j {
				nu := make(chan PerNodePair)
				nus[i*numParties+j] = nu                   // server i talking to client j
				go serverSideIOSetup(ios[i].blocks, j, nu) // i's setup server for party j
			}
		}
	}
	for i := 0; i < numParties; i++ {
		for j := 0; j < numParties; j++ {
			if i != j {
				nu := nus[j*numParties+i]               // client i talking to server j
				clientSideIOSetup(ios[i].blocks, j, nu) // i's setup client for party j
			}
		}
	}
	for i := 0; i < numParties; i++ {
		for j := 0; j < numParties; j++ {
			if i != j {
				<-done // wait for a setup server to finish
			}
		}
	}
	// all setup clients and servers have finished
	for i := 0; i < numParties; i++ {
		// copy ios[i].blocks[1:] to make an []Io; []BlockIO is not []Io
		x := make([]Io, numBlocks)
		for j := 0; j < numBlocks; j++ {
			x[j] = ios[i].blocks[j+1]
		}
		go runPeer(ios[i].blocks[0], x)
	}
	for i := 0; i < numParties; i++ {
		<-peerDone // wait for all peers to complete
	}
}

func (x GlobalIO) N() int {
	return x.n
}

func (x GlobalIO) Id() int {
	return x.id
}

func (x BlockIO) Triple1() (a, b, c bool) {
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

func (x BlockIO) Triple8() (a, b, c uint8) {
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

func spread(val uint32) []byte {
	arr := make([]byte, 4)
	for i := range arr {
		arr[i] = byte(val >> uint(i*8))
	}
	return arr
}

func combine(arr []byte) uint32 {
	if len(arr) != 4 {
		panic("combine")
	}
	result := uint32(0)
	for i,v := range arr {
		result |= uint32(v) << uint(i*8)
	}
	return result
}

func piMulRStream(val []byte, thisReceiver ot.StreamReceiver) []byte {
	return thisReceiver.ReceiveBitwise(val)
}

func piMulSStream(val []byte, thisSender ot.StreamSender) []byte {
	x0 := randomBytes(len(val))
	x1 := ot.XorBytes(x0, val)
	thisSender.SendBitwise(x0, x1)
	return x0
}

func randomBytes(numBytes int) []byte {
	result := make([]byte, numBytes)
	_, err := rand.Read(result)
	if err != nil {
		panic("random number generation")
	}
	return result
}

func AndBytes(a, b []byte) []byte {
	if len(a) != len(b) {
		panic("AndBytes")
	}
	result := make([]byte, len(a))
	for i, v := range a {
		result[i] = v & b[i]
	}
	return result
}

func triple32Stream(thisPartyId int, senders []ot.StreamSender, receivers []ot.StreamReceiver) []Triple {
	n := len(senders)
	result := make([]Triple, NUM_TRIPLES)
	numBytes := NUM_TRIPLES * 4
	// Notation is from Figure 9 (p11) of
	//
	// "Multiparty Computation for Dishonest Majority: from Passive to Active Security at Low Cost"
	// by Damgard and Orlandi
	// http://eprint.iacr.org/2010/318
	//
	// and Algorithm 1 from
	//
	// "More Efficient Oblivious Transfer and Extensions for Faster Secure Computation"
	// Gilad Asharov and Yehuda Lindell and Thomas Schneider and Michael Zohner
	// http://eprint.iacr.org/2013/552

	a := randomBytes(numBytes)
	b := randomBytes(numBytes)

	d := make([][]byte, n)
	e := make([][]byte, n)
	//	fmt.Printf("triple32Secure: n=%d, thisPartyId=%d, a=%032b, b=%032b\nd=%v\ne=%v\n", n, thisPartyId, a, b, d, e)

	for i := 0; i < n; i++ {
		if i == thisPartyId {
			continue
		}
		// fmt.Printf("secure triple this=%d, other=%d\n", thisPartyId, i)
		if thisPartyId > i {
			d[i] = piMulRStream(a, receivers[i])
			e[i] = piMulSStream(b, senders[i])
		} else {
			e[i] = piMulSStream(b, senders[i])
			d[i] = piMulRStream(a, receivers[i])
		}
	}

	c := AndBytes(a,b)
	for i := 0; i < n; i++ {
		if i == thisPartyId {
			continue
		}
		c = ot.XorBytes(c,
			ot.XorBytes(d[i], e[i]))
	}

	for i := range result {
		result[i] = Triple{combine(a[:4]), combine(b[:4]), combine(c[:4])}
		a = a[4:]
		b = b[4:]
		c = c[4:]
	}
	//fmt.Printf("[%d] a = 0x%032b\n", thisPartyId, result.a)
	//fmt.Printf("[%d] b = 0x%032b\n", thisPartyId, result.b)
	//fmt.Printf("[%d] c = 0x%032b\n", thisPartyId, result.c)

	return result
}

func (x BlockIO) Triple32() (a, b, c uint32) {

	if len(x.triples32) == 0 {
		//		fmt.Printf("X.id=%d out of triples, making more\n", x.id)
		x.triples32 = triple32Stream(x.id, x.otSenders, x.otReceivers)
	}
	result := x.triples32[0]
	x.triples32 = x.triples32[1:]
	return result.a, result.b, result.c
}

func (x BlockIO) Triple64() (a, b, c uint64) {
	a0, b0, c0 := x.Triple32()
	a1, b1, c1 := x.Triple32()
	return (uint64(a0) << 32) | uint64(a1), (uint64(b0) << 32) | uint64(b1), (uint64(c0) << 32) | uint64(c1)
}

func (x BlockIO) Open1(s bool) bool {
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

func (x BlockIO) Open8(s uint8) uint8 {
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

func (x BlockIO) Open32(s uint32) uint32 {
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

func (x BlockIO) Open64(s uint64) uint64 {
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

func (x BlockIO) Broadcast1(n bool) {
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
		// goroutine to avoid deadlock, all nodes broadcast then receive simultaneously
		go func(ch chan uint32) { ch <- n32 }(ch)
		if log_communication {
			fmt.Printf("%d -- 0x%1x -> %d\n", id, n32, i)
		}
	}
}

func (x BlockIO) Broadcast8(n uint8) {
	id := x.Id()
	if log_communication {
		fmt.Printf("%d: BROADCAST 0x%02x\n", id, n)
	}
	for i, ch := range x.wchannels {
		if i == id {
			continue
		}
		// goroutine to avoid deadlock, all nodes broadcast then receive simultaneously
		go func(ch chan uint32) { ch <- uint32(n) }(ch)
		if log_communication {
			fmt.Printf("%d -- 0x%02x -> %d\n", id, n, i)
		}
	}
}

func (x BlockIO) Broadcast32(n uint32) {
	id := x.Id()
	if log_communication {
		fmt.Printf("%d: BROADCAST 0x%08x\n", id, n)
	}
	for i, ch := range x.wchannels {
		if i == id {
			continue
		}
		// goroutine to avoid deadlock, all nodes broadcast then receive simultaneously
		go func(ch chan uint32) { ch <- n }(ch)
		if log_communication {
			fmt.Printf("%d -- 0x%08x -> %d\n", id, n, i)
		}
	}
}

func (x BlockIO) Broadcast64(n uint64) {
	n0 := uint32(n >> 32)
	n1 := uint32(n)
	x.Broadcast32(n0)
	x.Broadcast32(n1)
}

func (x BlockIO) Send1(party int, n bool) {
	var n32 uint32 = 0
	if n {
		n32 = 1
	}
	x.Send32(party, n32)
}

func (x BlockIO) Send8(party int, n uint8) {
	var n32 uint32 = uint32(n)
	id := x.Id()
	if party == id {
		return
	}
	x.Send32(party, n32)
}

func (x BlockIO) Send32(party int, n32 uint32) {
	id := x.Id()
	if party == id {
		return
	}
	ch := x.wchannels[party]
	ch <- n32 // NB nodes don't send32() simultaneously so no goroutine needed
	if log_communication {
		fmt.Printf("%d -- 0x%1x -> %d\n", id, n32, party)
	}
}

func (x BlockIO) Send64(party int, n uint64) {
	n0 := uint32(n >> 32)
	n1 := uint32(n)
	x.Send32(party, n0)
	x.Send32(party, n1)
}

func (x BlockIO) Receive1(party int) bool {
	result := x.Receive32(party)
	return result > 0
}

func (x BlockIO) Receive8(party int) uint8 {
	result := x.Receive32(party)
	return uint8(result)
}

func (x BlockIO) Receive32(party int) uint32 {
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

func (x BlockIO) Receive64(party int) uint64 {
	n0 := x.Receive32(party)
	n1 := x.Receive32(party)
	return (uint64(n0) << 32) | uint64(n1)
}

func (x BlockIO) GetInput() uint32 {
	result := x.inputs[0]
	x.inputs = x.inputs[1:]
	return result
}

func (x BlockIO) InitRam(contents []byte) {
	x.ram = contents
}

func (x BlockIO) Ram() []byte {
	return x.ram
}
