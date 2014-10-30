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

/* Share of a multiplication triple */
type Triple struct {
	a, b, c uint32
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
	otSenders   []*ot.StreamSender
	otReceivers []*ot.StreamReceiver
}

type PeerIO struct {
	*GlobalIO // The GlobalIO of the peer and all of its blocks must be the same
	blocks    []*BlockIO
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
	stchannel []ot.PerBlockStreamChans
	rwchannel []chan uint32
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

var done chan struct{} = make(chan struct{})

func clientSideIOSetup(peer *PeerIO, party int, nu chan<- PerNodePair) {
	blocks := peer.blocks
	numBlocks := len(blocks)

	ParamChan := make(chan big.Int)
	NpRecvPk := make(chan big.Int)
	NpSendEncs := make(chan ot.HashedElGamalCiph)
	x := PerNodePair{ot.NPChans{ParamChan, NpRecvPk, NpSendEncs}, make([]PerBlock, numBlocks)}

	for i := 0; i < numBlocks; i++ {
		rchannel := make(chan uint32)
		wchannel := make(chan uint32)
		x.BlockChans[i] = PerBlock{
			[]ot.PerBlockStreamChans{
				ot.PerBlockStreamChans{make(chan ot.MessagePair), make(chan []byte)},
				ot.PerBlockStreamChans{make(chan ot.MessagePair), make(chan []byte)}},
			[]chan uint32{rchannel, wchannel}}
		//		fmt.Printf("blocks[%d].rchannels[%d] = %v\n", i, party, rchannel)
	}
	nu <- x

	for i := 0; i < numBlocks; i++ {
		blocks[i].rchannels[party] = x.BlockChans[i].rwchannel[0]
		blocks[i].wchannels[party] = x.BlockChans[i].rwchannel[1]
	}

	baseReceiver := ot.NewNPReceiver(ParamChan, NpRecvPk, NpSendEncs)
	sender0 := ot.NewStreamSender(baseReceiver, x.BlockChans[0].stchannel[0].S2R, x.BlockChans[0].stchannel[0].R2S)
	blocks[0].otSenders[party] = sender0
	for i := 1; i < numBlocks; i++ {
		sender := sender0.Fork(x.BlockChans[i].stchannel[0].S2R, x.BlockChans[i].stchannel[0].R2S)
		blocks[i].otSenders[party] = sender
	}

	receiver0 := ot.NewStreamReceiver(sender0, x.BlockChans[0].stchannel[1].R2S, x.BlockChans[0].stchannel[1].S2R)
	blocks[0].otReceivers[party] = receiver0

	for i := 1; i < numBlocks; i++ {
		receiver := receiver0.Fork(x.BlockChans[i].stchannel[1].R2S, x.BlockChans[i].stchannel[1].S2R)
		blocks[i].otReceivers[party] = receiver
	}

	done <- struct{}{}
}

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

func serverSideIOSetup(peer *PeerIO, party int, nu <-chan PerNodePair) {
	blocks := peer.blocks
	numBlocks := len(blocks)

	x := <-nu
	if numBlocks != len(x.BlockChans) {
		panic("Block mismatch")
	}

	for i := 0; i < numBlocks; i++ {
		blocks[i].wchannels[party] = x.BlockChans[i].rwchannel[0]
		blocks[i].rchannels[party] = x.BlockChans[i].rwchannel[1]
	}

	baseSender := ot.NewNPSender(x.NPChans.ParamChan, x.NPChans.NpRecvPk, x.NPChans.NpSendEncs)
	receiver0 := ot.NewStreamReceiver(baseSender, x.BlockChans[0].stchannel[0].R2S, x.BlockChans[0].stchannel[0].S2R)
	blocks[0].otReceivers[party] = receiver0
	for i := 1; i < numBlocks; i++ {
		receiver := receiver0.Fork(x.BlockChans[i].stchannel[0].R2S, x.BlockChans[i].stchannel[0].S2R)
		blocks[i].otReceivers[party] = receiver
	}

	sender0 := ot.NewStreamSender(receiver0, x.BlockChans[0].stchannel[1].S2R, x.BlockChans[0].stchannel[1].R2S)
	blocks[0].otSenders[party] = sender0

	for i := 1; i < numBlocks; i++ {
		sender := sender0.Fork(x.BlockChans[i].stchannel[1].S2R, x.BlockChans[i].stchannel[1].R2S)
		blocks[i].otSenders[party] = sender
	}

	done <- struct{}{}
}

func NewPeerIO(numBlocks int, numParties int, id int) *PeerIO {
	var gio GlobalIO
	gio.n = numParties
	gio.id = id
	var io PeerIO
	io.GlobalIO = &gio
	io.blocks = make([]*BlockIO, numBlocks+1) // one extra BlockIO for main loop
	for i := range io.blocks {
		io.blocks[i] = &BlockIO{
			io.GlobalIO,
			nil, nil, nil,
			make([]chan uint32, numParties),
			make([]chan uint32, numParties),
			make([]*ot.StreamSender, numParties),
			make([]*ot.StreamReceiver, numParties),
		}
	}
	return &io
}

func SetupPeer(numBlocks int, numParties int, id int) {
	io := NewPeerIO(numBlocks, numParties, id)
	for i := 0; i < numParties; i++ {
		if io.id != i {
			if io.Leads(i) {
				go clientSideIOSetup(io, i, io.connect(i))
			} else {
				go serverSideIOSetup(io, i, io.listen(i))
			}
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
	ios := make([]*PeerIO, numParties)
	for i := 0; i < numParties; i++ {
		ios[i] = NewPeerIO(numBlocks, numParties, i)
	}
	nus := make([]chan PerNodePair, numParties*numParties)
	for i := 0; i < numParties; i++ {
		for j := 0; j < numParties; j++ {
			if i != j && !ios[i].Leads(j) {
				nu := make(chan PerNodePair)
				nus[i*numParties+j] = nu            // server i talking to client j
				go serverSideIOSetup(ios[i], j, nu) // i's setup server for party j
			}
		}
	}
	for i := 0; i < numParties; i++ {
		for j := 0; j < numParties; j++ {
			if i != j && ios[i].Leads(j) {
				nu := nus[j*numParties+i]           // client i talking to server j
				go clientSideIOSetup(ios[i], j, nu) // i's setup client for party j
			}
		}
	}
	for i := 0; i < numParties; i++ {
		for j := 0; j < numParties; j++ {
			if i != j {
				<-done // wait for a setup to finish
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

func (x *GlobalIO) N() int {
	return x.n
}

func (x *GlobalIO) Leads(y int) bool {
	// Leader calculation
	// We have N parties 0...N-1
	// Two parties A!=B want to determine a leader.
	// Think of the parties arranged in a circle.
	// Each party "follows" the other in the circle by a certain distance, or, equivalently, "leads" the other by a distance.
	// The lead of A over B is the smallest number x>=0 such that B+x mod N = A.
	// The leader of A and B is A if the lead of A is **less** than the lead of B.  (B is following closer.)
	// E.g., if we have 0 .. B A .. N then A leads B by 1 and B leads A by N-1.
	// Sometimes if N is even we will need to break a tie.  If so we choose the smaller of A and B as the leader.
	//
	// How to calculate the lead of A over B:
	//  if A>B then it is A-B
	//  if A<B then it is (N-B)+A = (A-B)+N
	//  if A=B then it is 0 (degenerate case)
	leadOfX := x.id - y
	if leadOfX < 0 {
		leadOfX += x.n
	}
	leadOfY := y - x.id
	if leadOfY < 0 {
		leadOfY += x.n
	}
	switch {
	case leadOfX < leadOfY:
		return true
	case leadOfX > leadOfY:
		return false
	default:
		return x.id < y
	}
}

func (x *GlobalIO) Id() int {
	return x.id
}

func (x *BlockIO) Triple1() (a, b, c bool) {
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

func (x *BlockIO) Triple8() (a, b, c uint8) {
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
	for i, v := range arr {
		result |= uint32(v) << uint(i*8)
	}
	return result
}

func piMulRStream(val []byte, thisReceiver *ot.StreamReceiver) []byte {
	return thisReceiver.ReceiveMBits(val)
}

func piMulSStream(val []byte, thisSender *ot.StreamSender) []byte {
	x0 := randomBytes(len(val))
	x1 := ot.XorBytes(x0, val)
	thisSender.SendMBits(x0, x1)
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

func triple32Stream(thisPartyId int, senders []*ot.StreamSender, receivers []*ot.StreamReceiver) []Triple {
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

	c := AndBytes(a, b)
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

func (x *BlockIO) Triple32() (a, b, c uint32) {

	if len(x.triples32) == 0 {
		//		fmt.Printf("X.id=%d out of triples, making more\n", x.id)
		x.triples32 = triple32Stream(x.id, x.otSenders, x.otReceivers)
	}
	result := x.triples32[0]
	x.triples32 = x.triples32[1:]
	return result.a, result.b, result.c
}

func (x *BlockIO) Triple64() (a, b, c uint64) {
	a0, b0, c0 := x.Triple32()
	a1, b1, c1 := x.Triple32()
	return (uint64(a0) << 32) | uint64(a1), (uint64(b0) << 32) | uint64(b1), (uint64(c0) << 32) | uint64(c1)
}

func (x *BlockIO) Open1(s bool) bool {
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

func (x *BlockIO) Open8(s uint8) uint8 {
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

func (x *BlockIO) Open32(s uint32) uint32 {
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

func (x *BlockIO) Open64(s uint64) uint64 {
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

func (x *BlockIO) Broadcast1(n bool) {
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

func (x *BlockIO) Broadcast8(n uint8) {
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

func (x *BlockIO) Broadcast32(n uint32) {
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

func (x *BlockIO) Broadcast64(n uint64) {
	n0 := uint32(n >> 32)
	n1 := uint32(n)
	x.Broadcast32(n0)
	x.Broadcast32(n1)
}

func (x *BlockIO) Send1(party int, n bool) {
	var n32 uint32 = 0
	if n {
		n32 = 1
	}
	x.Send32(party, n32)
}

func (x *BlockIO) Send8(party int, n uint8) {
	var n32 uint32 = uint32(n)
	id := x.Id()
	if party == id {
		return
	}
	x.Send32(party, n32)
}

func (x *BlockIO) Send32(party int, n32 uint32) {
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

func (x *BlockIO) Send64(party int, n uint64) {
	n0 := uint32(n >> 32)
	n1 := uint32(n)
	x.Send32(party, n0)
	x.Send32(party, n1)
}

func (x *BlockIO) Receive1(party int) bool {
	result := x.Receive32(party)
	return result > 0
}

func (x *BlockIO) Receive8(party int) uint8 {
	result := x.Receive32(party)
	return uint8(result)
}

func (x *BlockIO) Receive32(party int) uint32 {
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

func (x *BlockIO) Receive64(party int) uint64 {
	n0 := x.Receive32(party)
	n1 := x.Receive32(party)
	return (uint64(n0) << 32) | uint64(n1)
}

func (x *BlockIO) GetInput() uint32 {
	result := x.inputs[0]
	x.inputs = x.inputs[1:]
	return result
}

func (x *BlockIO) InitRam(contents []byte) {
	x.ram = contents
}

func (x *BlockIO) Ram() []byte {
	return x.ram
}
