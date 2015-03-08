package gmw

import (
	"fmt"
	"github.com/tjim/fatchan"
	"github.com/tjim/smpcc/runtime/ot"
	"log"
	"math/big"
	"net"
	"time"
)

var log_mem bool = true
var log_results bool = false
var log_triples bool = false // true => print triple stats for simulation mode
var log_communication bool = false
var check_split = false

var stats_triple_chan chan bool = make(chan bool)
var stats_triple_num int = 0

func log_triple_goroutine() {
	for {
		<-stats_triple_chan
		stats_triple_num++
	}
}

func log_triple_output() {
	fmt.Printf("Generated %d triples\n", stats_triple_num*NUM_TRIPLES*32)
}

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

	MaskTriple32() (a byte, b, c uint32)

	InitRam([]byte)
	Ram() []byte
}

/* Share of a multiplication triple */
type Triple struct {
	a, b, c uint32
}

type MaskTriple struct {
	a    byte
	B, C []byte
}

type TripleSource interface {
	triple32() []Triple
	maskTriple(numTriples, numBytesTriple int) []MaskTriple
}

type GlobalIO struct {
	n      int      /* number of parties */
	id     int      /* id of party, range is 0..n-1 */
	Inputs []uint32 /* inputs of this party */
	ram    []byte
}

type BlockIO struct {
	*GlobalIO
	triples32   []Triple
	triples8    []struct{ a, b, c uint8 }
	triples1    []struct{ a, b, c bool }
	maskTriples []MaskTriple
	Rchannels   []chan uint32
	Wchannels   []chan uint32
	Source      TripleSource
}

type PeerIO struct {
	*GlobalIO // The GlobalIO of the peer and all of its blocks must be the same
	Blocks    []*BlockIO
}

/*
Each distinct pair of peers has a leader and a follower.
Each peer runs a server that accepts connections from its leaders.
Each peer connects as a client to all of its followers.

A client establishes all of the communication channels between the
client and server and runs the base OT as receiver.
It then establishes stream OT as sender.
It then establishes stream OT as receiver.
The server side does the opposite.

Port mapping:
Each peer can listen to n-1 ports: base port + n*id, plus next n-1 ports, ignoring its own port.
Each peer can connect to n-1 ports: for i<n, base port + n*i + id
Peers only listen for leaders and only connect to followers.
*/

// for establishing connections
type ClientAsSender struct {
	S2R       chan ot.MessagePair `fatchan:"request"` // One per sender/receiver pair, sender->receiver
	R2S       chan []byte         `fatchan:"reply"`   // One per sender/receiver pair, receiver->sender
	Rwchannel chan uint32         `fatchan:"request"`
}
type ServerAsSender struct {
	S2R       chan ot.MessagePair `fatchan:"reply"`   // One per sender/receiver pair, sender->receiver
	R2S       chan []byte         `fatchan:"request"` // One per sender/receiver pair, receiver->sender
	Rwchannel chan uint32         `fatchan:"reply"`
}
type PerBlock struct {
	CAS ClientAsSender
	SAS ServerAsSender
}
type PerNodePair struct {
	ot.NPChans
	BlockChans []PerBlock
}

const (
	base_port int = 3042
)

func (io *PeerIO) connect(party int, done chan bool) {
	if io.id == party {
		panic("connect0")
	}
	addr := fmt.Sprintf("%s:%d", Hosts[io.id], Ports[party]+io.id)
	server, err := net.Dial("tcp", addr)
	if err != nil {
		log.Fatalf("dial(%q): %s", addr, err)
	}

	xport := fatchan.New(server, nil)
	nu := make(chan *PerNodePair)
	xport.FromChan(nu)
	x := NewPerNodePair(io)
	nu <- x
	ClientSideIOSetup(io, party, x, true, done)
}

func NewPerNodePair(peer *PeerIO) *PerNodePair {
	blocks := peer.Blocks
	numBlocks := len(blocks)

	ParamChan := make(chan *big.Int)
	NpRecvPk := make(chan *big.Int)
	NpSendEncs := make(chan ot.HashedElGamalCiph)
	x := PerNodePair{ot.NPChans{ParamChan, NpRecvPk, NpSendEncs}, make([]PerBlock, numBlocks)}

	for i := 0; i < numBlocks; i++ {
		x.BlockChans[i] = PerBlock{
			ClientAsSender{make(chan ot.MessagePair), make(chan []byte), make(chan uint32)},
			ServerAsSender{make(chan ot.MessagePair), make(chan []byte), make(chan uint32)},
		}
	}
	return &x
}

func ClientSideIOSetup(peer *PeerIO, party int, x *PerNodePair, wait bool, done chan bool) {
	blocks := peer.Blocks
	numBlocks := len(blocks)
	ParamChan := x.ParamChan
	NpRecvPk := x.NpRecvPk
	NpSendEncs := x.NpSendEncs

	if wait {
		time.Sleep(3 * time.Second) // wait for fatchan channel setup at server to complete
	}
	for i := 0; i < numBlocks; i++ {
		blocks[i].Rchannels[party] = x.BlockChans[i].SAS.Rwchannel
		blocks[i].Wchannels[party] = x.BlockChans[i].CAS.Rwchannel
	}

	baseReceiver := ot.NewNPReceiver(ParamChan, NpRecvPk, NpSendEncs)
	sender0 := ot.NewStreamSender(baseReceiver, x.BlockChans[0].CAS.S2R, x.BlockChans[0].CAS.R2S)
	receiver0 := ot.NewStreamReceiver(sender0, x.BlockChans[0].SAS.R2S, x.BlockChans[0].SAS.S2R)

	source := blocks[0].Source.(*OtState)
	source.senders[party] = sender0
	source.receivers[party] = receiver0

	for i := 1; i < numBlocks; i++ {
		sender := sender0.Fork(x.BlockChans[i].CAS.S2R, x.BlockChans[i].CAS.R2S)
		receiver := receiver0.Fork(x.BlockChans[i].SAS.R2S, x.BlockChans[i].SAS.S2R)
		source := blocks[i].Source.(*OtState)
		source.senders[party] = sender
		source.receivers[party] = receiver
	}

	done <- true
}

func (io *PeerIO) listen(party int, done chan bool) {
	if io.id == party {
		panic("listen0")
	}
	addr := fmt.Sprintf("%s:%d", Hosts[io.id], Ports[io.id]+party)
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		log.Fatalf("listen(%q): %s", addr, err)
	}
	conn, err := listener.Accept()
	if err != nil {
		log.Fatalf("accept(): %s", err)
	}
	xport := fatchan.New(conn, nil)
	nu := make(chan *PerNodePair)
	xport.ToChan(nu)
	ServerSideIOSetup(io, party, <-nu, done)
}

func ServerSideIOSetup(peer *PeerIO, party int, x *PerNodePair, done chan bool) {
	blocks := peer.Blocks
	numBlocks := len(blocks)

	if numBlocks != len(x.BlockChans) {
		panic("Block mismatch")
	}

	for i := 0; i < numBlocks; i++ {
		blocks[i].Wchannels[party] = x.BlockChans[i].SAS.Rwchannel
		blocks[i].Rchannels[party] = x.BlockChans[i].CAS.Rwchannel
	}

	baseSender := ot.NewNPSender(x.NPChans.ParamChan, x.NPChans.NpRecvPk, x.NPChans.NpSendEncs)
	receiver0 := ot.NewStreamReceiver(baseSender, x.BlockChans[0].CAS.R2S, x.BlockChans[0].CAS.S2R)
	sender0 := ot.NewStreamSender(receiver0, x.BlockChans[0].SAS.S2R, x.BlockChans[0].SAS.R2S)

	source := blocks[0].Source.(*OtState)
	source.senders[party] = sender0
	source.receivers[party] = receiver0

	for i := 1; i < numBlocks; i++ {
		sender := sender0.Fork(x.BlockChans[i].SAS.S2R, x.BlockChans[i].SAS.R2S)
		receiver := receiver0.Fork(x.BlockChans[i].CAS.R2S, x.BlockChans[i].CAS.S2R)
		source := blocks[i].Source.(*OtState)
		source.senders[party] = sender
		source.receivers[party] = receiver
	}

	done <- true
}

func NewPeerIO(numBlocks int, numParties int, id int) *PeerIO {
	var gio GlobalIO
	gio.n = numParties
	gio.id = id
	var io PeerIO
	io.GlobalIO = &gio
	io.Blocks = make([]*BlockIO, numBlocks+1) // one extra BlockIO for the main loop
	for i := range io.Blocks {
		io.Blocks[i] = &BlockIO{
			io.GlobalIO,
			nil, nil, nil, nil,
			make([]chan uint32, numParties),
			make([]chan uint32, numParties),
			NewOtState(id, numParties),
		}
	}
	return &io
}

func SetupPeer(inputs []uint32, numBlocks int, numParties int, id int, runPeer func(Io, []Io)) {
	io := NewPeerIO(numBlocks, numParties, id)
	io.Inputs = inputs
	done := make(chan bool)
	// start listening for clients
	for i := 0; i < numParties; i++ {
		if io.id != i && !io.Leads(i) {
			go io.listen(i, done)
		}
	}
	time.Sleep(2 * time.Second) // wait for servers of other parties to start listening
	// start connecting to servers of other parties
	for i := 0; i < numParties; i++ {
		if io.id != i && io.Leads(i) {
			go io.connect(i, done)
		}
	}
	for i := 0; i < numParties; i++ {
		if io.id != i {
			<-done
		}
	}
	// copy io.blocks[1:] to make an []Io; []BlockIO is not []Io
	x := make([]Io, numBlocks)
	for j := 0; j < numBlocks; j++ {
		x[j] = io.Blocks[j+1]
	}
	runPeer(io.Blocks[0], x)
}

func Simulation(inputs []uint32, numBlocks int, runPeer func(Io, []Io)) {
	if log_triples {
		go log_triple_goroutine()
	}
	numParties := len(inputs)
	if numParties == 0 { // for some test cases we may have no inputs
		numParties = 2
	}
	ios := make([]*PeerIO, numParties)
	for i := 0; i < numParties; i++ {
		peer := NewPeerIO(numBlocks, numParties, i)
		if len(inputs) > i {
			peer.Inputs = inputs[i : i+1]
		}
		ios[i] = peer
	}
	xs := make([]*PerNodePair, numParties*numParties)
	done := make(chan bool)
	for i := 0; i < numParties; i++ {
		for j := 0; j < numParties; j++ {
			if i != j && !ios[i].Leads(j) {
				x := NewPerNodePair(ios[i])
				xs[i*numParties+j] = x                   // server i talking to client j
				go ServerSideIOSetup(ios[i], j, x, done) // i's setup server for party j
			}
		}
	}
	for i := 0; i < numParties; i++ {
		for j := 0; j < numParties; j++ {
			if i != j && ios[i].Leads(j) {
				x := xs[j*numParties+i]                         // client i talking to server j
				go ClientSideIOSetup(ios[i], j, x, false, done) // i's setup client for party j
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
	peerDone := make(chan bool)
	for i := 0; i < numParties; i++ {
		// copy ios[i].blocks[1:] to make an []Io; []BlockIO is not []Io
		x := make([]Io, numBlocks)
		for j := 0; j < numBlocks; j++ {
			x[j] = ios[i].Blocks[j+1]
		}
		go func(i int, x []Io) {
			runPeer(ios[i].Blocks[0], x)
			peerDone <- true
		}(i, x)
	}
	for i := 0; i < numParties; i++ {
		<-peerDone // wait for all peers to complete
	}
	if log_triples {
		log_triple_output()
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

func AndVector(a byte, b []byte) []byte {
	mask := byte(0)
	if a != 0 {
		mask = 0xff
	}
	result := make([]byte, len(b))
	for i, v := range b {
		result[i] = v & mask
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

func (x *BlockIO) MaskTriple32() (a byte, B uint32, C uint32) {
	if len(x.maskTriples) == 0 {
		x.maskTriples = x.Source.maskTriple(32, 4) // 32 triples, 4 bytes each
	}
	result := x.maskTriples[0]
	x.maskTriples = x.maskTriples[1:]
	return result.a, combine(result.B), combine(result.C)
}

func (x *BlockIO) Triple32() (a, b, c uint32) {
	if len(x.triples32) == 0 {
		x.triples32 = x.Source.triple32()
		if log_triples && x.Id() == 0 {
			stats_triple_chan <- true
		}
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
	if x.Id() == 0 {
		result := s
		for i := 1; i < x.N(); i++ {
			result = xor(result, x.Receive1(i))
		}
		for i := 1; i < x.N(); i++ {
			x.Send1(i, result)
		}
		return result
	} else {
		x.Send1(0, s)
		return x.Receive1(0)
	}
}

func (x *BlockIO) Open8(s uint8) uint8 {
	if x.Id() == 0 {
		result := s
		for i := 1; i < x.N(); i++ {
			result ^= x.Receive8(i)
		}
		for i := 1; i < x.N(); i++ {
			x.Send8(i, result)
		}
		return result
	} else {
		x.Send8(0, s)
		return x.Receive8(0)
	}
}

func (x *BlockIO) Open32(s uint32) uint32 {
	if x.Id() == 0 {
		result := s
		for i := 1; i < x.N(); i++ {
			result ^= x.Receive32(i)
		}
		for i := 1; i < x.N(); i++ {
			x.Send32(i, result)
		}
		return result
	} else {
		x.Send32(0, s)
		return x.Receive32(0)
	}
}

func (x *BlockIO) Open64(s uint64) uint64 {
	if x.Id() == 0 {
		result := s
		for i := 1; i < x.N(); i++ {
			result ^= x.Receive64(i)
		}
		for i := 1; i < x.N(); i++ {
			x.Send64(i, result)
		}
		return result
	} else {
		x.Send64(0, s)
		return x.Receive64(0)
	}
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
	ch := x.Wchannels[party]
	ch <- n32
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
	switch { // temporary debugging measure
	case x == nil:
		fmt.Printf("x == nil, party == %d\n", party)
	case x.Rchannels == nil:
		fmt.Printf("x.Rchannels == nil, party == %d\n", party)
	case party < 0:
		fmt.Printf("party == %d\n", party)
	case party >= len(x.Rchannels):
		fmt.Printf("len(x.Rchannels) == %d, party == %d\n", len(x.Rchannels), party)
	}
	ch := x.Rchannels[party]
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
	result := x.Inputs[0]
	x.Inputs = x.Inputs[1:]
	return result
}

func (x *BlockIO) InitRam(contents []byte) {
	x.ram = contents
}

func (x *BlockIO) Ram() []byte {
	return x.ram
}
