package main

import (
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"encoding/gob"
	"encoding/hex"
	"flag"
	"fmt"
	"github.com/apcera/nats"
	"github.com/tjim/smpcc/runtime/gmw"
	"github.com/tjim/smpcc/runtime/max"
	"github.com/tjim/smpcc/runtime/ot"
	"github.com/tjim/smpcc/runtime/vickrey"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/sha3"
	"golang.org/x/crypto/ssh/terminal"
	"io"
	"log"
	"math/big"
	"os"
	"os/signal"
	"reflect"
	"runtime"
	"strings"
	"time"
)

var p2pAuth bool = true // temporary debugging flag, set to false to disable authentication and encryption

func shortKey(s string) string {
	if len(s) != 64 {
		panic("Malformed public key (wrong length)")
	}
	return s[:3]
}

func MarshalPublicKey(c *[32]byte) string {
	return hex.EncodeToString((*c)[:])
}

func UnmarshalPublicKey(s string) *[32]byte {
	if len(s) != 64 {
		panic("Malformed public key (wrong length)")
	}
	for _, v := range s {
		switch v {
		default:
			panic("Malformed public key (not lowercase hexidecimal)")
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f':
		}
	}
	var result [32]byte
	byteVal, err := hex.DecodeString(s)
	if err != nil {
		panic(err)
	}
	copy(result[:], byteVal)
	return &result
}

type Room struct {
	Room string
}

type Party struct {
	Nick string
	Key  string
}

// messages from clients to secretary
type JoinRequest struct {
	Party
}

type LeaveRequest struct {
	Party
}

type FuncRequest struct {
	Party
	FunctionName string
}

type StartRequest struct {
	Party
}

// messages from secretary to clients
type Message struct {
	Party
	Message string
}

type Members struct {
	Parties []Party
}

// messages from clients to commodity server
type StartCommodity struct {
	Hash    string
	Parties []Party
}

type EndCommodity struct {
}

type TripleCommodity struct {
}

type MaskTripleCommodity struct {
	numTriples     int
	numBytesTriple int
}

type PairConn struct {
	Nc            *nats.Conn
	ChanMasterPRF cipher.Block
	counter       int
	notMe         Party
}

type ChannelCrypto struct {
	BlockCipher cipher.AEAD
	PRG         cipher.Stream
}

func (p *PairConn) CryptoFromTag(tag string) ChannelCrypto {
	cc := ChannelCrypto{}
	hashedTagKey := sha3.Sum256([]byte("KEY_" + tag))
	hashedTagPrg := sha3.Sum256([]byte("PRG_" + tag))
	prgSeed := make([]byte, 32)
	key := make([]byte, 32)

	// Create two separate stream ciphers: one for generating pseudorandom values,
	// and one for encrypting payloads
	p.ChanMasterPRF.Encrypt(key, hashedTagKey[:])
	p.ChanMasterPRF.Encrypt(prgSeed, hashedTagPrg[:])
	cc.PRG = ot.NewPRG(prgSeed)
	aesPrf, err := aes.NewCipher(key)
	if err != nil {
		panic(err)
	}

	cc.BlockCipher, err = cipher.NewGCM(aesPrf)
	if err != nil {
		panic(err)
	}

	return cc
}

func (c *ChannelCrypto) Encrypt(plaintext []byte) []byte {
	nonce := make([]byte, c.BlockCipher.NonceSize())
	c.PRG.XORKeyStream(nonce, nonce)
	ciphertext := c.BlockCipher.Seal(nil, nonce, plaintext, nil)
	return ciphertext
}

func (c *ChannelCrypto) Decrypt(ciphertext []byte) []byte {
	nonce := make([]byte, c.BlockCipher.NonceSize())
	c.PRG.XORKeyStream(nonce, nonce)
	plaintext, err := c.BlockCipher.Open(nil, nonce, ciphertext, nil)
	if err != nil {
		panic(err)
	}
	return plaintext
}

func xorBytes(a, b, c []byte) {
	if len(a) != len(b) || len(b) != len(c) {
		panic("xorBytes: length mismatch")
	}
	for i := range a {
		a[i] = b[i] ^ c[i]
	}
}

func pairSubscribe(nc *nats.Conn, notMe Party) chan []byte {
	recvChan := make(chan []byte)
	ec, err := nats.NewEncodedConn(nc, "gob")
	if err != nil {
		panic(err)
	}
	ec.BindRecvChan(fmt.Sprintf("KEY-AGREEMENT-%s-%s", MyPublicKey, notMe.Key), recvChan)
	return recvChan
}

func pairInit(pc *PairConn, notMe Party, recvChan chan []byte, done chan bool) {
	//log.Printf("Marshalled peerPK: %v\n", notMe.Key)
	//log.Printf("Marshalled MyPK: %v\n", MyPublicKey)
	peerPk := UnmarshalPublicKey(notMe.Key)
	encapsulatedKey := make([]byte, 32)
	var nonce [24]byte
	rand.Read(encapsulatedKey)
	rand.Read(nonce[:])

	ciphertext := []byte{}
	ciphertext = box.Seal(ciphertext, encapsulatedKey, &nonce, peerPk, MyPrivateKey)

	//log.Printf("Out:\n%v\n%v\n%v\n%v\n\n", ciphertext, &nonce, peerPk, MyPrivateKey)

	ec, err := nats.NewEncodedConn(pc.Nc, "gob")
	if err != nil {
		panic(err)
	}

	sendChan := make(chan []byte)
	ec.BindSendChan(fmt.Sprintf("KEY-AGREEMENT-%s-%s", notMe.Key, MyPublicKey), sendChan)
	sendChan <- []byte(MyRoom.MpcFunc)
	sendChan <- nonce[:]
	sendChan <- ciphertext
	oMpcFunc := <-recvChan
	if MyRoom.MpcFunc != string(oMpcFunc) {
		panic("Disagreement on MPC function.")
	}
	oNonceArr := <-recvChan
	var oNonce [24]byte
	copy(oNonce[:], oNonceArr)
	oCiphertext := <-recvChan
	oEncapsulatedKey := []byte{}

	//log.Printf("In:\n%v\n%v\n%v\n%v\n\n", oCiphertext, oNonce, peerPk, MyPrivateKey)

	oEncapsulatedKey, isValid := box.Open(oEncapsulatedKey, oCiphertext, &oNonce, peerPk, MyPrivateKey)

	if p2pAuth && !isValid {
		panic("Ciphertext not valid!!!")
	}

	seedBytes := make([]byte, 32)
	xorBytes(encapsulatedKey, oEncapsulatedKey, seedBytes)
	pc.ChanMasterPRF, err = aes.NewCipher(seedBytes)
	if err != nil {
		panic(err)
	}
	done <- true
}

func Init() {
	gob.Register(JoinRequest{})
	gob.Register(LeaveRequest{})
	gob.Register(StartRequest{})
	gob.Register(FuncRequest{})
	gob.Register(Message{})
	gob.Register(Members{})
	gob.Register(big.NewInt(0))
	gob.Register(ot.HashedElGamalCiph{})
	gob.Register(ot.MessagePair{})
	gob.Register(StartCommodity{})
	gob.Register(EndCommodity{})
	gob.Register(TripleCommodity{})
	gob.Register(MaskTripleCommodity{})
}

type RoomState struct {
	Name    string
	Sub     *nats.Subscription
	Members []Party
	Hash    []byte
	MpcFunc string
}

func (st RoomState) indexOfParty(p Party) (int, error) {
	for i, v := range st.Members {
		if v == p {
			return i, nil
		}
	}
	return 0, nil
}

func encode(p interface{}) []byte {
	var buf bytes.Buffer
	enc := gob.NewEncoder(&buf)
	err := enc.Encode(&p)
	if err != nil {
		log.Fatal("encode:", err)
	}
	return buf.Bytes()
}

var MyPrivateKey *[32]byte
var MyPublicKey string
var MyParty Party
var MyRoom *RoomState
var MyNick string
var natsOptions nats.Options
var args []string // holds command line arguments after flag parsing

func handleNats(condition string) func(*nats.Conn) {
	return func(c *nats.Conn) {
		log.Println("Nats:", condition)
	}
}

func handleError(c *nats.Conn, s *nats.Subscription, err error) {
	log.Println("Nats error:", err)
}

var names []string = []string{
	"Alice",
	"Archie",
	"Bailey",
	"Bella",
	"Bob",
	"Buddy",
	"Charlie",
	"Coco",
	"Daisy",
	"Harry",
	"Jack",
	"Lucy",
	"Max",
	"Millie",
	"Milo",
	"Molly",
	"Oscar",
	"Poppy",
	"Rosie",
	"Roxy",
	"Ruby",
	"Toby",
}

func initialize() {
	Init()

	natsOptions = nats.DefaultOptions
	natsOptions.ClosedCB = handleNats("Close")
	natsOptions.DisconnectedCB = handleNats("Disconnect")
	natsOptions.ReconnectedCB = handleNats("Reconnect")
	natsOptions.AsyncErrorCB = handleError

	rawPublicKey, rawPrivateKey, _ := box.GenerateKey(rand.Reader)
	MyPrivateKey = rawPrivateKey
	MyPublicKey = MarshalPublicKey(rawPublicKey)
	nameIndex, err := rand.Int(rand.Reader, big.NewInt(int64(len(names))))
	if err != nil {
		log.Fatal(err)
	}
	MyNick = names[int(nameIndex.Int64())]
	MyParty = Party{MyNick, MyPublicKey}

	var serverAddress string
	flag.StringVar(&serverAddress, "server", "localhost:4222", "NATS server address (default localhost:4222)")
	flag.Parse()
	if strings.Contains(serverAddress, ":") {
		natsOptions.Url = "nats://" + serverAddress
	} else {
		natsOptions.Url = "nats://" + serverAddress + ":4222"
	}
	args = flag.Args()
}

func changeNick(nick string) {
	MyNick = nick
	MyParty = Party{MyNick, MyPublicKey}
}

func escape(s string) []byte {
	in := []byte(s)
	var out []byte
	for _, b := range in {
		switch {
		case b == '\t':
			out = append(out, ' ')
		case b == '\r':
			continue
		case b == '\n':
			out = append(out, '\n')
		case b < 32:
			out = append(out, '?')
		default:
			out = append(out, b)
		}
	}
	return out
}

func Tprintf(term *terminal.Terminal, format string, v ...interface{}) {
	term.Write(escape(fmt.Sprintf(format, v...)))
}

func client() {
	oldState, err := terminal.MakeRaw(0)
	if err != nil {
		panic(err.Error())
	}
	defer terminal.Restore(0, oldState)

	sigChan := make(chan os.Signal)
	signal.Notify(sigChan) // sadly this is useless in raw mode. cbreak mode might help but not supported by terminal package
	go func() {
		s := <-sigChan
		panic(fmt.Sprintf("Signal: %v", s))
	}()

	nc, err := natsOptions.Connect()
	if err != nil {
		panic("unable to connect to NATS server")
	}

	term := terminal.NewTerminal(os.Stdin, "> ")

	Tprintf(term, "Greetings, %s!\n", MyNick)
	Tprintf(term, "Commands:\n")
	Tprintf(term, "join foo      	(join the chatroom named foo)\n")
	Tprintf(term, "nick foo      	(change your nickname to foo)\n")
	Tprintf(term, "members       	(list the parties in the current room)\n")
	Tprintf(term, "func <function>  (propose a function for computation)\n")
	Tprintf(term, "run <number>  	(run max with input <number>)\n")
	Tprintf(term, "^D            	(buh-bye)\n")
	Tprintf(term, "anything else 	(send anything else to your current chatroom)\n")

	joinRoom(nc, term, "#general")

	for {
		line, err := term.ReadLine()
		if err != nil {
			if err.Error() != "EOF" {
				log.Println("Error!", err)
			} else {
				log.Println("Goodbye!")
			}
			leaveRoom(nc, term)
			nc.Close()
			return // exit
		}
		words := strings.Fields(line)
		if len(words) == 0 {
			continue
		}
		switch words[0] {
		case "noauth":
			p2pAuth = false
			Tprintf(term, "AUTHORIZATION DISABLED\n")
		case "nick":
			if len(words) == 2 {
				changeNick(words[1])
				Tprintf(term, "Your nickname is now %s\n", MyNick)

			} else {
				Tprintf(term, "Nicknames must be one word\n")
			}
		case "join":
			switch {
			case len(words) == 1:
				Tprintf(term, "You must say what room to join\n")
			case len(words) == 2:
				roomName := words[1]
				joinRoom(nc, term, roomName)
			default:
				Tprintf(term, "You can only join one room at once\n")
			}
		case "members":
			for _, member := range MyRoom.Members {
				Tprintf(term, "%s (%s)\n", member.Key, member.Nick)
			}
			Tprintf(term, "Hash: %x\n", MyRoom.Hash)
		case "func":
			switch {
			case len(words) == 1:
				Tprintf(term, "You must say what function you want to compute\n")
			case len(words) == 2:
				funcName := words[1]
				proposeFunc(nc, term, funcName)
			default:
				Tprintf(term, "You can only propose one function at once\n")
			}
		case "run":
			Tprintf(term, "Starting computation\n")
			session(nc, term, words[1:])
			MyRoom.MpcFunc = ""
			term.SetPrompt(fmt.Sprintf("%s> ", MyRoom.Name))
		case "test_crypto":
			Tprintf(term, "Testing crypto\n")
			// test crypto here
		default:
			msg := strings.TrimSpace(line)
			err = nc.Publish(MyRoom.Name, encode(Message{MyParty, msg}))
			checkError(err)
		}
	}
}

func (pc *PairConn) tag() string {
	result := fmt.Sprintf("%d", pc.counter)
	pc.counter++
	return result
}

func (pc *PairConn) bindSend(channel interface{}) {
	nc := pc.Nc
	tag := pc.tag()
	cc := pc.CryptoFromTag(tag)
	subject := fmt.Sprintf("%s.%s.%s.%s", MyRoom.Name, MyParty.Key, pc.notMe.Key, tag)
	//log.Println("bindSend", subject)
	// goroutine forwards values from channel over nats
	go func() {
		chVal := reflect.ValueOf(channel)
		if chVal.Kind() != reflect.Chan {
			panic("Can only bind channels")
		}
		counter := 0
		for {
			val, ok := chVal.Recv()
			//			log.Printf("%s.%s[%d] %v\n", shortKey(pc.notMe.Key), tag, counter, val.Type())
			if !ok {
				return // channel closed so we don't need goroutine any more
			}
			if !val.CanInterface() {
				log.Println("Error: failure due to unexported fields")
			}
			plaintext := encode(val.Interface())
			var msg []byte
			if p2pAuth {
				msg = cc.Encrypt(plaintext)
			} else {
				msg = plaintext
			}
			nc.Publish(subject, msg)
			//			log.Printf("%s.%s[%d].\n", shortKey(pc.notMe.Key), tag, counter)
			counter++
		}
	}()
}

// TODO: ought to return subscription so we can close the subscription
func (pc *PairConn) bindRecv(channel interface{}) *nats.Subscription {
	nc := pc.Nc
	tag := pc.tag()
	cc := pc.CryptoFromTag(tag)
	subject := fmt.Sprintf("%s.%s.%s.%s", MyRoom.Name, pc.notMe.Key, MyParty.Key, tag)
	//log.Println("bindRecv", subject)
	chVal := reflect.ValueOf(channel)
	if chVal.Kind() != reflect.Chan {
		panic("Can only bind channels")
	}
	sub, err := nc.Subscribe(subject, func(m *nats.Msg) {
		var decoderInput []byte
		ciphertext := m.Data
		if p2pAuth {
			decoderInput = cc.Decrypt(ciphertext)
		} else {
			decoderInput = ciphertext
		}
		dec := gob.NewDecoder(bytes.NewBuffer(decoderInput))
		var p interface{}
		err := dec.Decode(&p)
		if err != nil {
			log.Fatal("decode:", err)
		}
		chVal.Send(reflect.ValueOf(p)) // NB this is a blocking send.  NATS maintains a buffer before this
	})
	checkError(err)
	return sub
}

func barrier(nc *nats.Conn) bool {
	okChan := make(chan bool)
	ec, err := nats.NewEncodedConn(nc, "gob")
	if err != nil {
		panic("2")
	}
	ec.BindRecvChan(fmt.Sprintf("%s.secretary.barrier", MyRoom.Name), okChan)
	err = nc.Publish(fmt.Sprintf("secretary.%s", MyRoom.Name), encode(StartRequest{MyParty}))
	checkError(err)
	result := <-okChan
	close(okChan)
	return result
}

func session(nc *nats.Conn, term *terminal.Terminal, args []string) {
	inputs := make([]uint32, len(args))
	for i, v := range args {
		input := 0
		fmt.Sscanf(v, "%d", &input)
		inputs[i] = uint32(input)
	}
	// var Handle
	var Handle gmw.MPC
	switch MyRoom.MpcFunc {
	case "max":
		Handle = max.Handle
	case "vickrey":
		Handle = vickrey.Handle
	case "":
		Tprintf(term, "Before running a computation you must specify a function (use the 'func' command)\n")
		return
	default:
		Tprintf(term, "Unknown function '%s'\n", MyRoom.MpcFunc)
		return
	}

	numBlocks := Handle.NumBlocks
	id := -1

	//	log.Printf("Starting session. Members=\n%+v\n", MyRoom.Members)

	for i, v := range MyRoom.Members {
		if v == MyParty {
			id = i
			break
		}
	}
	if id == -1 {
		panic("Non-member trying to start a computation in a room")
	}

	numParties := len(MyRoom.Members)
	io := gmw.NewPeerIO(numBlocks, numParties, id)
	io.Inputs = inputs
	blocks := io.Blocks
	numBlocks = len(blocks) // increased by one by NewPeerIo

	recvChans := make([]chan []byte, numParties)
	for p := 0; p < numParties; p++ {
		if p == id {
			continue
		}
		notMe := MyRoom.Members[p]
		recvChans[p] = pairSubscribe(nc, notMe)
	}
	if !barrier(nc) {
		log.Println("Computation halted on error")
		return
	}
	pcs := make([]*PairConn, numParties)
	done := make(chan bool)
	for p := 0; p < numParties; p++ {
		if p == id {
			continue
		}
		notMe := MyRoom.Members[p]
		pc := &PairConn{nc, nil, 0, notMe}
		pcs[p] = pc
		go pairInit(pc, notMe, recvChans[p], done)
	}
	for p := 0; p < numParties; p++ {
		if p == id {
			continue
		}
		<-done
	}

	xs := make([]*gmw.PerNodePair, numParties)
	for p := 0; p < numParties; p++ {
		if p == id {
			continue
		}
		x := gmw.NewPerNodePair(io)
		xs[p] = x

		pc := pcs[p]

		if io.Leads(p) {
			// leader is server
			// that means it receives in the fatchan sense
			// also it is going to act as sender for the base OT
			pc.bindSend(x.ParamChan)
			s1 := pc.bindRecv(x.NpRecvPk)
			pc.bindSend(x.NpSendEncs)

			defer close(x.ParamChan)
			defer close(x.NpRecvPk)
			defer close(x.NpSendEncs)
			defer s1.Unsubscribe()

			for i := 0; i < numBlocks; i++ {
				pc.bindSend(x.BlockChans[i].SAS.Rwchannel)
				s2 := pc.bindRecv(x.BlockChans[i].CAS.Rwchannel)
				s3 := pc.bindRecv(x.BlockChans[i].CAS.S2R)
				pc.bindSend(x.BlockChans[i].CAS.R2S)
				s4 := pc.bindRecv(x.BlockChans[i].SAS.R2S)
				pc.bindSend(x.BlockChans[i].SAS.S2R)

				defer close(x.BlockChans[i].SAS.Rwchannel)
				defer close(x.BlockChans[i].CAS.Rwchannel)
				defer close(x.BlockChans[i].CAS.S2R)
				defer close(x.BlockChans[i].CAS.R2S)
				defer close(x.BlockChans[i].SAS.R2S)
				defer close(x.BlockChans[i].SAS.S2R)
				defer s2.Unsubscribe()
				defer s3.Unsubscribe()
				defer s4.Unsubscribe()
			}
		} else {
			s1 := pc.bindRecv(x.ParamChan)
			pc.bindSend(x.NpRecvPk)
			s2 := pc.bindRecv(x.NpSendEncs)

			defer close(x.ParamChan)
			defer close(x.NpRecvPk)
			defer close(x.NpSendEncs)
			defer s1.Unsubscribe()
			defer s2.Unsubscribe()

			for i := 0; i < numBlocks; i++ {
				s3 := pc.bindRecv(x.BlockChans[i].SAS.Rwchannel)
				pc.bindSend(x.BlockChans[i].CAS.Rwchannel)
				pc.bindSend(x.BlockChans[i].CAS.S2R)
				s4 := pc.bindRecv(x.BlockChans[i].CAS.R2S)
				pc.bindSend(x.BlockChans[i].SAS.R2S)
				s5 := pc.bindRecv(x.BlockChans[i].SAS.S2R)

				defer close(x.BlockChans[i].SAS.Rwchannel)
				defer close(x.BlockChans[i].CAS.Rwchannel)
				defer close(x.BlockChans[i].CAS.S2R)
				defer close(x.BlockChans[i].CAS.R2S)
				defer close(x.BlockChans[i].SAS.R2S)
				defer close(x.BlockChans[i].SAS.S2R)
				defer s3.Unsubscribe()
				defer s4.Unsubscribe()
				defer s5.Unsubscribe()
			}
		}
	}
	if !barrier(nc) {
		log.Println("Computation halted on error")
		return
	}

	//	log.Printf("I am party %d of %d\n", id, numParties)
	//	log.Printf("%s (%s)\n", MyParty.Key, MyParty.Nick)
	for p := 0; p < numParties; p++ {
		if p == id {
			continue
		}
		//log.Printf("Working on party %d\n", p)
		x := xs[p]
		if io.Leads(p) {
			//log.Println("Starting server side for", p)
			go gmw.ServerSideIOSetup(io, p, x, done)
		} else {
			//log.Println("Starting client side for", p)
			go gmw.ClientSideIOSetup(io, p, x, false, done)
		}
	}
	for p := 0; p < numParties; p++ {
		if p == id {
			continue
		}
		<-done
	}
	//	log.Println("Done setup")

	numBlocks = Handle.NumBlocks // make sure we have the right numBlocks
	// copy io.blocks[1:] to make an []Io; []BlockIO is not []Io
	x := make([]gmw.Io, numBlocks)
	for j := 0; j < numBlocks; j++ {
		x[j] = io.Blocks[j+1]
	}
	go Handle.Main(io.Blocks[0], x)
	<-Handle.Done
}

func leaveRoom(nc *nats.Conn, term *terminal.Terminal) {
	if MyRoom != nil { // leave current room; can be nil on startup only
		Tprintf(term, "You are leaving room %s\n", MyRoom.Name)
		err := nc.Publish(fmt.Sprintf("secretary.%s", MyRoom.Name), encode(LeaveRequest{MyParty}))
		checkError(err)
		err = MyRoom.Sub.Unsubscribe()
		checkError(err)
	}
}

func proposeFunc(nc *nats.Conn, term *terminal.Terminal, funcName string) {
	err := nc.Publish(MyRoom.Name, encode(FuncRequest{MyParty, funcName}))
	checkError(err)
}

func joinRoom(nc *nats.Conn, term *terminal.Terminal, roomName string) {
	leaveRoom(nc, term)
	MyRoom = &RoomState{roomName, nil, nil, nil, ""}
	term.SetPrompt(fmt.Sprintf("%s> ", roomName))
	err := nc.Publish(fmt.Sprintf("secretary.%s", roomName), encode(JoinRequest{MyParty}))
	checkError(err)
	Tprintf(term, "You have joined room %s\n", roomName)
	sub, err := nc.Subscribe(roomName, func(m *nats.Msg) {
		// Tprintf(term, "Received\n")
		dec := gob.NewDecoder(bytes.NewBuffer(m.Data))
		var p interface{}
		err := dec.Decode(&p)
		if err != nil {
			log.Fatal("decode:", err)
		}
		switch r := p.(type) {
		case Message:
			if r.Party != MyParty {
				Tprintf(term, "%s: %s -- (%s)\n", roomName, r.Message, r.Party.Nick)
			}
		case Members:
			MyRoom.Members = r.Parties
			h := sha3.New256()
			for _, member := range MyRoom.Members {
				io.WriteString(h, member.Key)
			}
			MyRoom.Hash = h.Sum(nil)
		case FuncRequest:
			MyRoom.MpcFunc = r.FunctionName
			term.SetPrompt(fmt.Sprintf("%s [%s]> ", MyRoom.Name, MyRoom.MpcFunc))
			Tprintf(term, "%s: Function proposed: %s -- (%s)\n", roomName, MyRoom.MpcFunc, r.Party.Nick)
		}
	})
	checkError(err)
	MyRoom.Sub = sub
}

func commodity() {
	// TODO: how to authenticate commodity server to chat participants
	initialize()
	states := make(map[string]*gmw.CommodityServerState)
	nc, err := natsOptions.Connect()
	if err != nil {
		panic("unable to connect to NATS server")
	}
	nc.Subscribe("commodity.>", func(m *nats.Msg) {
		dec := gob.NewDecoder(bytes.NewBuffer(m.Data))
		var p interface{}
		err := dec.Decode(&p)
		if err != nil {
			log.Fatal("decode:", err)
		}
		offerHash := m.Subject[len("commodity."):] // NEED BLOCK NUMBER
		switch r := p.(type) {
		case StartCommodity:
			_, ok := states[offerHash]
			if ok {
				// more than one offer for hash, something is wrong, halt that computation
				delete(states, offerHash)
				break
			}
			if r.Hash != offerHash {
				// hashes don't match, ignore it
				break
			}
			chs := make([]chan []byte, len(r.Parties))
			ec, _ := nats.NewEncodedConn(nc, "gob")
			for i, ch := range chs {
				ec.BindSendChan(fmt.Sprintf("commodity.%s.%s", offerHash, r.Parties[i].Key), ch)
			}
			// ALL CHANNELS NEED AUTHENTICATED ENCRYPTION
			// FIRST CHANNEL NEEDS SECURE SESSION
			states[offerHash] = gmw.NewCommodityServerState(chs)
		case EndCommodity:
			delete(states, offerHash)
		case TripleCommodity:
			s, ok := states[offerHash]
			if ok {
				s.TripleCorrection()
			}
		case MaskTripleCommodity:
			s, ok := states[offerHash]
			if ok {
				s.MaskTripleCorrection(r.numTriples, r.numBytesTriple)
			}
		}
	})
}

func secretary() {
	log.Println("starting secretary")
	changeNick("ChatAdministrator")
	rooms := make(map[string]bool)
	members := make(map[string](map[Party]bool))
	starters := make(map[string](map[Party]bool))
	nc, err := natsOptions.Connect()
	if err != nil {
		panic("unable to connect to NATS server")
	}

	// Go routine to ping room members to see if they're still there.
	go func() {
		for {
			time.Sleep(time.Second)
			// pinging code will go here
		}
	}()

	nc.Subscribe("secretary.>", func(m *nats.Msg) {
		dec := gob.NewDecoder(bytes.NewBuffer(m.Data))
		var p interface{}
		err := dec.Decode(&p)
		if err != nil {
			log.Fatal("decode:", err)
		}
		room := m.Subject[len("secretary."):]
		if !rooms[room] {
			rooms[room] = true
		}
		switch r := p.(type) {
		default:
			log.Println("Error: unknown message")
		case StartRequest:
			var result bool = false
			log.Println(r.Party, "asking to run in room", room)
			if _, ok := members[room]; !ok {
				log.Println("Warning:", room, "is empty, ignoring")
				return
			}
			if inRoom, ok := members[room][r.Party]; !(ok && inRoom) {
				log.Println("Warning:", r.Party, "is not a member of room", room, ", ignoring")
				return
			}
			if _, ok := starters[room]; !ok {
				log.Println("Allocating starters for", room)
				starters[room] = make(map[Party]bool)
				for k, _ := range members[room] {
					starters[room][k] = false
				}
			}
			if starters[room][r.Party] {
				log.Println("Warning:", r.Party, "has already requested to run in room", room, ", aborting")
				goto finish
			}
			starters[room][r.Party] = true
			for k, v := range starters[room] {
				if !v {
					log.Println("Still waiting for", k, "to run the computation in", room)
					return
				}
			}
			log.Println("Starting computation in", room)
			result = true
		finish:
			delete(starters, room)
			ec, _ := nats.NewEncodedConn(nc, "gob")
			okChan := make(chan bool)
			ec.BindSendChan(fmt.Sprintf("%s.secretary.barrier", room), okChan)
			okChan <- result
			close(okChan)
		case LeaveRequest:
			delete(members[room], r.Party)
			_ = nc.Publish(room, encode(Message{MyParty, fmt.Sprintf("%s has left the room", r.Party.Nick)}))
			log.Println("Leave", room, r)
			numMembers := len(members[room])
			parties := make([]Party, 0, numMembers)
			for party, _ := range members[room] {
				parties = append(parties, party)
			}
			_ = nc.Publish(room, encode(Members{parties}))
			log.Println("Members", room, members[room])
		case JoinRequest:
			if _, ok := members[room]; !ok {
				members[room] = make(map[Party]bool)
			}
			members[room][r.Party] = true
			_ = nc.Publish(room, encode(Message{MyParty, fmt.Sprintf("%s has joined %s", r.Party.Nick, room)}))
			log.Println("Join", room, r)
			numMembers := len(members[room])
			parties := make([]Party, 0, numMembers)
			for party, _ := range members[room] {
				parties = append(parties, party)
			}
			_ = nc.Publish(room, encode(Members{parties}))
			log.Println("Members", room, members[room])
		case Message:
			log.Println("Message", r.Message)
		}
	})
	runtime.Goexit()
}

func checkError(err error) {
	if err != nil {
		log.Fatal("Error:", err)
	}
}

func main() {
	initialize()
	if len(args) > 0 && args[0] == "secretary" {
		secretary()
	} else {
		client()
	}
}
