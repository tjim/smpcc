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
	"github.com/tjim/smpcc/runtime/ot"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/sha3"
	"golang.org/x/crypto/ssh/terminal"
	"log"
	"math/big"
	"reflect"
	"strings"
	"time"
)

var p2pAuth bool = true // temporary debugging flag, set to false to disable authentication and encryption

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
	checkError(err)
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
	Parties []Party
}

type EndCommodity struct {
}

type TripleCommodity struct {
}

type MaskTripleCommodity struct {
	NumTriples     int
	NumBytesTriple int
}

// NATS connection datatypes
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
	checkError(err)

	cc.BlockCipher, err = cipher.NewGCM(aesPrf)
	checkError(err)

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
	checkError(err)

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
	checkError(err)

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
	checkError(err)

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
	checkError(err)

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
	checkError(err)
	ec.BindRecvChan(fmt.Sprintf("%s.secretary.barrier", MyRoom.Name), okChan)
	err = nc.Publish(fmt.Sprintf("secretary.%s", MyRoom.Name), encode(StartRequest{MyParty}))
	checkError(err)
	result := <-okChan
	close(okChan)
	return result
}

func connectNats() *nats.Conn {
	nc, err := natsOptions.Connect()
	for i := 0; err != nil && i < 3; i++ {
		log.Printf("Error connecting to NATS server: %s\n", err)
		time.Sleep(5 * time.Second)
		log.Printf("Retrying...\n")
		nc, err = natsOptions.Connect()
	}
	checkError(err)
	return nc
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
	} else if len(args) > 0 && args[0] == "commodity" {
		commodity()
	} else {
		client()
	}
}
