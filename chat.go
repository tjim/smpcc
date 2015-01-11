package main

import (
	"bytes"
	"crypto/rand"
	"crypto/sha1"
	"encoding/gob"
	"fmt"
	"github.com/apcera/nats"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/ssh/terminal"
	"io"
	"log"
	"os"
	"runtime"
	"strings"
)

func MarshalPublicKey(c *[32]byte) string {
	return fmt.Sprintf("%x", *c)
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
	result := new([32]byte)
	fmt.Sscanf(s, "%x", result)
	return result
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

// messages from secretary to clients
type Message struct {
	Party
	Message string
}

type Members struct {
	Parties []Party
}

func Init() {
	gob.Register(JoinRequest{})
	gob.Register(LeaveRequest{})
	gob.Register(Message{})
	gob.Register(Members{})
}

type RoomState struct {
	Sub     *nats.Subscription
	Members []Party
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
var MyRooms map[string]*RoomState
var MyRoom string
var MyNick string

func initialize() {
	Init()
	rawPrivateKey, rawPublicKey, _ := box.GenerateKey(rand.Reader)
	MyPrivateKey = rawPrivateKey
	MyPublicKey = MarshalPublicKey(rawPublicKey)
	MyNick = "AnonymousCoward"
	MyParty = Party{MyNick, MyPublicKey}
	MyRooms = make(map[string]*RoomState)
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
	initialize()
	oldState, err := terminal.MakeRaw(0)
	if err != nil {
		panic(err.Error())
	}
	defer terminal.Restore(0, oldState)

	term := terminal.NewTerminal(os.Stdin, "> ")
	Tprintf(term, "Greetings, %s!\n", MyNick)
	Tprintf(term, "Commands:\n")
	Tprintf(term, "join foo      (join the chatroom named foo)\n")
	Tprintf(term, "leave foo     (leave the chatroom named foo)\n")
	Tprintf(term, "nick foo      (change your nickname to foo)\n")
	Tprintf(term, "members       (list the parties in the current room)\n")
	Tprintf(term, "^D            (buh-bye)\n")
	Tprintf(term, "anything else (send anything else to your current chatroom)\n")

	nc, err := nats.Connect(nats.DefaultURL)
	if err != nil {
		panic("unable to connect to NATS server")
	}

	for {
		line, err := term.ReadLine()
		if err != nil {
			break
		}
		words := strings.Fields(line)
		if len(words) == 0 {
			continue
		}
		switch words[0] {
		case "nick":
			if len(words) == 2 {
				changeNick(words[1])
				Tprintf(term, "Your nickname is now %s\n", MyNick)

			} else {
				Tprintf(term, "Nicknames must be one word\n")
			}
		case "join":
			if len(words) == 2 {
				room := words[1]
				MyRoom = room
				term.SetPrompt(fmt.Sprintf("[%s]> ", MyRoom))
				_, ok := MyRooms[room]
				if !ok {
					joinTerm(nc, term, room)
				}
			} else {
				Tprintf(term, "Join what room?\n")
			}
		case "rooms":
			for room, _ := range MyRooms {
				Tprintf(term, "%s\n", room)
			}
		case "leave":
			words = words[1:]
			for _, room := range words {
				if st, ok := MyRooms[room]; ok {
					delete(MyRooms, room)
					err = st.Sub.Unsubscribe()
					checkError(err)
					if MyRoom == room {
						if len(MyRooms) == 0 {
							MyRoom = ""
							term.SetPrompt(fmt.Sprintf("> "))
						} else {
							for r, _ := range MyRooms {
								MyRoom = r
								term.SetPrompt(fmt.Sprintf("[%s]> ", MyRoom))
								break
							}
						}
					}
				} else {
					Tprintf(term, "Not a member of %s\n", room)
				}
			}
		case "members":
			if MyRoom == "" {
				Tprintf(term, "You must join a room before you can see the members of the room\n")
			} else {
				h := sha1.New()
				st := MyRooms[MyRoom]
				for _, member := range st.Members {
					io.WriteString(h, member.Key)
					Tprintf(term, "%s (%s)\n", member.Key, member.Nick)
				}
				Tprintf(term, "Hash: %x\n", h.Sum(nil))
			}
		default:
			if MyRoom != "" {
				msg := strings.TrimSpace(line)
				err = nc.Publish(MyRoom, encode(Message{MyParty, msg}))
				checkError(err)
			} else {
				Tprintf(term, "You must join a room first\n")
			}
		}
	}
}

func joinTerm(nc *nats.Conn, term *terminal.Terminal, rm string) {
	err := nc.Publish(fmt.Sprintf("secretary.%s", rm), encode(JoinRequest{MyParty}))
	checkError(err)
	sub, err := nc.Subscribe(rm, func(m *nats.Msg) {
		dec := gob.NewDecoder(bytes.NewBuffer(m.Data))
		var p interface{}
		err := dec.Decode(&p)
		if err != nil {
			log.Fatal("decode:", err)
		}
		switch r := p.(type) {
		case Message:
			if r.Party != MyParty {
				Tprintf(term, "[%s]: %s -- (%s)\n", rm, r.Message, r.Party.Nick)
			}
		case Members:
			MyRooms[rm].Members = r.Parties
		}
	})
	checkError(err)
	MyRooms[rm] = &RoomState{sub, nil} // needs lock
}

func secretary() {
	log.Println("starting secretary")
	initialize()
	changeNick("ChatAdministrator")
	rooms := make(map[string]bool)
	members := make(map[string](map[Party]bool))
	nc, err := nats.Connect(nats.DefaultURL)
	if err != nil {
		panic("unable to connect to NATS server")
	}
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
		case LeaveRequest:
			delete(members[room], r.Party)
			_ = nc.Publish(room, encode(Message{MyParty, fmt.Sprintf("%s has left the room", r.Party.Nick)}))
			log.Println("Leave", room, r)
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
	if len(os.Args) > 1 && os.Args[1] == "secretary" {
		secretary()
	} else {
		client()
	}
}
