package main

import (
	"bytes"
	"encoding/gob"
	"fmt"
	"github.com/apcera/nats"
	"log"
	"runtime"
	"time"
)

func secretary() {
	log.Println("starting secretary")
	changeNick("ChatAdministrator")
	rooms := make(map[string]bool)
	members := make(map[string](map[Party]bool))
	starters := make(map[string](map[Party]bool))
	nc := connectNats()

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
