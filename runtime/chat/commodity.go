package main

import (
	"bytes"
	"encoding/gob"
	"fmt"
	"github.com/apcera/nats"
	"github.com/tjim/smpcc/runtime/gmw"
	"log"
	"runtime"
)

// The GMW commodity server
//
// Commodity Protocol Specification:
//
// CS == commodity server
// CHASH == computation hash
//
//        MESSAGE                            NATS SUBJECT
//
// P[0]   >-- (StartCommodity) ------> CS    commodity.CHASH.BLOCKNUM
// P[0]   <----------------- SEED ---< CS    P[0].commodity.CHASH.BLOCKNUM
//        ...
// P[n-1] <----------------- SEED ---< CS    P[n-1].commodity.CHASH.BLOCKNUM
//
// P[0]   >-- (TripleRequest) -------> CS    commodity.CHASH.BLOCKNUM
// P[0]   <------ (TripleMaterial) --< CS    P[0].commodity.CHASH.BLOCKNUM
//
// P[0]   >-- (MaskTripleRequest) ---> CS    commodity.CHASH.BLOCKNUM
// P[0]   <-- (MaskTripleMaterial) --< CS    P[0].commodity.CHASH.BLOCKNUM
//
// P[0]   >-- (EndCommodity) --------> CS    commodity.CHASH.BLOCKNUM
func commodity() {
	log.Println("Starting commodity server")
	// TODO: how to authenticate commodity server to chat participants
	//	initialize()
	nc := connectNats()
	states := make(map[string]*gmw.CommodityServerState) // maintain one state computation hash + blocknum
	nc.Subscribe("commodity.>", func(m *nats.Msg) {
		dec := gob.NewDecoder(bytes.NewBuffer(m.Data))
		var p interface{}
		err := dec.Decode(&p)
		if err != nil {
			log.Println("decode:", err)
			return
		}
		hashAndBlocknum := m.Subject[len("commodity."):]
		// TODO: check that all messages are from party 0
		switch r := p.(type) {
		case StartCommodity:
			log.Println("StartCommodity", r)
			_, ok := states[hashAndBlocknum]
			if ok {
				// more than one StartCommodity message for a hashAndBlocknum; error, stop the computation
				log.Println("Error: multiple StartCommodity messages for", hashAndBlocknum)
				delete(states, hashAndBlocknum)
				break
			}
			chs := make([]chan []byte, len(r.Parties))
			for i, _ := range chs {
				chs[i] = make(chan []byte)
			}
			ec, _ := nats.NewEncodedConn(nc, "gob")
			for i, ch := range chs {
				// TODO: all channels need authenticated encryption,
				// first channel needs secure session
				ec.BindSendChan(fmt.Sprintf("%s.commodity.%s", r.Parties[i].Key, hashAndBlocknum), ch)
			}
			states[hashAndBlocknum] = gmw.NewCommodityServerState(chs)
		case EndCommodity:
			log.Println("EndCommodity", r)
			delete(states, hashAndBlocknum)
		case TripleCommodity:
			log.Println("TripleCommodity", r)
			s, ok := states[hashAndBlocknum]
			if !ok {
				log.Println("Error: TripleCommodity without StartCommodity,", hashAndBlocknum)
			} else {
				s.TripleCorrection()
			}
		case MaskTripleCommodity:
			log.Println("MaskTripleCommodity", r)
			s, ok := states[hashAndBlocknum]
			if !ok {
				log.Println("Error: MaskTripleCommodity without StartCommodity,", hashAndBlocknum)
			} else {
				s.MaskTripleCorrection(r.NumTriples, r.NumBytesTriple)
			}
		default:
			log.Println("Error: unknown message", p)
		}
	})
	runtime.Goexit()
}
