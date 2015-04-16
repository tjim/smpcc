package main

import (
	"bytes"
	"encoding/gob"
	"fmt"
	"github.com/apcera/nats"
	"github.com/tjim/smpcc/runtime/gmw"
	"github.com/tjim/smpcc/runtime/max"
	"github.com/tjim/smpcc/runtime/vickrey"
	"golang.org/x/crypto/sha3"
	"golang.org/x/crypto/ssh/terminal"
	"io"
	"log"
	"os"
	"os/signal"
	"strings"
)

func ClientChannels(userTyping, msgReceiving chan string) {

	nc := connectNats()

	msgReceiving <- fmt.Sprintf("Greetings, %s!\n", MyNick)
	msgReceiving <- fmt.Sprintf("Commands:\n")
	msgReceiving <- fmt.Sprintf("join foo      	(join the chatroom named foo)\n")
	msgReceiving <- fmt.Sprintf("nick foo      	(change your nickname to foo)\n")
	msgReceiving <- fmt.Sprintf("members       	(list the parties in the current room)\n")
	msgReceiving <- fmt.Sprintf("func <function>  (propose a function for computation)\n")
	msgReceiving <- fmt.Sprintf("run <number>  	(run max with input <number>)\n")
	msgReceiving <- fmt.Sprintf("^D            	(buh-bye)\n")
	msgReceiving <- fmt.Sprintf("anything else 	(send anything else to your current chatroom)\n")

	joinRoomChannels(nc, msgReceiving, "#general")

	for {
		line := <-userTyping
		if line == "END" {
			log.Println("Goodbye!")
			leaveRoom(nc, msgReceiving)
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
			msgReceiving <- fmt.Sprintf("AUTHORIZATION DISABLED\n")
		case "nick":
			if len(words) == 2 {
				changeNick(words[1])
				msgReceiving <- fmt.Sprintf("Your nickname is now %s\n", MyNick)

			} else {
				msgReceiving <- fmt.Sprintf("Nicknames must be one word\n")
			}
		case "join":
			switch {
			case len(words) == 1:
				msgReceiving <- fmt.Sprintf("You must say what room to join\n")
			case len(words) == 2:
				roomName := words[1]
				joinRoomChannels(nc, msgReceiving, roomName)
			default:
				msgReceiving <- fmt.Sprintf("You can only join one room at once\n")
			}
		case "members":
			for _, member := range MyRoom.Members {
				msgReceiving <- fmt.Sprintf("%s (%s)\n", member.Key, member.Nick)
			}
			msgReceiving <- fmt.Sprintf("Hash: %x\n", MyRoom.Hash)
		case "func":
			switch {
			case len(words) == 1:
				msgReceiving <- fmt.Sprintf("You must say what function you want to compute\n")
			case len(words) == 2:
				funcName := words[1]
				proposeFunc(nc, funcName)
			default:
				msgReceiving <- fmt.Sprintf("You can only propose one function at once\n")
			}
		case "run":
			msgReceiving <- fmt.Sprintf("Starting computation\n")
			session(nc, msgReceiving, words[1:])
			MyRoom.MpcFunc = ""
			// term.SetPrompt(fmt.Sprintf("%s> ", MyRoom.Name))
		case "runco":
			msgReceiving <- fmt.Sprintf("Starting commodity computation\n")
			commoditySession(nc, msgReceiving, words[1:])
			MyRoom.MpcFunc = ""
			// term.SetPrompt(fmt.Sprintf("%s> ", MyRoom.Name))
		case "test_crypto":
			msgReceiving <- fmt.Sprintf("Testing crypto\n")
			// test crypto here
		default:
			msg := strings.TrimSpace(line)
			err := nc.Publish(MyRoom.Name, encode(Message{MyParty, msg}))
			checkError(err)
		}
	}
}

func client() {
	oldState, err := terminal.MakeRaw(0)
	checkError(err)

	defer terminal.Restore(0, oldState)

	sigChan := make(chan os.Signal)
	signal.Notify(sigChan) // sadly this is useless in raw mode. cbreak mode might help but not supported by terminal package
	go func() {
		s := <-sigChan
		panic(fmt.Sprintf("Signal: %v", s))
	}()

	nc := connectNats()
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

	printToTermChan := make(chan string)
	go func() {
		for {
			msg := <-printToTermChan
			Tprintf(term, msg)
		}
	}()
	joinRoomChannels(nc, printToTermChan, "#general")

	for {
		line, err := term.ReadLine()
		if err != nil {
			if err.Error() != "EOF" {
				log.Println("Error!", err)
			} else {
				log.Println("Goodbye!")
			}
			leaveRoom(nc, printToTermChan)
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
				joinRoomChannels(nc, printToTermChan, roomName)
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
				proposeFunc(nc, funcName)
			default:
				Tprintf(term, "You can only propose one function at once\n")
			}
		case "run":
			Tprintf(term, "Starting computation\n")
			session(nc, printToTermChan, words[1:])
			MyRoom.MpcFunc = ""
			term.SetPrompt(fmt.Sprintf("%s> ", MyRoom.Name))
		case "runco":
			Tprintf(term, "Starting commodity computation\n")
			commoditySession(nc, printToTermChan, words[1:])
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

func session(nc *nats.Conn, msgReceived chan string, args []string) {
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
		msgReceived <- fmt.Sprintf("Before running a computation you must specify a function (use the 'func' command)\n")
		return
	default:
		msgReceived <- fmt.Sprintf("Unknown function '%s'\n", MyRoom.MpcFunc)
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
	Handle.Main(io.Blocks[0], x)
}

type natsCommodityRequester struct {
	nc          *nats.Conn
	natsSubject string
}

func (ncr *natsCommodityRequester) RequestTripleCorrection() {
	err := ncr.nc.Publish(ncr.natsSubject, encode(TripleCommodity{}))
	checkError(err)
}

func (ncr *natsCommodityRequester) RequestMaskTripleCorrection(numTriples, numBytesTriple int) {
	err := ncr.nc.Publish(ncr.natsSubject, encode(MaskTripleCommodity{numTriples, numBytesTriple}))
	checkError(err)
}

func commoditySession(nc *nats.Conn, msgReceived chan string, args []string) {
	inputs := make([]uint32, len(args))
	for i, v := range args {
		input := 0
		fmt.Sscanf(v, "%d", &input)
		inputs[i] = uint32(input)
	}
	var Handle gmw.MPC
	switch MyRoom.MpcFunc {
	case "max":
		Handle = max.Handle
	case "vickrey":
		Handle = vickrey.Handle
	case "":
		msgReceived <- fmt.Sprintf("Before running a computation you must specify a function (use the 'func' command)\n")
		return
	default:
		msgReceived <- fmt.Sprintf("Unknown function '%s'\n", MyRoom.MpcFunc)
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

	for _, block := range blocks {
		for p := 0; p < numParties; p++ {
			if p == id {
				continue
			}
			block.Rchannels[p] = make(chan uint32)
			block.Wchannels[p] = make(chan uint32)

			pc := pcs[p]
			if io.Leads(p) {
				// Leader binds Recv then Send
				sub := pc.bindRecv(block.Rchannels[p])
				pc.bindSend(block.Wchannels[p])
				defer sub.Unsubscribe()
			} else {
				// Follower binds Send then Recv
				// This means NATS subjects will match up
				pc.bindSend(block.Wchannels[p])
				sub := pc.bindRecv(block.Rchannels[p])
				defer sub.Unsubscribe()
			}
			defer close(block.Rchannels[p])
			defer close(block.Wchannels[p])
		}
	}

	for i, block := range blocks {
		for p := 0; p < numParties; p++ {
			if p == id {
				continue
			}
			natsSubjectFrom := fmt.Sprintf("%s.commodity.%x.%d", MyPublicKey, MyRoom.Hash, i)
			natsSubjectTo := fmt.Sprintf("commodity.%x.%d", MyRoom.Hash, i)

			correctionCh := make(chan []byte)
			ncr := &natsCommodityRequester{nc, natsSubjectTo}
			block.Source = gmw.NewCommodityClientState(correctionCh, ncr)

			ec, err := nats.NewEncodedConn(nc, "gob")
			checkError(err)
			sub, err := ec.BindRecvChan(natsSubjectFrom, correctionCh)
			checkError(err)

			// non-distinguished parties could close earlier
			defer close(correctionCh)
			defer sub.Unsubscribe()
		}
	}

	if !barrier(nc) {
		log.Println("Computation halted on error")
		return
	}

	// Distinguished party sends StartCommodity message
	if id == 0 {
		for i, _ := range blocks {
			err := nc.Publish(fmt.Sprintf("commodity.%x.%d", MyRoom.Hash, i), encode(StartCommodity{MyRoom.Members}))
			checkError(err)
		}
	}

	for _, block := range blocks {
		distinguished := id == 0
		gmw.InitCommodityClientState(block.Source.(*gmw.CommodityClientState), distinguished)
	}

	numBlocks = Handle.NumBlocks // make sure we have the right numBlocks
	// copy io.blocks[1:] to make an []Io; []BlockIO is not []Io
	x := make([]gmw.Io, numBlocks)
	for j := 0; j < numBlocks; j++ {
		x[j] = io.Blocks[j+1]
	}
	Handle.Main(io.Blocks[0], x)
	// Distinguished party sends EndCommodity message
	if id == 0 {
		for i, _ := range blocks {
			err := nc.Publish(fmt.Sprintf("commodity.%x.%d", MyRoom.Hash, i), encode(EndCommodity{}))
			checkError(err)
		}
	}
}

func leaveRoom(nc *nats.Conn, msgReceived chan string) {
	if MyRoom != nil { // leave current room; can be nil on startup only
		msgReceived <- fmt.Sprintf("You are leaving room %s\n", MyRoom.Name)
		err := nc.Publish(fmt.Sprintf("secretary.%s", MyRoom.Name), encode(LeaveRequest{MyParty}))
		checkError(err)
		err = MyRoom.Sub.Unsubscribe()
		checkError(err)
	}
}

func proposeFunc(nc *nats.Conn, funcName string) {
	err := nc.Publish(MyRoom.Name, encode(FuncRequest{MyParty, funcName}))
	checkError(err)
}

func joinRoomChannels(nc *nats.Conn, msgReceiveing chan string, roomName string) {
	leaveRoom(nc, msgReceiveing)
	MyRoom = &RoomState{roomName, nil, nil, nil, ""}
	// term.SetPrompt(fmt.Sprintf("%s> ", roomName))
	err := nc.Publish(fmt.Sprintf("secretary.%s", roomName), encode(JoinRequest{MyParty}))
	checkError(err)
	msgReceiveing <- fmt.Sprintf("You have joined room %s\n", roomName)
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
				msgReceiveing <- fmt.Sprintf("%s: %s -- (%s)\n", roomName, r.Message, r.Party.Nick)
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
			// term.SetPrompt(fmt.Sprintf("%s [%s]> ", MyRoom.Name, MyRoom.MpcFunc))
			msgReceiveing <- fmt.Sprintf("%s: Function proposed: %s -- (%s)\n", roomName, MyRoom.MpcFunc, r.Party.Nick)
		}
	})
	checkError(err)
	MyRoom.Sub = sub
}
