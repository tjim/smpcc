// to create a .aar file run:
// gomobile bind github.com/tjim/smpcc/runtime/mobile/chat
package chat

import (
	"errors"
	schat "github.com/tjim/smpcc/runtime/chat"
)

var (
	userTyping   = make(chan string, 100)
	msgReceiving = make(chan string, 100)
)

func Init() {
	schat.InitializeExternal()
	go func() {
		schat.ClientChannels(userTyping, msgReceiving)
	}()
}

func SendMessage(msg string) error {
	select {
	case userTyping <- msg:
		return nil
	default:
		return errors.New("Was unable to write message to channel. Channel is probably full.")
	}

}

func PollForMessages() string {
	select {
	case msg := <-msgReceiving:
		return msg
	default:
		return ""
	}
}
