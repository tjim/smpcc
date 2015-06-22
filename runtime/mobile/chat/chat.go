// run
// gobind -lang go -outdir ./go_chat/ golang.org/x/mobile/example/smpcc/chat
// gobind -lang java -outdir ../app/src/main/java/go/chat golang.org/x/mobile/example/smpcc/chat
// from inside src/golang.org/x/mobile/example/smpcc/chat to generate gobind bindings
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
