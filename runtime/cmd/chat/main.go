package main

import (
	"github.com/tjim/smpcc/runtime/chat"
)

func main() {
	chat.Initialize()
	if len(chat.Args) > 0 && chat.Args[0] == "secretary" {
		chat.Secretary()
	} else if len(chat.Args) > 0 && chat.Args[0] == "commodity" {
		chat.Commodity()
	} else {
		chat.Client()
	}
}
