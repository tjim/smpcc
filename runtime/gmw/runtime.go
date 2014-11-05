package gmw

import (
	"flag"
	"fmt"
	"os"
	"runtime/pprof"
)

func Run(numBlocks int, runPeer func(Io, []Io), peerDone <-chan bool) {
	var do_pprof bool
	var id int
	var parties int
	flag.BoolVar(&do_pprof, "pprof", false, "run for profiling")
	flag.IntVar(&id, "id", 0, "id of this party")
	flag.IntVar(&parties, "parties", 0, "number of parties")
	flag.Parse()
	args := flag.Args()
	inputs := make([]uint32, len(args))
	for i, v := range args {
		input := 0
		fmt.Sscanf(v, "%d", &input)
		inputs[i] = uint32(input)
	}
	if do_pprof {
		file := "cpu.pprof"
		f, err := os.Create(file)
		if err != nil {
			fmt.Println("Error: ", err)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}
	if parties == 0 {
		Simulation(inputs, numBlocks, runPeer, peerDone)
	} else {
		SetupPeer(inputs, numBlocks, parties, id, runPeer, peerDone)
	}

}
