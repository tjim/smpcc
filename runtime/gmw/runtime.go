package gmw

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"runtime/pprof"
	"strings"
)

type MPC struct {
	NumBlocks int
	Main      func(io Io, ios []Io)
	Done      chan bool
}

var Hosts map[int]string = make(map[int]string)
var Ports map[int]int = make(map[int]int)

// Read a configuration file, which consists a series lines of the form host:port, on per party, in order.
// Return maps of hosts and ports, so hosts[i] is the host of party i and ports[i] is its base port.
func ReadConfig(filename string) bool {
	file, err := os.Open(filename)
	if err != nil {
		return false
	}
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanWords)
	numParties := 0
	for scanner.Scan() {
		hostport := scanner.Bytes()
		parts := strings.Split(string(hostport), ":")
		if len(parts) != 2 {
			panic("ReadConfig: input error")
		}
		host := parts[0]
		port := 0
		fmt.Sscanf(parts[1], "%d", &port)
		Hosts[numParties] = host
		Ports[numParties] = port
		numParties++
	}
	return true
}

func SetupHostsPorts(parties int) {
	for i := 0; i < parties; i++ {
		Hosts[i] = "127.0.0.1"
		Ports[i] = base_port + i*parties
	}
}

func Run(numBlocks int, runPeer func(Io, []Io), peerDone <-chan bool) {
	var do_pprof bool
	var id int
	var parties int
	var config string
	flag.BoolVar(&do_pprof, "pprof", false, "run for profiling")
	flag.IntVar(&id, "id", 0, "id of this party")
	flag.IntVar(&parties, "parties", 0, "number of parties")
	flag.StringVar(&config, "config", "", "config file")
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
	if ReadConfig(config) {
		parties = len(Hosts)
		SetupPeer(inputs, numBlocks, parties, id, runPeer, peerDone)
	} else if parties == 0 {
		Simulation(inputs, numBlocks, runPeer, peerDone)
	} else {
		SetupHostsPorts(parties)
		SetupPeer(inputs, numBlocks, parties, id, runPeer, peerDone)
	}

}
