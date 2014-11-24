package runtime

import (
	"flag"
	"fmt"
	"github.com/tjim/smpcc/runtime/gc/eval"
	"github.com/tjim/smpcc/runtime/gc/gen"
	vmeval "github.com/tjim/smpcc/runtime/gc/yao/eval"
	vmgen "github.com/tjim/smpcc/runtime/gc/yao/gen"
	"github.com/tjim/smpcc/runtime/gc/yao/sim"
	"os"
	"runtime/pprof"
)

var id int
var addr string
var args []string
var do_old bool
var do_sim bool
var do_pprof bool

func init_args() {
	flag.BoolVar(&do_pprof, "pprof", false, "run for profiling")
	flag.BoolVar(&do_old, "old", false, "use old, non-multiplex OT (default false)")
	flag.BoolVar(&do_sim, "sim", false, "run in simulation mode, single process (default false)")
	flag.IntVar(&id, "id", 0, "identity (default 0)")
	flag.StringVar(&addr, "addr", "127.0.0.1:3042", "network address (default 127.0.0.1:3042)")
	flag.Parse()
	args = flag.Args()
}

func next_arg() uint64 {
	if len(args) <= 0 {
		panic("Not enough command-line arguments")
	}
	arg := 0
	fmt.Sscanf(args[0], "%d", &arg)
	args = args[1:]
	return uint64(arg)
}

func Run(numBlocks int, gen_main func([]gen.VM), eval_main func([]eval.VM)) {
	init_args()
	if do_pprof {
		file := "cpu.pprof"
		f, err := os.Create(file)
		if err != nil {
			fmt.Println("Error: ", err)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}
	if do_sim {
		gvms, evms := sim.VMs(numBlocks + 1)
		go gen_main(gvms)
		eval_main(evms)
		fmt.Println("Done")
	} else if id == 0 && do_old {
		gen.Client(addr, gen_main, numBlocks+1, vmgen.NewVM)
	} else if id == 0 {
		gen.Client2(addr, gen_main, numBlocks+1, vmgen.NewVM)
	} else if do_old {
		eval.Server(addr, eval_main, numBlocks+1, vmeval.NewVM)
	} else {
		eval.Server2(addr, eval_main, numBlocks+1, vmeval.NewVM)
	}
}
