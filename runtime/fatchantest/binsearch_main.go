package main

import "github.com/tjim/smpcc/runtime/base"
import "github.com/tjim/smpcc/runtime/fatchanio"
import "github.com/tjim/smpcc/runtime/yao/eval"
import "github.com/tjim/smpcc/runtime/yao/gen"
import "os"
import "log"

var _main_done = make(chan bool, 1)

func eval_binsearch(nu chan base.Chanio) {
	log.Printf("Starting eval")

	ios := make([]eval.YaoState, 14)

	for i := range ios {
		io := <-nu
//		defer close(io.Kchan2)
		ios[i] = eval.NewState(base.NewEvalX(&io))
	}

	log.Printf("Starting eval_main\n")
	go eval_main(ios[0], ios[1], ios[2], ios[3], ios[4], ios[5], ios[6], ios[7], ios[8], ios[9], ios[10], ios[11], ios[12], ios[13])
	<- _main_done
	log.Printf("Eval done\n")
}

func gen_binsearch(nu chan base.Chanio) {
	log.Printf("Starting gen\n")
	defer close(nu)

	log.Printf("Generating new genios\n")
	ios := make([]gen.YaoState, 14)
	for i := range ios {
		io := fatchanio.NewGenio(nu)
		ios[i] = gen.NewState(io)
	}

	log.Printf("Starting gen_main\n")
	go gen_main(ios[0], ios[1], ios[2], ios[3], ios[4], ios[5], ios[6], ios[7], ios[8], ios[9], ios[10], ios[11], ios[12], ios[13])
	<- _main_done
	log.Printf("Gen done\n")
}

func main() {
	addr := "127.0.0.1:3042"
	if len(os.Args) == 1 {
		fatchanio.GenClient(addr, gen_binsearch)
	} else {
		fatchanio.EvalServer(addr, eval_binsearch)
	}
}
