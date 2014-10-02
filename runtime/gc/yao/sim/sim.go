package sim

import (
	"github.com/tjim/smpcc/runtime/gc"
	baseeval "github.com/tjim/smpcc/runtime/gc/eval"
	basegen "github.com/tjim/smpcc/runtime/gc/gen"
	"github.com/tjim/smpcc/runtime/gc/yao/eval"
	"github.com/tjim/smpcc/runtime/gc/yao/gen"
)

func pairVM(id gc.ConcurrentId) (basegen.VM, baseeval.VM) {
	io := gc.NewChanio()
	gchan := make(chan basegen.IOX, 1)
	echan := make(chan baseeval.IOX, 1)
	go func() {
		echan <- *baseeval.NewIOX(io)
	}()
	go func() {
		gchan <- *basegen.NewIOX(io)
	}()
	gio := <-gchan
	eio := <-echan
	return gen.NewVM(&gio, id), eval.NewVM(&eio, id)
}

func VMs(n int) ([]basegen.VM, []baseeval.VM) {
	result1 := make([]basegen.VM, n)
	result2 := make([]baseeval.VM, n)
	for i := 0; i < n; i++ {
		gio, eio := pairVM(gc.ConcurrentId(i))
		result1[i] = gio
		result2[i] = eio
	}
	return result1, result2
}
