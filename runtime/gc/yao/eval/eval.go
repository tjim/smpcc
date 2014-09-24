package eval

import (
	"github.com/tjim/smpcc/runtime/gc"
	baseeval "github.com/tjim/smpcc/runtime/gc/eval"
	basegen "github.com/tjim/smpcc/runtime/gc/gen"
	"github.com/tjim/smpcc/runtime/gc/yao/gen"
	"github.com/tjim/smpcc/runtime/ot"
	"math/rand"
)

type vm struct {
	io baseeval.IO
}

func NewVM(io baseeval.IO, id int) baseeval.VM {
	// id only to have the same type as other gc back ends (yaor, gax, gaxr)
	return vm{io}
}

func IO(id int) (basegen.VM, vm) {
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
	return gen.NewVM(&gio, id), vm{&eio}
}

func IOs(n int) ([]basegen.VM, []baseeval.VM) {
	result1 := make([]basegen.VM, n)
	result2 := make([]baseeval.VM, n)
	for i := 0; i < n; i++ {
		gio, eio := IO(i)
		result1[i] = gio
		result2[i] = eio
	}
	return result1, result2
}

var const0 gc.Key
var const1 gc.Key

func init_constants(io baseeval.IO) {
	if const0 == nil {
		const0 = io.RecvK()
		const1 = io.RecvK()
	}
}

func reset() {
	const0 = nil
	const1 = nil
}

func bitwise_binary_operator(io baseeval.IO, a, b []gc.Key) []gc.Key {
	if len(a) != len(b) {
		panic("Wire mismatch in eval.bitwise_binary_operator()")
	}
	result := make([]gc.Key, len(a))
	for i := 0; i < len(a); i++ {
		t := io.RecvT()
		result[i] = gen.Decrypt(t, a[i], b[i])
	}
	return result
}

func (y vm) And(a, b []gc.Key) []gc.Key {
	return bitwise_binary_operator(y.io, a, b)
}

func (y vm) Or(a, b []gc.Key) []gc.Key {
	return bitwise_binary_operator(y.io, a, b)
}

func (y vm) Xor(a, b []gc.Key) []gc.Key {
	if len(a) != len(b) {
		panic("Xor(): mismatch")
	}
	result := make([]gc.Key, len(a))
	for i := 0; i < len(a); i++ {
		result[i] = gc.XorKey(a[i], b[i])
	}
	return result
}

func (y vm) True() []gc.Key {
	init_constants(y.io)
	return []gc.Key{const1}
}

func (y vm) False() []gc.Key {
	init_constants(y.io)
	return []gc.Key{const0}
}

/* Reveal to party 0 = gen */
func (y vm) RevealTo0(a []gc.Key) {
	for i := 0; i < len(a); i++ {
		y.io.SendK2(a[i])
	}
}

/* Reveal to party 1 = eval */
func (y vm) RevealTo1(a []gc.Key) []bool {
	result := make([]bool, len(a))
	for i := 0; i < len(a); i++ {
		t := y.io.RecvT()
		b := gen.Decrypt(t, a[i])
		if b[0] == 0 {
			result[i] = false
		} else if b[0] == 1 {
			result[i] = true
		} else {
			panic("eval.Reveal(): invalid response")
		}
	}
	return result
}

func (y vm) ShareTo0(v uint64, bits int) []gc.Key {
	a := make([]bool, bits)
	for i := 0; i < len(a); i++ {
		bit := (v >> uint(i)) % 2
		if bit == 1 {
			a[i] = true
		} else {
			a[i] = false
		}
	}
	result := make([]gc.Key, len(a))
	for i := 0; i < len(a); i++ {
		selector := ot.Selector(0)
		if a[i] {
			selector = 1
		}
		result[i] = gc.Key(y.io.Receive(selector))
	}
	return result
}

// Random generates random bits.
func (y vm) Random(bits int) []gc.Key {
	if bits < 1 {
		panic("Random: bits < 1")
	}
	result := make([]gc.Key, bits)
	for i, _ := range result {
		selector := ot.Selector(0)
		if rand.Intn(2) != 0 {
			selector = 1
		}
		result[i] = gc.Key(y.io.Receive(selector))
	}
	return result
}

/* Bit transfer: Generator knows the bits, evaluator gets keys */
func (y vm) ShareTo1(bits int) []gc.Key {
	if bits > 64 {
		panic("BT: bits > 64")
	}
	result := make([]gc.Key, bits)
	for i := 0; i < bits; i++ {
		result[i] = y.io.RecvK()
	}
	return result
}
