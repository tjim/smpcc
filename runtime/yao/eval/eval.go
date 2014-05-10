package eval

import "github.com/tjim/smpcc/runtime/base"
import "github.com/tjim/smpcc/runtime/yao/gen"
import "github.com/tjim/smpcc/runtime/ot"
import "math/rand"

/* YaoState implements the EvalVM interface */
type YaoState struct {
	io base.Evalio
}

func IO(id int) (gen.YaoState, YaoState) {
	io := base.NewChanio()
	gchan := make(chan base.GenX, 1)
	echan := make(chan base.EvalX, 1)
	go func() {
		echan <- *base.NewEvalX(io)
	}()
	go func() {
		gchan <- *base.NewGenX(io)
	}()
	gio := <-gchan
	eio := <-echan
	return gen.NewYaoState(&gio), YaoState{&eio}
}

var const0 base.Key
var const1 base.Key

func init_constants(io base.Evalio) {
	if const0 == nil {
		const0 = io.RecvK()
		const1 = io.RecvK()
	}
}

func reset() {
	const0 = nil
	const1 = nil
}

func bitwise_binary_operator(io base.Evalio, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("Wire mismatch in eval.bitwise_binary_operator()")
	}
	result := make([]base.Key, len(a))
	for i := 0; i < len(a); i++ {
		t := io.RecvT()
		result[i] = gen.Decrypt(t, a[i], b[i])
	}
	return result
}

func (y YaoState) And(a, b []base.Key) []base.Key {
	return bitwise_binary_operator(y.io, a, b)
}

func (y YaoState) Or(a, b []base.Key) []base.Key {
	return bitwise_binary_operator(y.io, a, b)
}

func (y YaoState) Xor(a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("Xor(): mismatch")
	}
	result := make([]base.Key, len(a))
	for i := 0; i < len(a); i++ {
		result[i] = base.XorKey(a[i], b[i])
	}
	return result
}

func (y YaoState) Const(bits ...int) []base.Key {
	if len(bits) == 0 {
		panic("Const with no bits")
	}
	init_constants(y.io)
	result := make([]base.Key, len(bits))
	/* count down: leftmost argument bits[0] is high-order bit */
	for i := len(bits); i > 0; i-- {
		if bits[i-1] == 0 {
			result[i-1] = const0
		} else {
			result[i-1] = const1
		}
	}
	return result
}

func (y YaoState) Nand(a, b []base.Key) []base.Key {
	return bitwise_binary_operator(y.io, a, b)
}

/* We use the free XOR and unbounded fanout of constant bits */
func (y YaoState) Not(a []base.Key) []base.Key {
	init_constants(y.io)
	ones := make([]base.Key, len(a))
	for i := 0; i < len(ones); i++ {
		ones[i] = const1
	}
	return y.Xor(a, ones)
}

/* Reveal to party 0 = gen */
func (y YaoState) Reveal0(a []base.Key) {
	for i := 0; i < len(a); i++ {
		y.io.SendK2(a[i])
	}
}

/* Reveal to party 1 = eval */
func (y YaoState) Reveal1(a []base.Key) []bool {
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

func (y YaoState) OT(v uint64, bits int) []base.Key {
	a := make([]bool, bits)
	for i := 0; i < len(a); i++ {
		bit := (v >> uint(i)) % 2
		if bit == 1 {
			a[i] = true
		} else {
			a[i] = false
		}
	}
	result := make([]base.Key, len(a))
	for i := 0; i < len(a); i++ {
		selector := ot.Selector(0)
		if a[i] {
			selector = 1
		}
		result[i] = base.Key(y.io.Receive(selector))
	}
	return result
}

// Random generates random bits.
func (y YaoState) Random(bits int) []base.Key {
	if bits < 1 {
		panic("Random: bits < 1")
	}
	result := make([]base.Key, bits)
	for i, _ := range result {
		selector := ot.Selector(0)
		if rand.Intn(2) != 0 {
			selector = 1
		}
		result[i] = base.Key(y.io.Receive(selector))
	}
	return result
}

/* Bit transfer: Generator knows the bits, evaluator gets keys */
func (y YaoState) BT(bits int) []base.Key {
	if bits > 64 {
		panic("BT: bits > 64")
	}
	result := make([]base.Key, bits)
	for i := 0; i < bits; i++ {
		result[i] = y.io.RecvK()
	}
	return result
}
