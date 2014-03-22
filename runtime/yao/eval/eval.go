package eval

import "github.com/tjim/smpcc/runtime/base"
import "github.com/tjim/smpcc/runtime/yao/gen"
import "fmt"
import "math"
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

func (y YaoState) Add(a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("Key mismatch in eval.Add()")
	}
	if len(a) == 0 {
		panic("empty arguments in eval.Add()")
	}
	result := make([]base.Key, len(a))
	result[0] = y.Xor(a[0:1], b[0:1])[0]
	c := y.And(a[0:1], b[0:1])[0] /* carry bit */
	for i := 1; i < len(a); i++ {
		/* compute the result bit */
		result[i] = y.Xor(y.Xor(a[i:i+1], b[i:i+1]), []base.Key{c})[0]
		/* compute the carry bit */
		t := y.io.RecvT()
		c = gen.Decrypt(t, c, a[i], b[i])
	}
	return result
}

func (y YaoState) Sub(a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Sub()")
	}
	if len(a) == 0 {
		panic("empty arguments in Sub()")
	}
	result := make([]base.Key, len(a))
	result[0] = y.Xor(a[0:1], b[0:1])[0]
	c := y.And(y.Not(a[0:1]), b[0:1])[0] /* borrow bit */
	for i := 1; i < len(a); i++ {
		/* compute the result bit */
		result[i] = y.Xor(y.Xor(a[i:i+1], b[i:i+1]), []base.Key{c})[0]
		/* compute the borrow bit */
		t := y.io.RecvT()
		c = gen.Decrypt(t, c, a[i], b[i])
	}
	return result
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

func (y YaoState) Icmp_ugt(a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_ugt()")
	}
	if len(a) == 0 {
		return y.Const(0)
	}
	highbit := len(a) - 1
	t := y.io.RecvT()
	highbit_gt := make([]base.Key, 1)
	highbit_gt[0] = gen.Decrypt(t, a[highbit], b[highbit])
	t = y.io.RecvT()
	highbit_eq := make([]base.Key, 1)
	highbit_eq[0] = gen.Decrypt(t, a[highbit], b[highbit])
	return y.Or(highbit_gt, y.And(highbit_eq, y.Icmp_ugt(a[:highbit], b[:highbit])))
}

func (y YaoState) Icmp_uge(a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_uge()")
	}
	if len(a) == 0 {
		return y.Const(0)
	}
	highbit := len(a) - 1
	t := y.io.RecvT()
	highbit_gt := make([]base.Key, 1)
	highbit_gt[0] = gen.Decrypt(t, a[highbit], b[highbit])
	t = y.io.RecvT()
	highbit_eq := make([]base.Key, 1)
	highbit_eq[0] = gen.Decrypt(t, a[highbit], b[highbit])
	return y.Or(highbit_gt, y.And(highbit_eq, y.Icmp_uge(a[1:], b[1:])))
}

func (y YaoState) Select(s, a, b []base.Key) []base.Key {
	if len(s) != 1 {
		panic("Wire mismatch in eval.Select()")
	}
	if len(a) != len(b) {
		panic("Wire mismatch in eval.Select()")
	}
	result := make([]base.Key, len(a))
	for i := 0; i < len(a); i++ {
		t := y.io.RecvT()
		result[i] = gen.Decrypt(t, s[0], a[i], b[i])
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

func (y YaoState) Uint(a uint64, width int) []base.Key {
	if width > 64 {
		panic("Uint: width > 64")
	}
	init_constants(y.io)
	result := make([]base.Key, width)
	for i := 0; i < width; i++ {
		if (a>>uint(i))%2 == 0 {
			result[i] = const0
		} else {
			result[i] = const1
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

func (y YaoState) Reveal(a []base.Key) []bool {
	result := make([]bool, len(a))
	for i := 0; i < len(a); i++ {
		t := y.io.RecvT()
		y.io.SendK2(a[i])
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

/* Gen-side load that ignores numelts */
func (y YaoState) Load(loc, numelts, eltsize []base.Key) []base.Key {
	if len(loc) < 8 {
		panic("Load: bad address")
	}
	loc_len := int(math.Min(25, (float64(len(loc)))))
	loc = loc[:loc_len]
	for i := 0; i < len(loc); i++ {
		y.io.SendK2(loc[i])
	}
	b_eltsize := y.Reveal(eltsize)
	v_eltsize := 0
	for i := 0; i < len(b_eltsize); i++ {
		if b_eltsize[i] {
			v_eltsize += 1 << uint(i)
		}
	}
	switch v_eltsize {
	default:
		panic(fmt.Sprintf("Load: bad element size %d", v_eltsize))
	case 1, 2, 4, 8:
	}
	return y.BT(v_eltsize * 8)
}

/* Gen-side store that ignores numelts */
func (y YaoState) Store(loc, numelts, eltsize, val []base.Key) {
	if len(loc) < 8 {
		panic("Load: bad address")
	}
	loc_len := int(math.Min(25, (float64(len(loc)))))
	loc = loc[:loc_len]
	for i := 0; i < len(loc); i++ {
		y.io.SendK2(loc[i])
	}
	b_eltsize := y.Reveal(eltsize)
	v_eltsize := 0
	for i := 0; i < len(b_eltsize); i++ {
		if b_eltsize[i] {
			v_eltsize += 1 << uint(i)
		}
	}
	switch v_eltsize {
	default:
		panic(fmt.Sprintf("Load: bad element size %d", v_eltsize))
	case 1, 2, 4, 8:
	}
	for i := 0; i < len(val); i++ {
		y.io.SendK2(val[i])
	}
}
