package eval

import (
	"crypto/aes"

	"github.com/tjim/smpcc/runtime/base"
)

import "github.com/tjim/smpcc/runtime/gaxr/gen"
import "fmt"
import "math"
import "github.com/tjim/smpcc/runtime/ot"
import "math/rand"

type ConcurrentId int64

/* GaxState implements the EvalVM interface */
type GaxState struct {
	io           base.Evalio
	concurrentId ConcurrentId
	gateId       uint16
}

const (
	KEY_SIZE = aes.BlockSize
)

var (
	AESCount  uint     = 0
	ALL_ZEROS base.Key = make([]byte, KEY_SIZE)
)

func IO(id int64) (gen.GaxState, GaxState) {
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
	return gen.NewGaxState(&gio, gen.ConcurrentId(id)), GaxState{&eio, ConcurrentId(id), 0}
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

func slot(keys []base.Key) int {
	result := 0
	for i := 0; i < len(keys); i++ {
		key := keys[i]
		result *= 2
		result += int(key[0] % 2)
	}
	return result
}

func findZeroSlots(a, b []base.Key) (res []bool) {
	res = make([]bool, len(a))
	for i := 0; i < len(a); i++ {
		if a[i][0]%2 == 0 && b[i][0]%2 == 0 {
			res[i] = true
		} else {
			res[i] = false
		}
	}
	return
}

func decrypt_nonoptimized(keys []base.Key, ciphertext []byte) []byte {
	result := ciphertext
	for i := len(keys); i > 0; i-- {
		result = base.Decrypt(keys[i-1], result)
		AESCount++
	}
	// log.Printf("Decrypt_nonoptimized, result = %v\n", result)
	return result
}

func Decrypt_nonoptimized(t base.GarbledTable, keys []base.Key) []byte {
	return decrypt_nonoptimized(keys, t[slot(keys)])
}

func decrypt(keys []base.Key, ciphertext, tweak []byte) (result base.Key) {
	// log.Printf("Computing decrypt with inputs %v, %v, %v\n", keys, ciphertext, tweak)
	AESCount++
	return base.GaXDKC_D(keys[0], keys[1], tweak, ciphertext)
}

func (gax GaxState) Decrypt(t base.GarbledTable, keys ...base.Key) []byte {
	if len(keys) != 2 {
		// log.Println("Non-optimized decrypt slot")
		return Decrypt_nonoptimized(t, keys)
	}
	// log.Println("Optimized decrypt slot")
	tweak := make([]byte, base.KEY_SIZE)
	tweak[0] = byte(gax.gateId)
	tweak[1] = byte(gax.gateId >> 8)
	var i uint
	for i = 0; i < 8; i++ {
		tweak[i+2] = byte(gax.concurrentId >> (8 * i))
	}

	return decrypt(keys, t[slot(keys)], tweak)
}

func (y GaxState) Add(a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("key mismatch in eval.Add()")
	}
	if len(a) == 0 {
		panic("empty arguments in eval.Add()")
	}
	result := make([]base.Key, len(a))
	result[0] = y.Xor(a[0:1], b[0:1])[0]
	c := y.And(a[0:1], b[0:1]) /* carry bit */
	for i := 1; i < len(a); i++ {
		/* compute the result bit */
		inner := y.Xor(a[i:i+1], b[i:i+1])
		// fmt.Printf("gen inner length %d\n", len(inner))
		result[i] = y.Xor(inner, c)[0]

		/* compute the next carry bit w2. */
		and1 := y.And(a[i:i+1], b[i:i+1])
		and2 := y.And(inner, c)
		or1 := y.Or(and1, and2)

		// set the carry output
		c = or1
	}
	return result
}

func (y GaxState) Sub(a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Sub()")
	}
	if len(a) == 0 {
		panic("empty arguments in Sub()")
	}
	result := make([]base.Key, len(a))
	result[0] = y.Xor(a[0:1], b[0:1])[0]
	c := y.And(y.Not(a[0:1]), b[0:1]) /* borrow bit */
	for i := 1; i < len(a); i++ {
		/* compute the result bit */
		inner := y.Xor(a[i:i+1], b[i:i+1])
		// fmt.Printf("gen inner length %d\n", len(inner))
		result[i] = y.Xor(inner, c)[0]

		/* compute the next carry bit w2. */
		and1 := y.And(a[i:i+1], y.Not(b[i:i+1]))
		and2 := y.And(y.Not(inner), c)
		or1 := y.Or(and1, and2)

		// set the carry output
		c = or1
	}
	return result
}

func (gax *GaxState) computeTweak() base.Key {
	tweak := make([]byte, base.KEY_SIZE)
	tweak[0] = byte(gax.gateId)
	tweak[1] = byte(gax.gateId >> 8)
	var i uint
	for i = 0; i < 8; i++ {
		tweak[i+2] = byte(gax.concurrentId >> (8 * i))
	}
	return tweak
}

func (y GaxState) bitwise_binary_operator(io base.Evalio, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("Wire mismatch in eval.bitwise_binary_operator()")
	}
	result := make([]base.Key, len(a))
	for i := 0; i < len(a); i++ {
		t := io.RecvT()
		aa := a[i][0] % 2
		bb := b[i][0] % 2
		if aa == 0 && bb == 0 {
			result[i] = base.GaXDKC_E(a[i], b[i], y.computeTweak(), ALL_ZEROS)
		} else {
			tweak := y.computeTweak()
			result[i] = decrypt([]base.Key{a[i], b[i]}, t[bb*2+aa-1], tweak)
		}
	}
	return result
}

func (y GaxState) And(a, b []base.Key) []base.Key {
	return y.bitwise_binary_operator(y.io, a, b)
}

func (y GaxState) Or(a, b []base.Key) []base.Key {
	return y.bitwise_binary_operator(y.io, a, b)
}

func (y GaxState) Xor(a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("Xor(): mismatch")
	}
	result := make([]base.Key, len(a))
	for i := 0; i < len(a); i++ {
		result[i] = base.XorKey(a[i], b[i])
	}
	return result
}

func (y GaxState) Icmp_ugt(a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_ugt()")
	}
	if len(a) == 0 {
		return y.Const(0)
	}
	highbit := len(a) - 1
	t := y.io.RecvT()
	highbit_gt := make([]base.Key, 1)
	highbit_gt[0] = y.Decrypt(t, a[highbit], b[highbit])
	t = y.io.RecvT()
	highbit_eq := make([]base.Key, 1)
	highbit_eq[0] = y.Decrypt(t, a[highbit], b[highbit])
	return y.Or(highbit_gt, y.And(highbit_eq, y.Icmp_ugt(a[:highbit], b[:highbit])))
}

func (y GaxState) Icmp_uge(a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_uge()")
	}
	if len(a) == 0 {
		return y.Const(0)
	}
	highbit := len(a) - 1
	t := y.io.RecvT()
	highbit_gt := make([]base.Key, 1)
	highbit_gt[0] = y.Decrypt(t, a[highbit], b[highbit])
	t = y.io.RecvT()
	highbit_eq := make([]base.Key, 1)
	highbit_eq[0] = y.Decrypt(t, a[highbit], b[highbit])
	return y.Or(highbit_gt, y.And(highbit_eq, y.Icmp_uge(a[1:], b[1:])))
}

func (y GaxState) Select(s, a, b []base.Key) []base.Key {
	if len(s) != 1 {
		panic("Wire mismatch in eval.Select()")
	}
	if len(a) != len(b) {
		panic("Wire mismatch in eval.Select()")
	}
	result := make([]base.Key, len(a))
	for i := 0; i < len(a); i++ {
		// result[i] = y.Or(y.And(s, a[i:i+1]), y.And(y.Not(s), b[i:i+1]))[0]
		result[i] = y.Xor(b[i:i+1], y.And(s, y.Xor(a[i:i+1], b[i:i+1])))[0]
	}
	return result
}

func (y GaxState) Const(bits ...int) []base.Key {
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

func (y GaxState) Uint(a uint64, width int) []base.Key {
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

func (y GaxState) Nand(a, b []base.Key) []base.Key {
	return y.bitwise_binary_operator(y.io, a, b)
}

/* We use the free XOR and unbounded fanout of constant bits */
func (y GaxState) Not(a []base.Key) []base.Key {
	init_constants(y.io)
	ones := make([]base.Key, len(a))
	for i := 0; i < len(ones); i++ {
		ones[i] = const1
	}
	return y.Xor(a, ones)
}

func (y GaxState) Reveal(a []base.Key) []bool {
	// log.Printf("Inside gen reveal const0=%v, const1=%v\n", const0, const1)
	result := make([]bool, len(a))
	for i := 0; i < len(a); i++ {
		t := y.io.RecvT()
		y.io.SendK2(a[i])
		b := y.Decrypt(t, a[i])
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
func (y GaxState) OT(v uint64, bits int) []base.Key {
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
func (y GaxState) Random(bits int) []base.Key {
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
func (y GaxState) BT(bits int) []base.Key {
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
func (y GaxState) Load(loc, numelts, eltsize []base.Key) []base.Key {
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
func (y GaxState) Store(loc, numelts, eltsize, val []base.Key) {
	if len(loc) < 8 {
		panic("Store: bad address")
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
		panic(fmt.Sprintf("Store: bad element size %d", v_eltsize))
	case 1, 2, 4, 8:
	}
	for i := 0; i < len(val); i++ {
		y.io.SendK2(val[i])
	}
}
