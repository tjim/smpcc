package gen

import (
	"bytes"
	"crypto/aes"
	"fmt"
	"math/rand"

	"github.com/tjim/smpcc/runtime/base"
	"github.com/tjim/smpcc/runtime/ot"
)

var (
	AESCount int = 0
)

/* YaoState implements the GenVM interface */
type YaoState struct {
	io base.Genio
}

func NewYaoState(io base.Genio) YaoState {
	return YaoState{io}
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

func encrypt(keys []base.Key, result []byte) []byte {
	for i := 0; i < len(keys); i++ {
		result = base.Encrypt(keys[i], result)
		AESCount++
	}
	return result
}

func decrypt(keys []base.Key, ciphertext []byte) []byte {
	result := ciphertext
	for i := len(keys); i > 0; i-- {
		result = base.Decrypt(keys[i-1], result)
		AESCount++
	}
	return result
}

func encrypt_slot(t base.GarbledTable, plaintext []byte, keys ...base.Key) {
	t[slot(keys)] = encrypt(keys, plaintext)
}

func Decrypt(t base.GarbledTable, keys ...base.Key) []byte {
	return decrypt(keys, t[slot(keys)])
}

const (
	KEY_SIZE = aes.BlockSize
)

var key0 base.Key    // The XOR random constant
var const0 base.Wire // A wire for a constant 0 bit with unbounded fanout
var const1 base.Wire // A wire for a constant 1 bit with unbounded fanout

func init_key0() {
	if key0 != nil {
		return
	}
	key0 = make([]byte, KEY_SIZE)
	base.GenKey(key0) // least significant bit is random...
	key0[0] |= 1      // ...force it to 1
}

func init_constants(io base.Genio) {
	if const0 == nil {
		const0 = genWire()
		const1 = genWire()
		io.SendK(const0[0])
		io.SendK(const1[1])
	}
}

func reset() {
	key0 = nil
	const0 = nil
	const1 = nil
}

// Generates two keys of size KEY_SIZE and returns the pair
func genWire() base.Wire {
	init_key0()
	k0 := make([]byte, KEY_SIZE)
	base.GenKey(k0)
	k1 := base.XorKey(k0, key0)
	return []base.Key{k0, k1}
}

// Generates an array of wires. A wire is a pair of keys.
func genWires(size int) []base.Wire {
	if size <= 0 {
		panic("genWires with request <= 0")
	}
	res := make([]base.Wire, size)
	for i := 0; i < size; i++ {
		res[i] = genWire()
	}
	return res
}

/* http://www.llvm.org/docs/LangRef.html */

func (y YaoState) Add(a, b []base.Wire) []base.Wire {
	if len(a) != len(b) {
		panic(fmt.Sprintf("Wire mismatch in gen.Add(), %d vs %d", len(a), len(b)))
	}
	if len(a) == 0 {
		panic("empty arguments in gen.Add()")
	}
	result := make([]base.Wire, len(a))
	result[0] = y.Xor(a[0:1], b[0:1])[0]
	c := y.And(a[0:1], b[0:1])[0] /* carry bit */
	for i := 1; i < len(a); i++ {
		/* compute the result bit */
		result[i] = y.Xor(y.Xor(a[i:i+1], b[i:i+1]), []base.Wire{c})[0]
		/* compute the carry bit. */
		w := genWire()
		t := make([]base.Ciphertext, 8)
		encrypt_slot(t, w[0], c[0], a[i][0], b[i][0])
		encrypt_slot(t, w[0], c[0], a[i][0], b[i][1])
		encrypt_slot(t, w[0], c[0], a[i][1], b[i][0])
		encrypt_slot(t, w[1], c[0], a[i][1], b[i][1])
		encrypt_slot(t, w[0], c[1], a[i][0], b[i][0])
		encrypt_slot(t, w[1], c[1], a[i][0], b[i][1])
		encrypt_slot(t, w[1], c[1], a[i][1], b[i][0])
		encrypt_slot(t, w[1], c[1], a[i][1], b[i][1])
		y.io.SendT(t)
		c = w
	}
	return result
}

func (y YaoState) Sub(a, b []base.Wire) []base.Wire {
	if len(a) != len(b) {
		panic("argument mismatch in Sub()")
	}
	if len(a) == 0 {
		panic("empty arguments in Sub()")
	}
	result := make([]base.Wire, len(a))
	result[0] = y.Xor(a[0:1], b[0:1])[0]
	c := y.And(y.Not(a[0:1]), b[0:1])[0] /* borrow bit */
	for i := 1; i < len(a); i++ {
		/* compute the result bit */
		result[i] = y.Xor(y.Xor(a[i:i+1], b[i:i+1]), []base.Wire{c})[0]
		/* compute the borrow bit. */
		w := genWire()
		t := make([]base.Ciphertext, 8)
		encrypt_slot(t, w[0], c[0], a[i][0], b[i][0])
		encrypt_slot(t, w[1], c[0], a[i][0], b[i][1])
		encrypt_slot(t, w[0], c[0], a[i][1], b[i][0])
		encrypt_slot(t, w[0], c[0], a[i][1], b[i][1])
		encrypt_slot(t, w[1], c[1], a[i][0], b[i][0])
		encrypt_slot(t, w[1], c[1], a[i][0], b[i][1])
		encrypt_slot(t, w[0], c[1], a[i][1], b[i][0])
		encrypt_slot(t, w[1], c[1], a[i][1], b[i][1])
		y.io.SendT(t)
		c = w
	}
	return result
}

func (y YaoState) And(a, b []base.Wire) []base.Wire {
	if len(a) != len(b) {
		panic("Wire mismatch in gen.And()")
	}
	result := make([]base.Wire, len(a))
	for i := 0; i < len(a); i++ {
		w := genWire()
		result[i] = w
		t := make([]base.Ciphertext, 4)
		encrypt_slot(t, w[0], a[i][0], b[i][0])
		encrypt_slot(t, w[0], a[i][0], b[i][1])
		encrypt_slot(t, w[0], a[i][1], b[i][0])
		encrypt_slot(t, w[1], a[i][1], b[i][1])
		y.io.SendT(t)
	}
	return result
}

func (y YaoState) Or(a, b []base.Wire) []base.Wire {
	if len(a) != len(b) {
		panic("Wire mismatch in gen.Or()")
	}
	result := make([]base.Wire, len(a))
	for i := 0; i < len(a); i++ {
		w := genWire()
		result[i] = w
		t := make([]base.Ciphertext, 4)
		encrypt_slot(t, w[0], a[i][0], b[i][0])
		encrypt_slot(t, w[1], a[i][0], b[i][1])
		encrypt_slot(t, w[1], a[i][1], b[i][0])
		encrypt_slot(t, w[1], a[i][1], b[i][1])
		y.io.SendT(t)
	}
	return result
}

func (y YaoState) Xor(a, b []base.Wire) []base.Wire {
	if len(a) != len(b) {
		panic("Xor(): mismatch")
	}
	result := make([]base.Wire, len(a))
	for i := 0; i < len(a); i++ {
		k0 := base.XorKey(a[i][0], b[i][0])
		k1 := base.XorKey(a[i][0], b[i][1])
		result[i] = []base.Key{k0, k1}
	}
	return result
}

func (y YaoState) Icmp_ugt(a, b []base.Wire) []base.Wire {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_ugt()")
	}
	if len(a) == 0 {
		return y.Const(0)
	}
	highbit := len(a) - 1
	highbit_gt := make([]base.Wire, 1)
	highbit_gt[0] = genWire()
	t := make([]base.Ciphertext, 4)
	encrypt_slot(t, highbit_gt[0][0], a[highbit][0], b[highbit][0])
	encrypt_slot(t, highbit_gt[0][0], a[highbit][0], b[highbit][1])
	encrypt_slot(t, highbit_gt[0][1], a[highbit][1], b[highbit][0])
	encrypt_slot(t, highbit_gt[0][0], a[highbit][1], b[highbit][1])
	y.io.SendT(t)
	highbit_eq := make([]base.Wire, 1)
	highbit_eq[0] = genWire()
	t = make([]base.Ciphertext, 4)
	encrypt_slot(t, highbit_eq[0][1], a[highbit][0], b[highbit][0])
	encrypt_slot(t, highbit_eq[0][0], a[highbit][0], b[highbit][1])
	encrypt_slot(t, highbit_eq[0][0], a[highbit][1], b[highbit][0])
	encrypt_slot(t, highbit_eq[0][1], a[highbit][1], b[highbit][1])
	y.io.SendT(t)
	return y.Or(highbit_gt, y.And(highbit_eq, y.Icmp_ugt(a[:highbit], b[:highbit])))
}

func (y YaoState) Icmp_uge(a, b []base.Wire) []base.Wire {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_uge()")
	}
	if len(a) == 0 {
		return y.Const(0)
	}
	highbit := len(a) - 1
	highbit_gt := make([]base.Wire, 1)
	highbit_gt[0] = genWire()
	t := make([]base.Ciphertext, 4)
	encrypt_slot(t, highbit_gt[0][0], a[highbit][0], b[highbit][0])
	encrypt_slot(t, highbit_gt[0][0], a[highbit][0], b[highbit][1])
	encrypt_slot(t, highbit_gt[0][1], a[highbit][1], b[highbit][0])
	encrypt_slot(t, highbit_gt[0][0], a[highbit][1], b[highbit][1])
	y.io.SendT(t)
	highbit_eq := make([]base.Wire, 1)
	highbit_eq[0] = genWire()
	t = make([]base.Ciphertext, 4)
	encrypt_slot(t, highbit_eq[0][1], a[highbit][0], b[highbit][0])
	encrypt_slot(t, highbit_eq[0][0], a[highbit][0], b[highbit][1])
	encrypt_slot(t, highbit_eq[0][0], a[highbit][1], b[highbit][0])
	encrypt_slot(t, highbit_eq[0][1], a[highbit][1], b[highbit][1])
	y.io.SendT(t)
	return y.Or(highbit_gt, y.And(highbit_eq, y.Icmp_uge(a[1:], b[1:])))
}

func (y YaoState) Select(s, a, b []base.Wire) []base.Wire {
	if len(s) != 1 {
		panic("Wire mismatch in gen.Select()")
	}
	if len(a) != len(b) {
		panic("Wire mismatch in gen.Select()")
	}
	result := make([]base.Wire, len(a))

	for i := 0; i < len(a); i++ {
		// result[i] = y.Or(y.And(s, a[i:i+1]), y.And(y.Not(s), b[i:i+1]))[0]
		result[i] = y.Xor(b[i:i+1], y.And(s, y.Xor(a[i:i+1], b[i:i+1])))[0]
	}
	return result
}

/* example: Const(0,1,0,0) is 8 */
func (y YaoState) Const(bits ...int) []base.Wire {
	if len(bits) == 0 {
		panic("Const with no bits")
	}
	init_constants(y.io)
	result := make([]base.Wire, len(bits))
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

func (y YaoState) Uint(a uint64, width int) []base.Wire {
	if width > 64 {
		panic("Uint: width > 64")
	}
	init_constants(y.io)
	result := make([]base.Wire, width)
	for i := 0; i < width; i++ {
		if (a>>uint(i))%2 == 0 {
			result[i] = const0
		} else {
			result[i] = const1
		}
	}
	return result
}

func (y YaoState) Nand(a, b []base.Wire) []base.Wire {
	if len(a) != len(b) {
		panic("Wire mismatch in gen.Nand()")
	}
	result := make([]base.Wire, len(a))
	for i := 0; i < len(a); i++ {
		w := genWire()
		result[i] = w
		t := make([]base.Ciphertext, 4)
		encrypt_slot(t, w[1], a[i][0], b[i][0])
		encrypt_slot(t, w[1], a[i][0], b[i][1])
		encrypt_slot(t, w[1], a[i][1], b[i][0])
		encrypt_slot(t, w[0], a[i][1], b[i][1])
		y.io.SendT(t)
	}
	return result
}

/* We use the free XOR and unbounded fanout of constant bits */
func (y YaoState) Not(a []base.Wire) []base.Wire {
	init_constants(y.io)
	ones := make([]base.Wire, len(a))
	for i := 0; i < len(ones); i++ {
		ones[i] = const1
	}
	return y.Xor(a, ones)
}

/* Reveal to all parties */
func (y YaoState) Reveal(a []base.Wire) []bool {
	y.Reveal1(a)
	return y.Reveal0(a)
}

/* Reveal to party 0 = gen */
func (y YaoState) Reveal0(a []base.Wire) []bool {
	result := make([]bool, len(a))
	for i := 0; i < len(a); i++ {
		bit := resolveKey(a[i], y.io.RecvK2())
		if bit == 0 {
			result[i] = false
		} else {
			result[i] = true
		}
	}
	return result
}

/* Reveal to party 1 = eval */
func (y YaoState) Reveal1(a []base.Wire) {
	for i := 0; i < len(a); i++ {
		t := make([]base.Ciphertext, 2)
		w := genWire()
		w[0][0] = 0
		w[1][0] = 1
		encrypt_slot(t, w[0], a[i][0])
		encrypt_slot(t, w[1], a[i][1])
		y.io.SendT(t)
	}
}

func (y YaoState) OT(bits int) []base.Wire {
	a := make([]base.Wire, bits)
	for i := 0; i < len(a); i++ {
		w := genWire()
		a[i] = w
		y.io.Send(ot.Message(w[0]), ot.Message(w[1]))
	}
	return a
}

func (y YaoState) BT(a uint64, bits int) []base.Wire {
	if bits > 64 {
		panic("BT: bits > 64")
	}
	result := make([]base.Wire, bits)
	for i := 0; i < bits; i++ {
		w := genWire()
		result[i] = w
		if (a>>uint(i))%2 == 0 {
			y.io.SendK(w[0])
		} else {
			y.io.SendK(w[1])
		}
	}
	return result
}

// Random generates random bits.
func (y YaoState) Random(bits int) []base.Wire {
	if bits < 1 {
		panic("Random: bits < 1")
	}
	result := make([]base.Wire, bits)
	for i, _ := range result {
		w := genWire()
		result[i] = w
		switch rand.Intn(2) {
		case 0:
			y.io.Send(ot.Message(w[0]), ot.Message(w[1]))
		default:
			y.io.Send(ot.Message(w[1]), ot.Message(w[0]))
		}
	}
	return result
}

func resolveKey(w base.Wire, k base.Key) int {
	if bytes.Equal(k, w[0]) {
		return 0
	} else if bytes.Equal(k, w[1]) {
		return 1
	} else {
		panic(fmt.Sprintf("resolveKey(): key and wire mismatch\nKey: %v\nWire: %v\n", k, w))
	}
	panic("unreachable")
}
