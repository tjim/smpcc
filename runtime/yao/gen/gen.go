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

func (y YaoState) True() []base.Wire {
	init_constants(y.io)
	return []base.Wire{ const1 }
}

func (y YaoState) False() []base.Wire {
	init_constants(y.io)
	return []base.Wire{ const0 }
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
