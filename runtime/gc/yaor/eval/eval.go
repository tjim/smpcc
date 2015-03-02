package eval

import (
	"crypto/aes"
	"github.com/tjim/smpcc/runtime/bit"
	"github.com/tjim/smpcc/runtime/gc"
	baseeval "github.com/tjim/smpcc/runtime/gc/eval"
	"github.com/tjim/smpcc/runtime/ot"
)

type vm struct {
	io           baseeval.IO
	concurrentId gc.ConcurrentId
	gateId       uint16
}

func NewVM(io baseeval.IO, id gc.ConcurrentId) baseeval.VM {
	return vm{io, id, 0}
}

const (
	KEY_SIZE = aes.BlockSize
)

var (
	ALL_ZEROS gc.Key = make([]byte, KEY_SIZE)
)

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

func slot(keys []gc.Key) int {
	result := 0
	for i := 0; i < len(keys); i++ {
		key := keys[i]
		result *= 2
		result += int(key[0] % 2)
	}
	return result
}

func decrypt_nonoptimized(keys []gc.Key, ciphertext []byte) []byte {
	result := ciphertext
	for i := len(keys); i > 0; i-- {
		result = gc.Decrypt(keys[i-1], result)
	}
	// log.Printf("Decrypt_nonoptimized, result = %v\n", result)
	return result
}

func Decrypt_nonoptimized(t gc.GarbledTable, keys []gc.Key) []byte {
	return decrypt_nonoptimized(keys, t[slot(keys)])
}

func decrypt(keys []gc.Key, ciphertext []byte) []byte {
	result := ciphertext
	for i := len(keys); i > 0; i-- {
		result = gc.Decrypt(keys[i-1], result)
	}
	return result
}

func encrypt(keys []gc.Key, result []byte) []byte {
	for i := 0; i < len(keys); i++ {
		result = gc.Encrypt(keys[i], result)
	}
	return result
}

func (gax vm) Decrypt(t gc.GarbledTable, keys ...gc.Key) []byte {
	if len(keys) != 2 {
		// log.Println("Non-optimized decrypt slot")
		return Decrypt_nonoptimized(t, keys)
	}

	return decrypt(keys, t[slot(keys)])
}

func (y vm) bitwise_binary_operator(io baseeval.IO, a, b []gc.Key) []gc.Key {
	if len(a) != len(b) {
		panic("Wire mismatch in eval.bitwise_binary_operator()")
	}
	result := make([]gc.Key, len(a))
	for i := 0; i < len(a); i++ {
		t := io.RecvT()
		aa := a[i][0] % 2
		bb := b[i][0] % 2
		if aa == 0 && bb == 0 {
			result[i] = encrypt([]gc.Key{a[i], b[i]}, ALL_ZEROS)
		} else {
			result[i] = decrypt([]gc.Key{a[i], b[i]}, t[bb*2+aa-1])
		}
	}
	return result
}

func (y vm) And(a, b []gc.Key) []gc.Key {
	return y.bitwise_binary_operator(y.io, a, b)
}

func (y vm) Or(a, b []gc.Key) []gc.Key {
	return y.bitwise_binary_operator(y.io, a, b)
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
	numBytes := bits / 8
	if bits%8 != 0 {
		numBytes++
	}
	random := make([]byte, numBytes)
	gc.GenKey(random)
	for i, _ := range result {
		selector := ot.Selector(0)
		if bit.GetBit(random, i) != 0 {
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
