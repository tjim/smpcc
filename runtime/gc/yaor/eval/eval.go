package eval

import (
	"crypto/aes"
	"github.com/tjim/smpcc/runtime/gc"
	baseeval "github.com/tjim/smpcc/runtime/gc/eval"
	basegen "github.com/tjim/smpcc/runtime/gc/gen"
	"github.com/tjim/smpcc/runtime/gc/yaor/gen"
	"github.com/tjim/smpcc/runtime/ot"
	"math/rand"
)

type ConcurrentId int64

type vm struct {
	io           baseeval.IO
	concurrentId ConcurrentId
	gateId       uint16
}

func NewVM(io baseeval.IO, id int) baseeval.VM {
	return vm{io, ConcurrentId(id), 0}
}

const (
	KEY_SIZE = aes.BlockSize
)

var (
	AESCount  uint   = 0
	ALL_ZEROS gc.Key = make([]byte, KEY_SIZE)
)

func IO(id int64) (basegen.VM, baseeval.VM) {
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
	return gen.Newvm(&gio, gen.ConcurrentId(id)), vm{&eio, ConcurrentId(id), 0}
}

func IOs(n int) ([]basegen.VM, []baseeval.VM) {
	result1 := make([]basegen.VM, n)
	result2 := make([]baseeval.VM, n)
	for i := 0; i < n; i++ {
		gio, eio := IO(int64(i))
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
		AESCount++
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
