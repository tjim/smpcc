package eval

import (
	"crypto/aes"

	"github.com/tjim/smpcc/runtime/base"
)

import "github.com/tjim/smpcc/runtime/yaor/gen"
import "github.com/tjim/smpcc/runtime/ot"
import basegen "github.com/tjim/smpcc/runtime/gen"
import baseeval "github.com/tjim/smpcc/runtime/eval"
import "math/rand"

type ConcurrentId int64

/* YaoRState implements the EvalVM interface */
type YaoRState struct {
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

func IO(id int64) (gen.YaoRState, YaoRState) {
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
	return gen.NewYaoRState(&gio, gen.ConcurrentId(id)), YaoRState{&eio, ConcurrentId(id), 0}
}

func IOs(n int) ([]basegen.GenVM, []baseeval.EvalVM) {
	result1 := make([]basegen.GenVM, n)
	result2 := make([]baseeval.EvalVM, n)
	for i := 0; i < n; i++ {
		gio, eio := IO(int64(i))
		result1[i] = gio
		result2[i] = eio
	}
	return result1, result2
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

func decrypt(keys []base.Key, ciphertext []byte) []byte {
	result := ciphertext
	for i := len(keys); i > 0; i-- {
		result = base.Decrypt(keys[i-1], result)
	}
	return result
}

func encrypt(keys []base.Key, result []byte) []byte {
	for i := 0; i < len(keys); i++ {
		result = base.Encrypt(keys[i], result)
	}
	return result
}

func (gax YaoRState) Decrypt(t base.GarbledTable, keys ...base.Key) []byte {
	if len(keys) != 2 {
		// log.Println("Non-optimized decrypt slot")
		return Decrypt_nonoptimized(t, keys)
	}

	return decrypt(keys, t[slot(keys)])
}

func (y YaoRState) bitwise_binary_operator(io base.Evalio, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("Wire mismatch in eval.bitwise_binary_operator()")
	}
	result := make([]base.Key, len(a))
	for i := 0; i < len(a); i++ {
		t := io.RecvT()
		aa := a[i][0] % 2
		bb := b[i][0] % 2
		if aa == 0 && bb == 0 {
			result[i] = encrypt([]base.Key{a[i], b[i]}, ALL_ZEROS)
		} else {
			result[i] = decrypt([]base.Key{a[i], b[i]}, t[bb*2+aa-1])
		}
	}
	return result
}

func (y YaoRState) And(a, b []base.Key) []base.Key {
	return y.bitwise_binary_operator(y.io, a, b)
}

func (y YaoRState) Or(a, b []base.Key) []base.Key {
	return y.bitwise_binary_operator(y.io, a, b)
}

func (y YaoRState) Xor(a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("Xor(): mismatch")
	}
	result := make([]base.Key, len(a))
	for i := 0; i < len(a); i++ {
		result[i] = base.XorKey(a[i], b[i])
	}
	return result
}

func (y YaoRState) True() []base.Key {
	init_constants(y.io)
	return []base.Key{const1}
}

func (y YaoRState) False() []base.Key {
	init_constants(y.io)
	return []base.Key{const0}
}

/* Reveal to party 0 = gen */
func (y YaoRState) RevealTo0(a []base.Key) {
	for i := 0; i < len(a); i++ {
		y.io.SendK2(a[i])
	}
}

/* Reveal to party 1 = eval */
func (y YaoRState) RevealTo1(a []base.Key) []bool {
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

func (y YaoRState) ShareTo0(v uint64, bits int) []base.Key {
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
func (y YaoRState) Random(bits int) []base.Key {
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
func (y YaoRState) ShareTo1(bits int) []base.Key {
	if bits > 64 {
		panic("BT: bits > 64")
	}
	result := make([]base.Key, bits)
	for i := 0; i < bits; i++ {
		result[i] = y.io.RecvK()
	}
	return result
}
