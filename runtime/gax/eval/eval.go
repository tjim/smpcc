package eval

import (
	"github.com/tjim/smpcc/runtime/base"
)

import "github.com/tjim/smpcc/runtime/gax/gen"
import "github.com/tjim/smpcc/runtime/ot"
import "math/rand"

type ConcurrentId int64

/* GaxState implements the EvalVM interface */
type GaxState struct {
	io           base.Evalio
	concurrentId ConcurrentId
	gateId       uint16
}

var (
	AESCount uint = 0
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

func (y GaxState) bitwise_binary_operator(io base.Evalio, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("Wire mismatch in eval.bitwise_binary_operator()")
	}
	result := make([]base.Key, len(a))
	for i := 0; i < len(a); i++ {
		t := io.RecvT()
		result[i] = y.Decrypt(t, a[i], b[i])
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

func (y GaxState) True() []base.Key {
	init_constants(y.io)
	return []base.Key{ const1 }
}

func (y GaxState) False() []base.Key {
	init_constants(y.io)
	return []base.Key{ const0 }
}

/* Reveal to party 0 = gen */
func (y GaxState) Reveal0(a []base.Key) {
	for i := 0; i < len(a); i++ {
		y.io.SendK2(a[i])
	}
}

/* Reveal to party 1 = eval */
func (y GaxState) Reveal1(a []base.Key) []bool {
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

