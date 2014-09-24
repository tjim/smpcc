package eval

import (
	"github.com/tjim/smpcc/runtime/base"
	"github.com/tjim/smpcc/runtime/gc"
)

import "github.com/tjim/smpcc/runtime/gc/gaxr/gen"
import "github.com/tjim/smpcc/runtime/ot"
import basegen "github.com/tjim/smpcc/runtime/gc/gen"
import baseeval "github.com/tjim/smpcc/runtime/gc/eval"
import "math/rand"

type ConcurrentId int64

/* GaxState implements the "gc/eval".VM interface */
type GaxState struct {
	io           baseeval.IO
	concurrentId ConcurrentId
	gateId       uint16
}

var (
	AESCount  uint   = 0
	ALL_ZEROS gc.Key = make([]byte, base.KEY_SIZE)
)

func NewState(io baseeval.IO, id int) GaxState {
	return GaxState{io, ConcurrentId(id), 0}
}

func IO(id int64) (gen.GaxState, GaxState) {
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
	return gen.NewGaxState(&gio, gen.ConcurrentId(id)), GaxState{&eio, ConcurrentId(id), 0}
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

func decrypt(keys []gc.Key, ciphertext, tweak []byte) (result gc.Key) {
	// log.Printf("Computing decrypt with inputs %v, %v, %v\n", keys, ciphertext, tweak)
	AESCount++
	return gc.GaXDKC_D(keys[0], keys[1], tweak, ciphertext)
}

func (gax GaxState) Decrypt(t gc.GarbledTable, keys ...gc.Key) []byte {
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

func (gax *GaxState) computeTweak() gc.Key {
	tweak := make([]byte, base.KEY_SIZE)
	tweak[0] = byte(gax.gateId)
	tweak[1] = byte(gax.gateId >> 8)
	var i uint
	for i = 0; i < 8; i++ {
		tweak[i+2] = byte(gax.concurrentId >> (8 * i))
	}
	return tweak
}

func (y GaxState) bitwise_binary_operator(io baseeval.IO, a, b []gc.Key) []gc.Key {
	if len(a) != len(b) {
		panic("Wire mismatch in eval.bitwise_binary_operator()")
	}
	result := make([]gc.Key, len(a))
	for i := 0; i < len(a); i++ {
		t := io.RecvT()
		aa := a[i][0] % 2
		bb := b[i][0] % 2
		if aa == 0 && bb == 0 {
			result[i] = gc.GaXDKC_E(a[i], b[i], y.computeTweak(), ALL_ZEROS)
		} else {
			tweak := y.computeTweak()
			result[i] = decrypt([]gc.Key{a[i], b[i]}, t[bb*2+aa-1], tweak)
		}
	}
	return result
}

func (y GaxState) And(a, b []gc.Key) []gc.Key {
	return y.bitwise_binary_operator(y.io, a, b)
}

func (y GaxState) Or(a, b []gc.Key) []gc.Key {
	return y.bitwise_binary_operator(y.io, a, b)
}

func (y GaxState) Xor(a, b []gc.Key) []gc.Key {
	if len(a) != len(b) {
		panic("Xor(): mismatch")
	}
	result := make([]gc.Key, len(a))
	for i := 0; i < len(a); i++ {
		result[i] = gc.XorKey(a[i], b[i])
	}
	return result
}

func (y GaxState) True() []gc.Key {
	init_constants(y.io)
	return []gc.Key{const1}
}

func (y GaxState) False() []gc.Key {
	init_constants(y.io)
	return []gc.Key{const0}
}

/* Reveal to party 0 = gen */
func (y GaxState) RevealTo0(a []gc.Key) {
	for i := 0; i < len(a); i++ {
		y.io.SendK2(a[i])
	}
}

/* Reveal to party 1 = eval */
func (y GaxState) RevealTo1(a []gc.Key) []bool {
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

func (y GaxState) ShareTo0(v uint64, bits int) []gc.Key {
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
func (y GaxState) Random(bits int) []gc.Key {
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
func (y GaxState) ShareTo1(bits int) []gc.Key {
	if bits > 64 {
		panic("BT: bits > 64")
	}
	result := make([]gc.Key, bits)
	for i := 0; i < bits; i++ {
		result[i] = y.io.RecvK()
	}
	return result
}
