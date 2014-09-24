/* generate.go

   Generation functions for garbled circuits

*/

package gc

import (
	"crypto/rand"
	"fmt"
	"github.com/tjim/smpcc/runtime/base"
	"io"
)

type ConcurrentId int64
type Ciphertext []byte
type Key []byte
type GarbledTable []Ciphertext
type Wire []Key // len(wire) is always 2
type Bit int8   // always either 0 or 1

func XorKey(a, b Key) Key {
	if len(a) != len(b) {
		panic("XorKey(): key length mismatch")
	}
	result := make(Key, len(a))
	for i := 0; i < len(a); i++ {
		result[i] = a[i] ^ b[i]
	}
	return result
}

// Fills keyBuf with random bytes
func GenKey(keyBuf []byte) {
	n, err := io.ReadFull(rand.Reader, keyBuf)
	if n != len(keyBuf) || err != nil {
		fmt.Println("Could not generate random key\nERROR:", err)
		panic(err)
	}
}

func Encrypt(key Key, input []byte) Ciphertext {
	return base.AESEval(key, input)
}

func Decrypt(key Key, input Ciphertext) []byte {
	return base.AESInvert(key, input)
}
