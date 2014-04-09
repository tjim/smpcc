package base

import (
	"crypto/aes"
	"crypto/cipher"
	"math/big"
)

var FIXED_KEY Key = []byte{83, 36, 191, 126, 172, 151, 226, 234, 140, 225, 71, 219, 216, 96, 130, 209, 17,
	13, 67, 12, 74, 207, 217, 7, 20, 13, 151, 20, 179, 221, 190, 245}

var aesprf cipher.Block

type DKC interface {
	E(A, B, T, X Key) Key
	D(A, B, T, P Key) Key
}

func init() {
	a, err := aes.NewCipher(FIXED_KEY)
	if err != nil {
		panic(err)
	}
	aesprf = a
}

//--- Ga

func GaDKC_E(A, B, T, X Key) Key {
	K := XorKey(A, B)
	K = XorKey(K, T)
	ciphertext := make([]byte, aes.BlockSize)
	aesprf.Encrypt(ciphertext, K)

	rho := XorKey(ciphertext, K)
	return XorKey(rho, X)
}

func GaDKC_D(A, B, T, P Key) Key {
	K := XorKey(A, B)
	K = XorKey(K, T)
	ciphertext := make([]byte, aes.BlockSize)
	aesprf.Encrypt(ciphertext, K)

	rho := XorKey(ciphertext, K)
	return XorKey(rho, P)
}

//--- GaX

func GaXDKC_E(A, B, T, X Key) Key {
	if len(A) != 16 || len(B) != 16 {
		panic("Doubling approach won't work")
	}
	A2i := new(big.Int)
	A2i.SetBytes(A)
	A2i.Lsh(A2i, 1)
	// log.Printf("A=%v, A2i = %v\n", A, A2i)
	A2 := make([]byte, 16)
	copy(A2, A2i.Bytes()[max(len(A2i.Bytes())-16, 0):])

	B4i := new(big.Int)
	B4i.SetBytes(B)
	B4i.Lsh(B4i, 2)
	B4 := make([]byte, 16)
	copy(B4, B4i.Bytes()[max(len(B4i.Bytes())-16, 0):])

	K := XorKey(A2, B4)
	K = XorKey(K, T)

	ciphertext := make([]byte, aes.BlockSize)
	aesprf.Encrypt(ciphertext, K)

	rho := XorKey(ciphertext, K)
	return XorKey(rho, X)
}

func GaXDKC_D(A, B, T, P Key) Key {
	if len(A) != 16 || len(B) != 16 {
		panic("Doubling approach won't work")
	}
	A2i := new(big.Int)
	A2i.SetBytes(A)
	A2i.Lsh(A2i, 1)
	A2 := make([]byte, 16)
	copy(A2, A2i.Bytes()[max(len(A2i.Bytes())-16, 0):])

	B4i := new(big.Int)
	B4i.SetBytes(B)
	B4i.Lsh(B4i, 2)
	B4 := make([]byte, 16)
	copy(B4, B4i.Bytes()[max(len(B4i.Bytes())-16, 0):])

	K := XorKey(A2, B4)
	K = XorKey(K, T)

	ciphertext := make([]byte, aes.BlockSize)
	aesprf.Encrypt(ciphertext, K)

	rho := XorKey(ciphertext, K)
	return XorKey(rho, P)
}

func max(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}
