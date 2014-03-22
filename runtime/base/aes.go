package base

import (
	"crypto/aes"
)

func AESEval(key []byte, input []byte) []byte {
	aesprf, err := aes.NewCipher(key)
	if err != nil {
		panic(err)
	}
	ciphertext := make([]byte, aes.BlockSize)
	aesprf.Encrypt(ciphertext, input)
	return ciphertext
}

func AESInvert(key []byte, input []byte) []byte {
	aesprf, err := aes.NewCipher(key)
	if err != nil {
		panic(err)
	}
	plaintext := make([]byte, aes.BlockSize)
	aesprf.Decrypt(plaintext, input)
	return plaintext
}
