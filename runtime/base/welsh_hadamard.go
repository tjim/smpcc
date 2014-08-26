package main

import (
	"fmt"
	"math"
)

func inner_product(x, y []byte) (res byte) {
	if len(x) != len(y) {
		panic("Inputs must have the same length")
	}

	intermediate := byte(0)

	for i, _ := range x {
		intermediate ^= x[i] & y[i]
	}

	res = 0
	for i := uint8(0); i < 8; i++ {
		res ^= (intermediate >> i) & 0x1
	}
	return
}

func WH(alpha []byte) []byte {
	result := make([]byte, int(math.Pow(255, len(alpha))))
	x := make([]byte, len(alpha))
	for i := 0; x[len(x)-1] != 0xff; {
		result
		if x[i] == 0xff {
			i++
		} else {
			x[i]++
		}
	}

	return result
}

func main() {
	x := []byte{1, 3, 7}
	fmt.Printf("%v\n", inner_product(x, x))
}
