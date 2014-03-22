package bit_test

import (
	"bit"
	"testing"
)

func Test(t *testing.T) {
	n := 300
	m := 200
	A := bit.NewMatrix8(8*n, 8*m)
	for i := 0; i < A.NumRows; i = i + 2 {
		for j := 0; j < m; j++ {
			A.Data[i*A.NumCols/8+j] = 0xff
		}
	}
	B := A.Transpose()
	C := B.Transpose()
	D := C.Transpose()
	if !A.Equal(C) {
		t.Errorf("Incorrect: A != C")
	}
	if !B.Equal(D) {
		t.Errorf("Incorrect: B != D")
	}
	E := bit.NewMatrix8(8, 8)
	E.Randomize()
	G := TransposeLeDumb(E)
	F := E.Transpose()
	if !F.Equal(G) {
		t.Errorf("Incorrect: F != G")
	}

}

func TransposeLeDumb(self *bit.Matrix8) *bit.Matrix8 {
	res := bit.NewMatrix8(self.NumCols, self.NumRows)

	for i := 0; i < self.NumRows; i++ {
		for j := 0; j < self.NumCols; j++ {
			res.Set(j, i, self.Get(i, j))
		}
	}
	return res
}

func Test2(t *testing.T) {
	n := 3000
	m := 2000
	A := bit.NewMatrix8(8*n, 8*m)
	B := TransposeLeDumb(A)
	if B.NumRows < 0 {
		panic("Nonsense")
	}
}

func Test3(t *testing.T) {
	n := 3000
	m := 2000
	A := bit.NewMatrix8(8*n, 8*m)
	B := A.Transpose()
	if B.NumRows < 0 {
		panic("Nonsense")
	}
}
