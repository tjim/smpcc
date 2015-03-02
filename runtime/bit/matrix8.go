package bit

import (
	"crypto/rand"
	"io"
)

type Matrix8 struct {
	NumRows int
	NumCols int
	Data    []byte
}

func NewMatrix8(rows, cols int) *Matrix8 {
	if rows < 0 || rows%8 != 0 || cols < 0 || cols%8 != 0 {
		panic("matrix8.New: bad dimensions")
	}
	return &Matrix8{rows, cols, make([]byte, rows*cols/8)}
}

func (self *Matrix8) Randomize() {
	n, err := io.ReadFull(rand.Reader, self.Data)
	if err != nil || n != len(self.Data) {
		panic("matrix8.Randomize: randomness allocation failed")
	}
}

func (self *Matrix8) Set(row, col int, v int) {
	if row < 0 || row >= self.NumRows || col < 0 || col >= self.NumCols {
		panic("matrix8.Set: out of bounds")
	}
	byteWidth := (self.NumCols / 8)
	slot := row*byteWidth + col/8
	switch {
	case v == 0:
		self.Data[slot] &= (0xff ^ (1 << (7 - uint(col)%8)))
	case v == 1:
		self.Data[slot] |= (1 << (7 - uint(col)%8))
	default:
		panic("matrix8.Set: not a 0/1 bit")
	}
}

func (self *Matrix8) Get(row, col int) int {
	if row < 0 || row >= self.NumRows || col < 0 || col >= self.NumCols {
		panic("matrix8.Get: out of bounds")
	}
	byteWidth := (self.NumCols / 8)
	slot := row*byteWidth + col/8
	return int((self.Data[slot] >> (7 - uint(col)%8)) & 1)
}

func GetBit(vector []byte, index int) byte {
	return (vector[index/8] >> (7 - uint(index)%8)) & 1
}

func (self *Matrix8) GetRow(row int) []byte {
	if row < 0 || row >= self.NumRows {
		panic("matrix8.GetRow: out of bounds")
	}
	byteWidth := (self.NumCols / 8)
	return self.Data[row*byteWidth : (row+1)*byteWidth]
}

func (self *Matrix8) SetRow(row int, data []byte) {
	if row < 0 || row >= self.NumRows {
		panic("matrix8.SetRow: out of bounds")
	}
	byteWidth := (self.NumCols / 8)
	// if data is short, only those columns will be written
	// if data is long, no more than one row is written
	copy(self.Data[row*byteWidth:(row+1)*byteWidth], data)
}

func transpose8(A, B []byte, m, n int) {
	// Hacker's Delight, Fig. 7.2
	var x, y, t uint32

	x = (uint32(A[0*m]) << 24) | (uint32(A[1*m]) << 16) | (uint32(A[2*m]) << 8) | uint32(A[3*m])
	y = (uint32(A[4*m]) << 24) | (uint32(A[5*m]) << 16) | (uint32(A[6*m]) << 8) | uint32(A[7*m])

	t = (x ^ (x >> 7)) & 0x00AA00AA
	x = x ^ t ^ (t << 7)

	t = (y ^ (y >> 7)) & 0x00AA00AA
	y = y ^ t ^ (t << 7)

	t = (x ^ (x >> 14)) & 0x0000CCCC
	x = x ^ t ^ (t << 14)

	t = (y ^ (y >> 14)) & 0x0000CCCC
	y = y ^ t ^ (t << 14)

	t = (x & 0xF0F0F0F0) | ((y >> 4) & 0x0F0F0F0F)
	y = ((x << 4) & 0xF0F0F0F0) | (y & 0x0F0F0F0F)
	x = t

	B[0*n] = byte(x >> 24)
	B[1*n] = byte(x >> 16)
	B[2*n] = byte(x >> 8)
	B[3*n] = byte(x >> 0)
	B[4*n] = byte(y >> 24)
	B[5*n] = byte(y >> 16)
	B[6*n] = byte(y >> 8)
	B[7*n] = byte(y >> 0)
}

func (self Matrix8) Transpose() *Matrix8 {
	rslt := NewMatrix8(self.NumCols, self.NumRows)
	for j := 0; j < len(rslt.Data); j++ {
		rslt.Data[j] = byte(j)
	}
	n := self.NumRows / 8 // result matrix width in bytes
	m := self.NumCols / 8 // source matrix width in bytes
	for i := 0; i < m*n; i++ {
		aRow := i / m
		aCol := i % m
		A := self.Data[8*aRow*m+aCol:]
		B := rslt.Data[8*aCol*n+aRow:]
		transpose8(A, B, m, n)
	}
	return rslt
}

func (self Matrix8) Equal(other *Matrix8) bool {
	if self.NumRows != other.NumRows || self.NumCols != other.NumCols {
		return false
	}
	for i := 0; i < len(self.Data); i++ {
		if self.Data[i] != other.Data[i] {
			return false
		}
	}
	return true
}

func (self Matrix8) XorFrom(other *Matrix8) {
	if self.NumRows != other.NumRows || self.NumCols != other.NumCols {
		panic("Xor")
	}
	for i, v := range other.Data {
		self.Data[i] ^= v
	}
}
