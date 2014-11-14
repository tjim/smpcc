package gmw

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func xor(x, y bool) bool {
	return (x && !y) || (y && !x)
}

func Xor1(io Io, x, y bool) bool {
	return xor(x, y)
}

func Xor8(io Io, x, y uint8) uint8 {
	return x ^ y
}

func Xor32(io Io, x, y uint32) uint32 {
	return x ^ y
}

func Xor64(io Io, x, y uint64) uint64 {
	return x ^ y
}

func And1(io Io, x, y bool) bool {
	a, b, c := io.Triple1()
	d := io.Open1(xor(x, a))
	e := io.Open1(xor(y, b))
	if io.Id() == 0 {
		return xor(c, xor(d && b, xor(e && a, d && e)))
	} else {
		return xor(c, xor(d && b, e && a))
	}
}

func And8(io Io, x, y uint8) uint8 {
	a, b, c := io.Triple8()
	d := io.Open8(x ^ a)
	e := io.Open8(y ^ b)
	if io.Id() == 0 {
		return c ^ d&b ^ e&a ^ d&e
	} else {
		return c ^ d&b ^ e&a
	}
}

func And32(io Io, x, y uint32) uint32 {
	a, b, c := io.Triple32()
	d := io.Open32(x ^ a)
	e := io.Open32(y ^ b)
	if io.Id() == 0 {
		return c ^ d&b ^ e&a ^ d&e
	} else {
		return c ^ d&b ^ e&a
	}
}

func And64(io Io, x, y uint64) uint64 {
	a, b, c := io.Triple64()
	d := io.Open64(x ^ a)
	e := io.Open64(y ^ b)
	if io.Id() == 0 {
		return c ^ d&b ^ e&a ^ d&e
	} else {
		return c ^ d&b ^ e&a
	}
}

func Or1(io Io, a, b bool) bool {
	return Not1(io, And1(io, Not1(io, a), Not1(io, b)))
}

func Or8(io Io, a, b uint8) uint8 {
	return Not8(io, And8(io, Not8(io, a), Not8(io, b)))
}

func Or32(io Io, a, b uint32) uint32 {
	return Not32(io, And32(io, Not32(io, a), Not32(io, b)))
}

func Or64(io Io, a, b uint64) uint64 {
	return Not64(io, And64(io, Not64(io, a), Not64(io, b)))
}

func Not1(io Io, a bool) bool {
	if io.Id() == 0 {
		return !a
	}
	return a
}

func Not8(io Io, a uint8) uint8 {
	if io.Id() == 0 {
		return 0xff ^ a
	}
	return a
}

func Not32(io Io, a uint32) uint32 {
	if io.Id() == 0 {
		return 0xffffffff ^ a
	}
	return a
}

func Not64(io Io, a uint64) uint64 {
	if io.Id() == 0 {
		return 0xffffffffffffffff ^ a
	}
	return a
}

func Icmp_eq8(io Io, a, b uint8) bool {
	bitwise_inequality := Xor8(io, a, b)
	var treeor func(x, n uint8) bool
	treeor = func(x, n uint8) bool {
		// compute OR on rightmost n bits of x
		// invariant: bits to left of rightmost n bits are 0
		if n == 1 {
			return x > 0
		}
		half := n / 2
		left := x >> half
		right := x & ((1 << half) - 1)
		return Or1(io, treeor(left, half), treeor(right, half))
	}
	return Not1(io, treeor(bitwise_inequality, uint8(8)))
}

func Icmp_eq32(io Io, a, b uint32) bool {
	bitwise_inequality := Xor32(io, a, b)
	var treeor func(x, n uint32) bool
	treeor = func(x, n uint32) bool {
		// compute OR on rightmost n bits of x
		// invariant: bits to left of rightmost n bits are 0
		if n == 1 {
			return x > 0
		}
		half := n / 2
		left := x >> half
		right := x & ((1 << half) - 1)
		return Or1(io, treeor(left, half), treeor(right, half))
	}
	return Not1(io, treeor(bitwise_inequality, uint32(32)))
}

func Icmp_eq64(io Io, a, b uint64) bool {
	bitwise_inequality := Xor64(io, a, b)
	var treeor func(x, n uint64) bool
	treeor = func(x, n uint64) bool {
		// compute OR on rightmost n bits of x
		// invariant: bits to left of rightmost n bits are 0
		if n == 1 {
			return x > 0
		}
		half := n / 2
		left := x >> half
		right := x & ((1 << half) - 1)
		return Or1(io, treeor(left, half), treeor(right, half))
	}
	return Not1(io, treeor(bitwise_inequality, uint64(64)))
}

func Icmp_ugt8(io Io, a, b uint8) bool {
	c := false
	for i := uint8(1); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		c = xor(ai, And1(io, xor(ai, c), xor(bi, c)))
	}
	return c
}

func Icmp_ugt32(io Io, a, b uint32) bool {
	c := false
	for i := uint32(1); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		c = xor(ai, And1(io, xor(ai, c), xor(bi, c)))
	}
	return c
}

func Icmp_ugt64(io Io, a, b uint64) bool {
	c := false
	for i := uint64(1); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		c = xor(ai, And1(io, xor(ai, c), xor(bi, c)))
	}
	return c
}

func Icmp_ult8(io Io, a, b uint8) bool {
	return Icmp_ugt8(io, b, a)
}

func Icmp_ult32(io Io, a, b uint32) bool {
	return Icmp_ugt32(io, b, a)
}

func Icmp_ult64(io Io, a, b uint64) bool {
	return Icmp_ugt64(io, b, a)
}

func Icmp_sgt8(io Io, a, b uint8) bool {
	highbit := uint8(1 << 7)
	a_high, a_rest := (a&highbit) > 0, a^highbit
	b_high, b_rest := (b&highbit) > 0, b^highbit
	return Or1(io, And1(io, Not1(io, a_high), b_high), // a_high = 0, b_high = 1
		And1(io, Not1(io, Xor1(io, a_high, b_high)), // a_high and b_high are the same
			Icmp_ugt8(io, a_rest, b_rest))) // a_rest > b_rest (unsigned)
}

func Icmp_sgt32(io Io, a, b uint32) bool {
	highbit := uint32(1 << 31)
	a_high, a_rest := (a&highbit) > 0, a^highbit
	b_high, b_rest := (b&highbit) > 0, b^highbit
	return Or1(io, And1(io, Not1(io, a_high), b_high), // a_high = 0, b_high = 1
		And1(io, Not1(io, Xor1(io, a_high, b_high)), // a_high and b_high are the same
			Icmp_ugt32(io, a_rest, b_rest))) // a_rest > b_rest (unsigned)
}

func Icmp_sgt64(io Io, a, b uint64) bool {
	highbit := uint64(1 << 63)
	a_high, a_rest := (a&highbit) > 0, a^highbit
	b_high, b_rest := (b&highbit) > 0, b^highbit
	return Or1(io, And1(io, Not1(io, a_high), b_high), // a_high = 0, b_high = 1
		And1(io, Not1(io, Xor1(io, a_high, b_high)), // a_high and b_high are the same
			Icmp_ugt64(io, a_rest, b_rest))) // a_rest > b_rest (unsigned)
}

func Icmp_slt8(io Io, a, b uint8) bool {
	return Icmp_sgt8(io, b, a)
}

func Icmp_slt32(io Io, a, b uint32) bool {
	return Icmp_sgt32(io, b, a)
}

func Icmp_slt64(io Io, a, b uint64) bool {
	return Icmp_sgt64(io, b, a)
}

func Icmp_uge8(io Io, a, b uint8) bool {
	c := true
	for i := uint8(1); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		c = xor(ai, And1(io, xor(ai, c), xor(bi, c)))
	}
	return c
}

func Icmp_uge32(io Io, a, b uint32) bool {
	c := true
	for i := uint32(1); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		c = xor(ai, And1(io, xor(ai, c), xor(bi, c)))
	}
	return c
}

func Icmp_uge64(io Io, a, b uint64) bool {
	c := true
	for i := uint64(1); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		c = xor(ai, And1(io, xor(ai, c), xor(bi, c)))
	}
	return c
}

func Icmp_ule8(io Io, a, b uint8) bool {
	return Icmp_uge8(io, b, a)
}

func Icmp_ule32(io Io, a, b uint32) bool {
	return Icmp_uge32(io, b, a)
}

func Icmp_ule64(io Io, a, b uint64) bool {
	return Icmp_uge64(io, b, a)
}

func Add8(io Io, a, b uint8) uint8 {
	var result uint8 = 0
	var a0, b0 bool = (a & 1) > 0, (b & 1) > 0
	if xor(a0, b0) {
		result |= 1
	}
	c := And1(io, a0, b0) /* carry bit */
	for i := uint8(2); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		/* compute the result bit */
		bi_xor_c := xor(bi, c)
		if xor(ai, bi_xor_c) {
			result |= i
		}
		/* compute the carry bit. */
		c = xor(c, And1(io, xor(ai, c), bi_xor_c))
	}
	return result
}

func Add32(io Io, a, b uint32) uint32 {
	var result uint32 = 0
	var a0, b0 bool = (a & 1) > 0, (b & 1) > 0
	if xor(a0, b0) {
		result |= 1
	}
	c := And1(io, a0, b0) /* carry bit */
	for i := uint32(2); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		/* compute the result bit */
		bi_xor_c := xor(bi, c)
		if xor(ai, bi_xor_c) {
			result |= i
		}
		/* compute the carry bit. */
		c = xor(c, And1(io, xor(ai, c), bi_xor_c))
	}
	return result
}

func Add64(io Io, a, b uint64) uint64 {
	var result uint64 = 0
	var a0, b0 bool = (a & 1) > 0, (b & 1) > 0
	if xor(a0, b0) {
		result |= 1
	}
	c := And1(io, a0, b0) /* carry bit */
	for i := uint64(2); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		/* compute the result bit */
		bi_xor_c := xor(bi, c)
		if xor(ai, bi_xor_c) {
			result |= i
		}
		/* compute the carry bit. */
		c = xor(c, And1(io, xor(ai, c), bi_xor_c))
	}
	return result
}

func Sub8(io Io, a, b uint8) uint8 {
	var result uint8 = 0
	var a0, b0 bool = (a & 1) > 0, (b & 1) > 0
	if xor(a0, b0) {
		result |= 1
	}
	c := xor(a0, And1(io, Not1(io, a0), Not1(io, b0))) /* carry bit */
	for i := uint8(2); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		/* compute the result bit */
		bi_xor_c := xor(bi, c)
		if Not1(io, xor(ai, bi_xor_c)) {
			result |= i
		}
		/* compute the carry bit. */
		c = xor(ai, And1(io, xor(ai, c), bi_xor_c))
	}
	return result
}

func Sub32(io Io, a, b uint32) uint32 {
	var result uint32 = 0
	var a0, b0 bool = (a & 1) > 0, (b & 1) > 0
	if xor(a0, b0) {
		result |= 1
	}
	c := xor(a0, And1(io, Not1(io, a0), Not1(io, b0))) /* carry bit */
	for i := uint32(2); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		/* compute the result bit */
		bi_xor_c := xor(bi, c)
		if Not1(io, xor(ai, bi_xor_c)) {
			result |= i
		}
		/* compute the carry bit. */
		c = xor(ai, And1(io, xor(ai, c), bi_xor_c))
	}
	return result
}

func Sub64(io Io, a, b uint64) uint64 {
	var result uint64 = 0
	var a0, b0 bool = (a & 1) > 0, (b & 1) > 0
	if xor(a0, b0) {
		result |= 1
	}
	c := xor(a0, And1(io, Not1(io, a0), Not1(io, b0))) /* carry bit */
	for i := uint64(2); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		/* compute the result bit */
		bi_xor_c := xor(bi, c)
		if Not1(io, xor(ai, bi_xor_c)) {
			result |= i
		}
		/* compute the carry bit. */
		c = xor(ai, And1(io, xor(ai, c), bi_xor_c))
	}
	return result
}

func Uint1(io Io, a uint8) bool {
	/* Alternately, party 0 could distribute random shares */
	if io.Id() == 0 {
		return a > 0
	}
	return false
}

func Uint8(io Io, a uint8) uint8 {
	/* Alternately, party 0 could distribute random shares */
	if io.Id() == 0 {
		return a
	}
	return 0
}

func Uint32(io Io, a uint32) uint32 {
	/* Alternately, party 0 could distribute random shares */
	if io.Id() == 0 {
		return a
	}
	return 0
}

func Uint64(io Io, a uint64) uint64 {
	/* Alternately, party 0 could distribute random shares */
	if io.Id() == 0 {
		return a
	}
	return 0
}

func Select1(io Io, s bool, a, b bool) bool {
	return xor(b, And1(io, s, xor(a, b)))
}

func Select8(io Io, s bool, a, b uint8) uint8 {
	return b ^ Mask8(io, s, a^b)
}

func Select32(io Io, s bool, a, b uint32) uint32 {
	return b ^ Mask32(io, s, a^b)
}

func Select64(io Io, s bool, a, b uint64) uint64 {
	return b ^ Mask64(io, s, a^b)
}

func Mul8(io Io, a, b uint8) uint8 {
	zeros := Uint8(io, 0)
	b0 := (b & 1) > 0
	result := Select8(io, b0, a, zeros)
	for i := uint8(2); i > 0; i = i * 2 {
		bi := (b & i) > 0
		a = a << 1
		sum := Add8(io, result, a)
		result = Select8(io, bi, sum, result)
	}
	return result
}

func Mul32(io Io, a, b uint32) uint32 {
	zeros := Uint32(io, 0)
	b0 := (b & 1) > 0
	result := Select32(io, b0, a, zeros)
	for i := uint32(2); i > 0; i = i * 2 {
		bi := (b & i) > 0
		a = a << 1
		sum := Add32(io, result, a)
		result = Select32(io, bi, sum, result)
	}
	return result
}

func Mul64(io Io, a, b uint64) uint64 {
	zeros := Uint64(io, 0)
	b0 := (b & 1) > 0
	result := Select64(io, b0, a, zeros)
	for i := uint64(2); i > 0; i = i * 2 {
		bi := (b & i) > 0
		a = a << 1
		sum := Add64(io, result, a)
		result = Select64(io, bi, sum, result)
	}
	return result
}

/* constant shift left; TODO: variable Shl */
func Shl8(io Io, a uint8, b uint) uint8 {
	return a << b
}

func Shl32(io Io, a uint32, b uint) uint32 {
	return a << b
}

func Shl64(io Io, a uint64, b uint) uint64 {
	return a << b
}

/* constant logical shift right; TODO: variable Lshr */
func Lshr8(io Io, a uint8, b uint) uint8 {
	return a >> b
}

func Lshr32(io Io, a uint32, b uint) uint32 {
	return a >> b
}

func Lshr64(io Io, a uint64, b uint) uint64 {
	return a >> b
}

/* constant arithmetic shift right; TODO: variable Ashr */
func Ashr8(io Io, a uint8, b uint) uint8 {
	return uint8(int8(a) >> b)
}

func Ashr32(io Io, a uint32, b uint) uint32 {
	return uint32(int32(a) >> b)
}

func Ashr64(io Io, a uint64, b uint) uint64 {
	return uint64(int64(a) >> b)
}

func Mask1(io Io, s bool, a bool) bool {
	a32 := uint32(0)
	if a {
		a32 = uint32(1)
	}
	if (Mask32(io, s, a32) & 1) == 0 {
		return false
	} else {
		return true
	}
}

func Mask8(io Io, s bool, a uint8) uint8 {
	return uint8(Mask32(io, s, uint32(a)))
}

func Mask32(io Io, s bool, Y uint32) uint32 {
	x := byte(0)
	if s {
		x = byte(1)
	}
	// x is 0 or 1
	a, B, C := io.MaskTriple32()
	// a is 0 or 1
	d := io.Open8(x ^ a)
	// d is 0 or 1
	A := uint32(0)
	if a != 0 {
		A = 0xffffffff
	}
	D := uint32(0)
	if d != 0 {
		D = 0xffffffff
	}
	E := io.Open32(Y ^ B)
	if io.Id() == 0 {
		return C ^ D&B ^ E&A ^ D&E
	} else {
		return C ^ D&B ^ E&A
	}
}

func Mask64(io Io, s bool, a uint64) uint64 {
	low := uint32(a)
	high := uint32(a >> 32)
	mlow := Mask32(io, s, low)
	mhigh := Mask32(io, s, high)
	return uint64(mlow) | (uint64(mhigh) << 32)
}

func NumPeers32(io Io) uint32 {
	return Uint32(io, uint32(io.N()))
}

func rand32() uint32 {
	max := big.NewInt(1 << 32)
	x, err := rand.Int(rand.Reader, max)
	if err != nil {
		panic("Error: random number generation")
	}
	return uint32(x.Int64())
}

/* return a slice of n random uint32 values that ^ to x */
func split_uint32(x uint32, n int) []uint32 {
	x0 := x
	if n <= 0 {
		panic("Error: split")
	}
	result := make([]uint32, n)
	for i := 1; i < n; i++ {
		xi := rand32()
		x ^= xi
		result[i] = xi
	}
	result[0] = x
	if check_split {
		var y uint32 = 0
		for _, s := range result {
			y ^= s
		}
		if y != x0 {
			panic("NOT EQUAL")
		}
	}
	return result
}

func Input32(io Io, mask bool, party uint32) uint32 {
	if !io.Open1(mask) {
		return 0
	}
	id := io.Id()
	party = io.Open32(party)
	if id == int(party) {
		X := io.GetInput()
		shares := split_uint32(X, io.N())
		for i := range shares {
			if i == id {
				continue
			}
			io.Send32(i, shares[i])
		}
		return shares[id]
	} else {
		X := io.Receive32(int(party))
		return X
	}
}

func Output1(io Io, x bool) uint32 {
	var result uint32 = 0
	if io.Open1(x) {
		result = 1
	}
	if log_results {
		fmt.Printf("%d: RESULT 0x%1x\n", io.Id(), result)
	}
	return result
}

func Output8(io Io, x uint8) uint32 {
	result := uint32(io.Open8(x))
	if log_results {
		fmt.Printf("%d: RESULT 0x%02x\n", io.Id(), result)
	}
	return result
}

func Output32(io Io, x uint32) uint32 {
	result := io.Open32(x)
	if log_results {
		fmt.Printf("%d: RESULT 0x%08x\n", io.Id(), result)
	}
	return result
}

func TreeXor1(io Io, x ...bool) bool {
	switch len(x) {
	case 0:
		panic("TreeXor with no arguments")
	case 1:
		return x[0]
	case 2:
		return Xor1(io, x[0], x[1])
	default:
		mid := len(x) / 2
		return Xor1(io, TreeXor1(io, x[:mid]...), TreeXor1(io, x[mid:]...))
	}
	panic("unreachable")
}

func TreeXor8(io Io, x ...uint8) uint8 {
	switch len(x) {
	case 0:
		panic("TreeXor with no arguments")
	case 1:
		return x[0]
	case 2:
		return Xor8(io, x[0], x[1])
	default:
		mid := len(x) / 2
		return Xor8(io, TreeXor8(io, x[:mid]...), TreeXor8(io, x[mid:]...))
	}
	panic("unreachable")
}

func TreeXor32(io Io, x ...uint32) uint32 {
	switch len(x) {
	case 0:
		panic("TreeXor with no arguments")
	case 1:
		return x[0]
	case 2:
		return Xor32(io, x[0], x[1])
	default:
		mid := len(x) / 2
		return Xor32(io, TreeXor32(io, x[:mid]...), TreeXor32(io, x[mid:]...))
	}
	panic("unreachable")
}

func TreeXor64(io Io, x ...uint64) uint64 {
	switch len(x) {
	case 0:
		panic("TreeXor with no arguments")
	case 1:
		return x[0]
	case 2:
		return Xor64(io, x[0], x[1])
	default:
		mid := len(x) / 2
		return Xor64(io, TreeXor64(io, x[:mid]...), TreeXor64(io, x[mid:]...))
	}
	panic("unreachable")
}

// NB Printf() reveals the active block as well as the values of its arguments
func Printf(io Io, mask bool, f string, args ...uint64) {
	if !io.Open1(mask) {
		return
	}
	fargs := make([]interface{}, len(args))
	for i := 0; i < len(args); i++ {
		fargs[i] = io.Open64(args[i])
	}
	if io.Id() == 0 {
		fmt.Printf(f, fargs...)
	}
}

func Printf32(io Io, mask bool, f string, args ...uint32) {
	if !io.Open1(mask) {
		return
	}
	fargs := make([]interface{}, len(args))
	for i := 0; i < len(args); i++ {
		fargs[i] = io.Open32(args[i])
	}
	if io.Id() == 0 {
		fmt.Printf(f, fargs...)
	}
}

func Zext8_32(io Io, a uint8) uint32 {
	return uint32(a)
}

func Zext32_64(io Io, a uint32) uint64 {
	return uint64(a)
}

// Switch(io, s, dflt, c0, c1, ...) tests s.
// If s == 0 it returns c0, if s == 1 it returns c1, etc.
// If s is not the index of any c, it returns dflt.
func Switch32(io Io, s, dflt uint32, cases ...uint32) uint32 {
	if len(cases) == 0 {
		return dflt
	}
	masks := make([]bool, len(cases))
	masked_cases := make([]uint32, len(cases))
	for i := 0; i < len(cases); i++ {
		masks[i] = Icmp_eq32(io, s, Uint32(io, uint32(i)))
		masked_cases[i] = Mask32(io, masks[i], cases[i])
	}
	m := TreeXor1(io, masks...)
	x := TreeXor32(io, masked_cases...)
	return Select32(io, m, x, dflt)
}

func Reveal1(io Io, a bool) bool {
	return io.Open1(a)
}

func Reveal8(io Io, a uint8) uint8 {
	return io.Open8(a)
}

func Reveal32(io Io, a uint32) uint32 {
	return io.Open32(a)
}

func Reveal64(io Io, a uint64) uint64 {
	return io.Open64(a)
}

// Convert a binary value to a unary value.
// (Can be used to efficiently compute MUX)
// Let A be a []bool with LSB in position 0
// So if A = []{false, true, true} then A represents 6
// Output should be an array B where B[i] iff A represents i
//
// Label a binary tree with root as 1
// Next level 2, 3
// Next level 4, 5, 6, 7
//
// So to move down left, multiply by two
// So to move down right, multiply by two and add 1
// So to move up, divide by two
//
// Say the root is at level 0
// Children of root at level 1
// etc.
//
// Leftmost node at level x is therefore 2^x
// Level of a node labeled x is log_2(x)
//
// Suppose we have N input bits A[0],A[1],..,A[N-1]
// Define a tree with 2^N leaves, N levels
// Label edges of tree as 0 if child is on left, 1 if on right
// Define a boolean value phi[x] for each node x in the tree
// phi(x) iff path from x to root has labels according to ...,A[N-2],A[N-1]
// i.e., as many high-order bits of A as necessary.
// Then the output of phi(x) of the leaves, left to right, gives the value of A in unary.
func Unary(io Io, A []bool) []bool {
	phi := make([]bool, 2*(1<<uint(len(A))))
	phi[1] = Uint1(io, uint8(1))
	for i := range A {
		leftmost := (1 << uint(i)) * 2
		for j := leftmost; j < leftmost*2; j++ {
			switch j % 2 {
			case 0:
				phi[j] = And1(io, phi[j/2], Not1(io, A[len(A)-i-1]))
			case 1:
				phi[j] = And1(io, phi[j/2], A[len(A)-i-1])
			}
		}
	}
	return phi[len(phi)/2:]
}

/* This (temporary) implementation reveals the memory access pattern but not memory contents */
func Load(io Io, loc uint64, eltsize uint32) uint64 {
	address := int(Reveal64(io, loc))
	bytes := int(Reveal32(io, eltsize))
	if log_mem && io.Id() == 0 {
		fmt.Printf("Loading Ram[0x%08x]<%d>", address, bytes)
	}
	switch bytes {
	default:
		panic(fmt.Sprintf("Load: bad element size %d", bytes))
	case 1, 2, 4, 8:
	}
	x := uint64(0)
	ram := io.Ram()
	for j := 0; j < bytes; j++ {
		byte_j := uint64(ram[address+j])
		x += byte_j << uint(j*8)
	}
	if log_mem {
		y := Reveal64(io, x)
		if io.Id() == 0 {
			fmt.Printf(" = 0x%x\n", y)
		}
	} else {
		fmt.Printf("\n")
	}
	return x
}

func Store(io Io, loc uint64, eltsize uint32, x uint32) {
	address := int(Reveal64(io, loc))
	bytes := int(Reveal32(io, eltsize))
	if io.Id() == 0 {
		fmt.Printf("Storing Ram[0x%08x]<%d>", address, bytes)
	}
	switch bytes {
	default:
		panic(fmt.Sprintf("Store: bad element size %d", bytes))
	case 1, 2, 4, 8:
	}
	ram := io.Ram()
	if log_mem {
		y := Reveal32(io, x)
		if io.Id() == 0 {
			fmt.Printf(" = 0x%x\n", y)
		}
	} else {
		fmt.Printf("\n")
	}
	for j := 0; j < bytes; j++ {
		byte_j := byte(x>>uint(j*8)) & 0xff
		ram[address+j] = byte_j
	}
}
