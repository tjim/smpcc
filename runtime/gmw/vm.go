package gmw

import "fmt"
import "math/big"
import "crypto/rand"

var log_results bool = false
var log_communication bool = false

type Io interface {
	Id() int
	GetInput() uint32

	Open1(bool) bool
	Open8(uint8) uint8
	Open32(uint32) uint32
	Open64(uint64) uint64

	Broadcast1(bool)
	Broadcast8(uint8)
	Broadcast32(uint32)
	Broadcast64(uint64)

	Receive1(party int) bool
	Receive8(party int) uint8
	Receive32(party int) uint32
	Receive64(party int) uint64

	Triple1() (a, b, c bool)
	Triple8() (a, b, c uint8)
	Triple32() (a, b, c uint32)
	Triple64() (a, b, c uint64)

	InitRam([]byte)
	Ram() []byte
}

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
	return Not1(io, treeor(bitwise_inequality, uint64(32)))
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
	c := a0 && b0 /* carry bit */
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
	c := a0 && b0 /* carry bit */
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
	c := a0 && b0 /* carry bit */
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

func Int8(io Io, a int8) uint8 {
	return Uint8(io, uint8(a))
}

func Int32(io Io, a int32) uint32 {
	return Uint32(io, uint32(a))
}

func Int64(io Io, a int64) uint64 {
	return Uint64(io, uint64(a))
}

func Select8(io Io, s bool, a, b uint8) uint8 {
	var result uint8 = 0
	for i := uint8(1); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		if xor(bi, And1(io, s, xor(ai, bi))) {
			result |= i
		}
	}
	return result
}

func Select32(io Io, s bool, a, b uint32) uint32 {
	var result uint32 = 0
	for i := uint32(1); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		if xor(bi, And1(io, s, xor(ai, bi))) {
			result |= i
		}
	}
	return result
}

func Select64(io Io, s bool, a, b uint64) uint64 {
	var result uint64 = 0
	for i := uint64(1); i > 0; i = i * 2 {
		ai := (a & i) > 0
		bi := (b & i) > 0
		if xor(bi, And1(io, s, xor(ai, bi))) {
			result |= i
		}
	}
	return result
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

func Mask8(io Io, s bool, a uint8) uint8 {
	return Select8(io, s, a, Uint8(io, 0))
}

func Mask32(io Io, s bool, a uint32) uint32 {
	return Select32(io, s, a, Uint32(io, 0))
}

func Mask64(io Io, s bool, a uint64) uint64 {
	return Select64(io, s, a, Uint64(io, 0))
}

func Input32(io Io, party int) uint32 {
	id := io.Id()
	if id == party {
		X := io.GetInput()
		return X
	} else {
		return 0
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

/* This (temporary) implementation reveals the memory access pattern but not memory contents */
func Load(io Io, loc uint64, eltsize uint32) uint64 {
	address := int(Reveal64(io, loc))
	bytes := int(Reveal32(io, eltsize))
	if io.Id() == 0 {
		fmt.Printf("Loading Ram[0x%08x]<%d>\n", address, bytes)
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
	return x
}

func Store(io Io, loc uint64, eltsize uint32, x uint32) {
	address := int(Reveal64(io, loc))
	bytes := int(Reveal32(io, eltsize))
	if io.Id() == 0 {
		fmt.Printf("Storing Ram[0x%08x]<%d>\n", address, bytes)
	}
	switch bytes {
	default:
		panic(fmt.Sprintf("Store: bad element size %d", bytes))
	case 1, 2, 4, 8:
	}
	ram := io.Ram()
	for j := 0; j < bytes; j++ {
		byte_j := byte(x>>uint(j*8)) & 0xff
		ram[address+j] = byte_j
	}
}

/* mocked-up implementation of Io */
type X struct {
	id        int /* id of party, range is 0..n-1 */
	triples32 []struct{ a, b, c uint32 }
	triples8  []struct{ a, b, c uint8 }
	triples1  []struct{ a, b, c bool }
	rchannels []chan uint32 /* channels for reading from other parties */
	wchannels []chan uint32 /* channels for writing to other parties */
	inputs    []uint32      /* inputs of this party */
	ram       []byte
}

func (x *X) Id() int {
	return x.id
}

func (x *X) Triple1() (a, b, c bool) {
	if len(x.triples1) == 0 {
		a32, b32, c32 := x.Triple32()
		x.triples1 = make([]struct{ a, b, c bool }, 32)
		for i := range x.triples1 {
			ui := uint(i)
			x.triples1[i] = struct{ a, b, c bool }{0 < (1 & (a32 >> ui)), 0 < (1 & (b32 >> ui)), 0 < (1 & (c32 >> ui))}
		}
	}
	result := x.triples1[0]
	x.triples1 = x.triples1[1:]
	return result.a, result.b, result.c
}

func (x *X) Triple8() (a, b, c uint8) {
	if len(x.triples8) == 0 {
		a32, b32, c32 := x.Triple32()
		x.triples8 = []struct{ a, b, c uint8 }{{uint8(a32 >> 0), uint8(b32 >> 0), uint8(c32 >> 0)},
			{uint8(a32 >> 8), uint8(b32 >> 8), uint8(c32 >> 8)},
			{uint8(a32 >> 16), uint8(b32 >> 16), uint8(c32 >> 16)},
			{uint8(a32 >> 24), uint8(b32 >> 24), uint8(c32 >> 24)}}
	}
	result := x.triples8[0]
	x.triples8 = x.triples8[1:]
	return result.a, result.b, result.c
}

func (x *X) Triple32() (a, b, c uint32) {
	result := x.triples32[0]
	x.triples32 = x.triples32[1:]
	return result.a, result.b, result.c
}

func (x *X) Triple64() (a, b, c uint64) {
	a0, b0, c0 := x.Triple32()
	a1, b1, c1 := x.Triple32()
	return (uint64(a0) << 32) | uint64(a1), (uint64(b0) << 32) | uint64(b1), (uint64(c0) << 32) | uint64(c1)
}

func (x *X) Open1(s bool) bool {
	x.Broadcast1(s)
	result := s
	id := x.Id()
	for i := range x.rchannels {
		if i == id {
			continue
		}
		result = xor(result, x.Receive1(i))
	}
	return result
}

func (x *X) Open8(s uint8) uint8 {
	x.Broadcast8(s)
	result := s
	id := x.Id()
	for i := range x.rchannels {
		if i == id {
			continue
		}
		result ^= x.Receive8(i)
	}
	return result
}

func (x *X) Open32(s uint32) uint32 {
	x.Broadcast32(s)
	result := s
	id := x.Id()
	for i := range x.rchannels {
		if i == id {
			continue
		}
		result ^= x.Receive32(i)
	}
	return result
}

func (x *X) Open64(s uint64) uint64 {
	x.Broadcast64(s)
	result := s
	id := x.Id()
	for i := range x.rchannels {
		if i == id {
			continue
		}
		result ^= x.Receive64(i)
	}
	return result
}

func (x *X) Broadcast1(n bool) {
	var n32 uint32 = 0
	if n {
		n32 = 1
	}
	id := x.Id()
	if log_communication {
		fmt.Printf("%d: BROADCAST 0x%1x\n", id, n32)
	}
	for i, ch := range x.wchannels {
		if i == id {
			continue
		}
		go func(ch chan<- uint32, i int) {
			ch <- n32
			if log_communication {
				fmt.Printf("%d -- 0x%1x -> %d\n", id, n32, i)
			}
		}(ch, i)
	}
}

func (x *X) Broadcast8(n uint8) {
	id := x.Id()
	if log_communication {
		fmt.Printf("%d: BROADCAST 0x%02x\n", id, n)
	}
	for i, ch := range x.wchannels {
		if i == id {
			continue
		}
		go func(ch chan<- uint32, i int) {
			ch <- uint32(n)
			if log_communication {
				fmt.Printf("%d -- 0x%02x -> %d\n", id, n, i)
			}
		}(ch, i)
	}
}

func (x *X) Broadcast32(n uint32) {
	id := x.Id()
	if log_communication {
		fmt.Printf("%d: BROADCAST 0x%08x\n", id, n)
	}
	for i, ch := range x.wchannels {
		if i == id {
			continue
		}
		go func(ch chan<- uint32, i int) {
			ch <- n
			if log_communication {
				fmt.Printf("%d -- 0x%08x -> %d\n", id, n, i)
			}
		}(ch, i)
	}
}

func (x *X) Broadcast64(n uint64) {
	n0 := uint32(n >> 32)
	n1 := uint32(n)
	x.Broadcast32(n0)
	x.Broadcast32(n1)
}

func (x *X) Receive1(party int) bool {
	id := x.Id()
	if party == id {
		return false
	}
	ch := x.rchannels[party]
	result := <-ch
	if log_communication {
		fmt.Printf("%d <- 0x%1x -- %d\n", id, result, party)
	}
	return result > 0
}

func (x *X) Receive8(party int) uint8 {
	id := x.Id()
	if party == id {
		return 0
	}
	ch := x.rchannels[party]
	result := <-ch
	if log_communication {
		fmt.Printf("%d <- 0x%02x -- %d\n", id, result, party)
	}
	return uint8(result)
}

func (x *X) Receive32(party int) uint32 {
	id := x.Id()
	if party == id {
		return 0
	}
	ch := x.rchannels[party]
	result := <-ch
	if log_communication {
		fmt.Printf("%d <- 0x%08x -- %d\n", id, result, party)
	}
	return result
}

func (x *X) Receive64(party int) uint64 {
	n0 := x.Receive32(party)
	n1 := x.Receive32(party)
	return (uint64(n0) << 32) | uint64(n1)
}

func (x *X) GetInput() uint32 {
	result := x.inputs[0]
	x.inputs = x.inputs[1:]
	return result
}

func (x *X) InitRam(contents []byte) {
	x.ram = contents
}

func (x *X) Ram() []byte {
	return x.ram
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
	var y uint32 = 0
	for _, s := range result {
		y ^= s
	}
	if y != x0 {
		panic("NOT EQUAL")
	}
	return result
}

/* create n shares of a multiplication triple */
func triple32(n int) []struct{ a, b, c uint32 } {
	a, b := rand32(), rand32()
	c := a & b
	a_shares := split_uint32(a, n)
	b_shares := split_uint32(b, n)
	c_shares := split_uint32(c, n)
	result := make([]struct{ a, b, c uint32 }, n)
	for i, _ := range result {
		result[i] = struct{ a, b, c uint32 }{a_shares[i], b_shares[i], c_shares[i]}
	}
	return result
}

func Example(n int) []*X {
	/* triples */
	triples32 := make([][]struct{ a, b, c uint32 }, n)
	num_triples := 8*4096
	for i, _ := range triples32 {
		triples32[i] = make([]struct{ a, b, c uint32 }, num_triples)
	}
	for j := 0; j < num_triples; j++ {
		shares_of_a_triple := triple32(n)
		for i, t := range shares_of_a_triple {
			triples32[i][j] = t
		}
	}

	/* channels */
	rchannels := make([] /*n*/ [] /*n*/ chan uint32, n)
	wchannels := make([] /*n*/ [] /*n*/ chan uint32, n)
	for i := range rchannels {
		rchannels[i] = make([] /*n*/ chan uint32, n)
		wchannels[i] = make([] /*n*/ chan uint32, n)
	}
	for i := range rchannels {
		for j := range rchannels {
			if i == j {
				continue
			}
			ch := make(chan uint32)
			rchannels[i][j] = ch
			wchannels[j][i] = ch
		}
	}

	/* final result */
	result := make([]*X, n)
	for id := range result {
		inputs := make([]uint32, 1)
		inputs[0] = uint32(id)
		result[id] =
			&X{id,
				triples32[id],
				make([]struct{ a, b, c uint8 }, 0),
				make([]struct{ a, b, c bool }, 0),
				rchannels[id],
				wchannels[id],
				inputs,
				nil}
	}
	return result
}

func RunExample() uint32 {
	xs := Example(3)
	done := make(chan uint32, len(xs))
	results := make([]uint32, len(xs))
	for _, x := range xs {
		go func(x *X) {
			done <- Output8(x, Lshr8(x, Uint8(x, 128), 7))
		}(x)
	}
	for i := range xs {
		results[i] = <-done
	}
	result := results[0]
	for i := range results {
		if results[i] != result {
			panic(fmt.Sprintf("Error: results don't match (0x%08x != 0x%08x)", results[i], result))
		}
	}
	fmt.Printf("Result: 0x%08x\n", result)
	return result
}
