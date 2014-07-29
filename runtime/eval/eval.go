package eval

import "github.com/tjim/smpcc/runtime/base"
import "fmt"

type EvalVM interface {
	And(a, b []base.Key) []base.Key
	Or(a, b []base.Key) []base.Key
	Xor(a, b []base.Key) []base.Key
	True() []base.Key
	False() []base.Key
	RevealTo0(a []base.Key)
	RevealTo1(a []base.Key) []bool
	ShareTo0(v uint64, bits int) []base.Key
	ShareTo1(bits int) []base.Key
	Random(bits int) []base.Key
}

func Mul(io EvalVM, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Mul()")
	}
	if len(a) == 0 {
		panic("empty arguments in Mul()")
	}
	zeros := Uint(io, 0, len(a))
	result := Select(io, b[0:1], a, zeros)
	for i := 1; i < len(b); i++ {
		a_shifted := Uint(io, 0, len(a))
		for j := 0; i+j < len(a); j++ {
			a_shifted[j+i] = a[j]
		}
		sum := Add(io, result, a_shifted)
		result = Select(io, b[i:i+1], sum, result)
	}
	return result
}

func Add(io EvalVM, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic(fmt.Sprintf("Key mismatch in eval.Add(), %d vs %d", len(a), len(b)))
	}
	if len(a) == 0 {
		panic("empty arguments in eval.Add()")
	}
	result := make([]base.Key, len(a))
	result[0] = Xor(io, a[0:1], b[0:1])[0]
	c := And(io, a[0:1], b[0:1])[0:1] /* carry bit */
	for i := 1; i < len(a); i++ {
		ai := a[i : i+1]
		bi := b[i : i+1]
		/* compute the result bit */
		bi_xor_c := Xor(io, bi, c)
		result[i] = Xor(io, ai, bi_xor_c)[0]
		/* compute the carry bit. */
		c = Xor(io, c, And(io, Xor(io, ai, c), bi_xor_c))
	}
	return result
}

func Sub(io EvalVM, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic(fmt.Sprintf("Wire mismatch in eval.Sub(), %d vs %d", len(a), len(b)))
	}
	if len(a) == 0 {
		panic("empty arguments in eval.Sub()")
	}
	result := make([]base.Key, len(a))
	result[0] = Xor(io, a[0:1], b[0:1])[0]
	c := Xor(io, a[0:1], And(io, Not(io, a[0:1]), Not(io, b[0:1])))[0:1] /* carry bit */
	for i := 1; i < len(a); i++ {
		ai := a[i : i+1]
		bi := b[i : i+1]
		/* compute the result bit */
		bi_xor_c := Xor(io, bi, c)
		result[i] = Not(io, Xor(io, ai, bi_xor_c))[0]
		/* compute the carry bit. */
		c = Xor(io, ai, And(io, Xor(io, ai, c), bi_xor_c))
	}
	return result
}

/* constant shift left; TODO: variable shift left */
func Shl(io EvalVM, a []base.Key, b int) []base.Key {
	if len(a) <= b {
		panic("Shl() too far")
	}
	z := False(io)
	zeros := make([]base.Key, b)
	for i := 0; i < len(zeros); i++ {
		zeros[i] = z[0]
	}
	return append(zeros, a...)[:len(a)]
}

/* constant logical shift right; TODO: variable Lshr */
func Lshr(io EvalVM, a []base.Key, b int) []base.Key {
	if len(a) <= b {
		panic("Lshr() too far")
	}
	result := Zext(io, a, b+len(a))
	return result[b:]
}

/* constant logical shift right; TODO: variable Ashr */
func Ashr(io EvalVM, a []base.Key, b int) []base.Key {
	if len(a) <= b {
		panic("Ashr() too far")
	}
	result := Sext(io, a, b+len(a))
	return result[b:]
}

func And(io EvalVM, a, b []base.Key) []base.Key {
	return io.And(a, b)
}

func Or(io EvalVM, a, b []base.Key) []base.Key {
	return io.Or(a, b)
}

func Xor(io EvalVM, a, b []base.Key) []base.Key {
	return io.Xor(a, b)
}

func Trunc(io EvalVM, a []base.Key, b int) []base.Key {
	if len(a) <= b {
		panic("trunc must truncate operand")
	}
	return a[:b]
}

func Zext(io EvalVM, a []base.Key, b int) []base.Key {
	if len(a) >= b {
		panic("zext must extend operand")
	}
	if b == len(a)+1 {
		return append(a, False(io)...)
	}
	return Sext(io, append(a, False(io)...), b)
}

func Sext(io EvalVM, a []base.Key, b int) []base.Key {
	if len(a) >= b {
		panic("sext must extend operand")
	}
	if len(a) == 0 {
		panic("sext on zero-length operand")
	}
	newbits := make([]base.Key, b-len(a))
	for i := 0; i < len(newbits); i++ {
		newbits[i] = a[len(a)-1]
	}
	return append(a, newbits...)
}

func Icmp_eq(io EvalVM, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("Key mismatch in eval.Icmp_eq()")
	}
	if len(a) == 0 {
		panic("empty argument in eval.Icmp_eq()")
	}
	bitwise_inequality := Xor(io, a, b)
	return Not(io, []base.Key{TreeOr0(io, bitwise_inequality...)})
}

func Icmp_ugt(io EvalVM, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in eval.Icmp_ugt()")
	}
	c := False(io)
	for i := 0; i < len(a); i++ {
		ai := a[i : i+1]
		bi := b[i : i+1]
		c = Xor(io, ai, And(io, Xor(io, ai, c), Xor(io, bi, c)))
	}
	return c
}

func Icmp_ult(io EvalVM, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_ult()")
	}
	return Icmp_ugt(io, b, a)
}

func Icmp_sgt(io EvalVM, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_sgt()")
	}
	if len(a) == 0 {
		return False(io)
	}
	highbit := len(a) - 1
	a_high, a_rest := a[highbit:highbit+1], a[:highbit]
	b_high, b_rest := b[highbit:highbit+1], b[:highbit]
	return Or(io, And(io, Not(io, a_high), b_high), // a_high = 0, b_high = 1
		And(io, Not(io, Xor(io, a_high, b_high)), // a_high and b_high are the same
			Icmp_ugt(io, a_rest, b_rest))) // a_rest > b_rest (unsigned)
}

func Icmp_slt(io EvalVM, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_slt()")
	}
	return Icmp_sgt(io, b, a)
}

func Icmp_uge(io EvalVM, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_uge()")
	}
	c := True(io)
	for i := 0; i < len(a); i++ {
		ai := a[i : i+1]
		bi := b[i : i+1]
		c = Xor(io, ai, And(io, Xor(io, ai, c), Xor(io, bi, c)))
	}
	return c
}

func Icmp_ule(io EvalVM, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_ule()")
	}
	return Icmp_uge(io, b, a)
}

func Select(io EvalVM, s, a, b []base.Key) []base.Key {
	if len(s) != 1 {
		panic("Wire mismatch in eval.Select()")
	}
	if len(a) != len(b) {
		panic("Wire mismatch in eval.Select()")
	}
	result := make([]base.Key, len(a))
	for i := 0; i < len(a); i++ {
		result[i] = Xor(io, b[i:i+1], And(io, s, Xor(io, a[i:i+1], b[i:i+1])))[0]
	}
	return result
}

func Mask(io EvalVM, s, a []base.Key) []base.Key {
	if len(s) != 1 {
		panic("Mask: mask must be one bit")
	}
	switch len(a) {
	case 0:
		panic("Mask: empty value")
	case 1:
		return And(io, s, a)
	default:
		return And(io, Sext(io, s, len(a)), a)
	}
	panic("unreachable")
}

func LoadDebug(io EvalVM, mask, loc, eltsize []base.Key) []base.Key {
	if len(mask) != 1 {
		panic("LoadDebug")
	}
	r_eltsize := int(RevealUint32(io, eltsize))
	if !Reveal(io, mask)[0] {
		return Uint(io, 0, r_eltsize*8)
	}
	return Load(io, loc, eltsize)[:r_eltsize*8]
}

func StoreDebug(io EvalVM, mask, loc, eltsize, val []base.Key) {
	if len(mask) != 1 {
		panic("StoreDebug")
	}
	if !Reveal(io, mask)[0] {
		return
	}
	Store(io, loc, eltsize, val)
}

func Unsupported(x string) []base.Key {
	panic(x)
}

// NB Printf() reveals the active block as well as the values of its arguments
func Printf(io EvalVM, mask []base.Key, f string, args ...[]base.Key) {
	if len(mask) != 1 {
		panic("Printf")
	}
	if !Reveal(io, mask)[0] {
		return
	}
	for i := 0; i < len(args); i++ {
		RevealUint64(io, args[i])
	}
}

// NB Input32() reveals the active block
func Input32(io EvalVM, mask []base.Key, party []base.Key, next_arg func() uint64) []base.Key {
	if len(mask) != 1 {
		panic("Input32")
	}
	if !Reveal(io, mask)[0] {
		return Uint(io, 0, 32)
	}
	if 0 == RevealUint32(io, party) {
		// input from party 0 == gen
		return ShareTo1(io, 32)
	} else {
		// input from party 1 == eval
		return ShareTo0(io, next_arg(), 32)
	}
}

// Switch(io, s, dflt, c0, c1, ...) tests s.
// If s == 0 it returns c0, if s == 1 it returns c1, etc.
// If s is not the index of any c, it returns dflt.
func Switch(io EvalVM, s, dflt []base.Key, cases ...[]base.Key) []base.Key {
	if len(cases) == 0 {
		return dflt
	}
	masks := make([]base.Key, len(cases))
	masked_cases := make([][]base.Key, len(cases))
	for i := 0; i < len(cases); i++ {
		masks[i] = Icmp_eq(io, s, Uint(io, uint64(i), len(s)))[0]
		masked_cases[i] = Mask(io, []base.Key{masks[i]}, cases[i])
	}
	m := TreeXor0(io, masks...)
	x := TreeXor(io, masked_cases...)
	return Select(io, []base.Key{m}, x, dflt)
}

func Or0(io EvalVM, a, b base.Key) base.Key {
	result := Or(io, []base.Key{a}, []base.Key{b})[0]
	return result
}

func TreeOr0(io EvalVM, x ...base.Key) base.Key {
	switch len(x) {
	case 0:
		panic("TreeOr with no arguments")
	case 1:
		return x[0]
	case 2:
		return Or0(io, x[0], x[1])
	default:
		mid := len(x) / 2
		return Or0(io, TreeOr0(io, x[:mid]...), TreeOr0(io, x[mid:]...))
	}
	panic("unreachable")
}

func TreeOr(io EvalVM, x ...[]base.Key) []base.Key {
	switch len(x) {
	case 0:
		panic("TreeOr with no arguments")
	case 1:
		return x[0]
	case 2:
		return Or(io, x[0], x[1])
	default:
		mid := len(x) / 2
		return Or(io, TreeOr(io, x[:mid]...), TreeOr(io, x[mid:]...))
	}
	panic("unreachable")
}

func Xor0(io EvalVM, a, b base.Key) base.Key {
	// NB io is unused
	result := base.XorKey(a, b)
	return result
}

func TreeXor0(io EvalVM, x ...base.Key) base.Key {
	// NB io is unused
	switch len(x) {
	case 0:
		panic("TreeXor0 with no arguments")
	case 1:
		return x[0]
	case 2:
		return Xor0(io, x[0], x[1])
	default:
		mid := len(x) / 2
		return Xor0(io, TreeXor0(io, x[:mid]...), TreeXor0(io, x[mid:]...))
	}
	panic("unreachable")
}

func TreeXor(io EvalVM, x ...[]base.Key) []base.Key {
	switch len(x) {
	case 0:
		panic("TreeXor with no arguments")
	case 1:
		return x[0]
	case 2:
		return Xor(io, x[0], x[1])
	default:
		mid := len(x) / 2
		return Xor(io, TreeXor(io, x[:mid]...), TreeXor(io, x[mid:]...))
	}
	panic("unreachable")
}

func True(io EvalVM) []base.Key {
	return io.True()
}

func False(io EvalVM) []base.Key {
	return io.False()
}

func Uint(io EvalVM, a uint64, width int) []base.Key {
	if width > 64 {
		panic("Uint: width > 64")
	}
	result := make([]base.Key, width)
	const0 := False(io)[0]
	const1 := True(io)[0]
	for i := 0; i < width; i++ {
		if (a>>uint(i))%2 == 0 {
			result[i] = const0
		} else {
			result[i] = const1
		}
	}
	return result
}

func Int(io EvalVM, a int64, width int) []base.Key {
	return Uint(io, uint64(a), width)
}

func Not(io EvalVM, a []base.Key) []base.Key {
	const1 := True(io)[0]
	ones := make([]base.Key, len(a))
	for i := 0; i < len(ones); i++ {
		ones[i] = const1
	}
	return Xor(io, a, ones)
}

/* Reveal to all parties */
func Reveal(io EvalVM, a []base.Key) []bool {
	result := RevealTo1(io, a)
	RevealTo0(io, a)
	return result
}

func RevealTo0(io EvalVM, a []base.Key) {
	io.RevealTo0(a)
}

func RevealTo1(io EvalVM, a []base.Key) []bool {
	return io.RevealTo1(a)
}

func RevealUint32(io EvalVM, a []base.Key) uint32 {
	if len(a) > 32 {
		panic("RevealUint32: argument too large")
	}
	bits := Reveal(io, a)
	var result uint32
	for i := 0; i < len(bits); i++ {
		if bits[i] {
			result |= 1 << uint(i)
		}
	}
	return result
}

func RevealInt32(io EvalVM, a []base.Key) int32 {
	if len(a) > 32 {
		panic("RevealInt32: argument too large")
	}
	return int32(RevealUint32(io, a))
}

func RevealUint64(io EvalVM, a []base.Key) uint64 {
	if len(a) > 64 {
		panic("RevealUint64: argument too large")
	}
	bits := Reveal(io, a)
	var result uint64
	for i := 0; i < len(bits); i++ {
		if bits[i] {
			result |= 1 << uint(i)
		}
	}
	return result
}

func ShareTo0(io EvalVM, v uint64, bits int) []base.Key {
	return io.ShareTo0(v, bits)
}

func ShareTo1(io EvalVM, bits int) []base.Key {
	return io.ShareTo1(bits)
}

func Random(io EvalVM, bits int) []base.Key {
	return io.Random(bits)
}

/* Gen-side load */
func Load(io EvalVM, loc, eltsize []base.Key) []base.Key {
	RevealTo0(io, loc)
	RevealTo0(io, eltsize)
	return ShareTo1(io, 64)
}

/* Gen-side store */
func Store(io EvalVM, loc, eltsize, val []base.Key) {
	RevealTo0(io, loc)
	RevealTo0(io, eltsize)
	RevealTo0(io, val)
}
