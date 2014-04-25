package eval

import "github.com/tjim/smpcc/runtime/base"

type EvalVM interface {
	Add(a, b []base.Key) []base.Key
	Sub(a, b []base.Key) []base.Key
	And(a, b []base.Key) []base.Key
	Or(a, b []base.Key) []base.Key
	Xor(a, b []base.Key) []base.Key
	Icmp_ugt(a, b []base.Key) []base.Key
	Icmp_uge(a, b []base.Key) []base.Key
	Select(s, a, b []base.Key) []base.Key
	Const(bits ...int) []base.Key
	Uint(a uint64, width int) []base.Key
	Nand(a, b []base.Key) []base.Key
	Not(a []base.Key) []base.Key
	Reveal(a []base.Key) []bool
	OT(v uint64, bits int) []base.Key
	BT(bits int) []base.Key
	Random(bits int) []base.Key
	Load(loc, eltsize []base.Key) []base.Key
	Store(loc, eltsize, val []base.Key)
}

func Add(io EvalVM, a, b []base.Key) []base.Key {
	return io.Add(a, b)
}

func Sub(io EvalVM, a, b []base.Key) []base.Key {
	return io.Sub(a, b)
}

/* constant shift left; TODO: variable shift left */
func Shl(io EvalVM, a []base.Key, b int) []base.Key {
	if len(a) <= b {
		panic("Shl() too far")
	}
	z := Const(io, 0)
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
		return append(a, Const(io, 0)...)
	}
	return Sext(io, append(a, Const(io, 0)...), b)
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
	return io.Icmp_ugt(a, b)
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
		return Const(io, 0)
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
	return io.Icmp_uge(a, b)
}

func Icmp_ule(io EvalVM, a, b []base.Key) []base.Key {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_ule()")
	}
	return Icmp_uge(io, b, a)
}

func Select(io EvalVM, s, a, b []base.Key) []base.Key {
	return io.Select(s, a, b)
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

func LoadDebug(io EvalVM, mask, loc []base.Key) []base.Key {
	if len(mask) != 1 {
		panic("LoadDebug")
	}
	if !Reveal(io, mask)[0] {
		return Uint(io, 0, 32)
	}
	return Load(io, loc, Uint(io, 4, 32))
}

func StoreDebug(io EvalVM, mask, loc, val []base.Key) {
	if len(mask) != 1 {
		panic("StoreDebug")
	}
	if !Reveal(io, mask)[0] {
		return
	}
	Store(io, loc, Uint(io, 4, 32), val)
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

// NB Gen_int() reveals the active block
func Gen_int(io EvalVM, mask []base.Key) []base.Key {
	if len(mask) != 1 {
		panic("Gen_int")
	}
	if !Reveal(io, mask)[0] {
		return Uint(io, 0, 32)
	}
	return BT(io, 32)
}

// NB Eval_int() reveals the active block
func Eval_int(io EvalVM, mask []base.Key, next_arg func() uint64) []base.Key {
	if len(mask) != 1 {
		panic("Eval_int")
	}
	if !Reveal(io, mask)[0] {
		return Uint(io, 0, 32)
	}
	return OT(io, next_arg(), 32)
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

func Const(io EvalVM, bits ...int) []base.Key {
	return io.Const(bits...)
}

func Uint(io EvalVM, a uint64, width int) []base.Key {
	return io.Uint(a, width)
}

func Int(io EvalVM, a int64, width int) []base.Key {
	return io.Uint(uint64(a), width)
}

func Nand(io EvalVM, a, b []base.Key) []base.Key {
	return io.Nand(a, b)
}

func Not(io EvalVM, a []base.Key) []base.Key {
	return io.Not(a)
}

func Reveal(io EvalVM, a []base.Key) []bool {
	return io.Reveal(a)
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

func OT(io EvalVM, v uint64, bits int) []base.Key {
	return io.OT(v, bits)
}

func BT(io EvalVM, bits int) []base.Key {
	return io.BT(bits)
}

func Random(io EvalVM, bits int) []base.Key {
	return io.Random(bits)
}

func Load(io EvalVM, loc, eltsize []base.Key) []base.Key {
	return io.Load(loc, eltsize)
}

func Store(io EvalVM, loc, eltsize, val []base.Key) {
	io.Store(loc, eltsize, val)
}
