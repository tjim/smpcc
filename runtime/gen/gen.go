package gen

import (
	"github.com/tjim/smpcc/runtime/base"
	"fmt"
)

type GenVM interface {
	InitRam(contents []byte)
	Add(a, b []base.Wire) []base.Wire
	Sub(a, b []base.Wire) []base.Wire
	And(a, b []base.Wire) []base.Wire
	Or(a, b []base.Wire) []base.Wire
	Xor(a, b []base.Wire) []base.Wire
	Icmp_ugt(a, b []base.Wire) []base.Wire
	Icmp_uge(a, b []base.Wire) []base.Wire
	Select(s, a, b []base.Wire) []base.Wire
	Const(bits ...int) []base.Wire
	Uint(a uint64, width int) []base.Wire
	Nand(a, b []base.Wire) []base.Wire
	Not(a []base.Wire) []base.Wire
	Reveal(a []base.Wire) []bool
	OT(bits int) []base.Wire
	BT(a uint64, bits int) []base.Wire
	Random(bits int) []base.Wire
	Load(loc, eltsize []base.Wire) []base.Wire
	Store(loc, eltsize, val []base.Wire)
}

func Add(io GenVM, a, b []base.Wire) []base.Wire {
	return io.Add(a, b)
}

func Sub(io GenVM, a, b []base.Wire) []base.Wire {
	return io.Sub(a, b)
}

/* constant shift left; TODO: variable Shl */
func Shl(io GenVM, a []base.Wire, b int) []base.Wire {
	if len(a) <= b {
		panic("Shl() too far")
	}
	z := Const(io, 0)
	zeros := make([]base.Wire, b)
	for i := 0; i < len(zeros); i++ {
		zeros[i] = z[0]
	}
	return append(zeros, a...)[:len(a)]
}

/* constant logical shift right; TODO: variable Lshr */
func Lshr(io GenVM, a []base.Wire, b int) []base.Wire {
	if len(a) <= b {
		panic("Lshr() too far")
	}
	result := Zext(io, a, b+len(a))
	return result[b:]
}

/* constant logical shift right; TODO: variable Ashr */
func Ashr(io GenVM, a []base.Wire, b int) []base.Wire {
	if len(a) <= b {
		panic("Ashr() too far")
	}
	result := Sext(io, a, b+len(a))
	return result[b:]
}

func And(io GenVM, a, b []base.Wire) []base.Wire {
	return io.And(a, b)
}

func Or(io GenVM, a, b []base.Wire) []base.Wire {
	return io.Or(a, b)
}

func Xor(io GenVM, a, b []base.Wire) []base.Wire {
	return io.Xor(a, b)
}

func Trunc(io GenVM, a []base.Wire, b int) []base.Wire {
	if len(a) <= b {
		panic("trunc must truncate operand")
	}
	return a[:b]
}

func Zext(io GenVM, a []base.Wire, b int) []base.Wire {
	if len(a) >= b {
		panic("zext must extend operand")
	}
	if b == len(a)+1 {
		return append(a, Const(io, 0)...)
	}
	return Sext(io, append(a, Const(io, 0)...), b)
}

func Sext(io GenVM, a []base.Wire, b int) []base.Wire {
	if len(a) >= b {
		panic("sext must extend operand")
	}
	if len(a) == 0 {
		panic("sext on zero-length operand")
	}
	newbits := make([]base.Wire, b-len(a))
	for i := 0; i < len(newbits); i++ {
		newbits[i] = a[len(a)-1]
	}
	return append(a, newbits...)
}

func Icmp_eq(io GenVM, a, b []base.Wire) []base.Wire {
	if len(a) != len(b) {
		panic("Wire mismatch in gen.Icmp_eq()")
	}
	if len(a) == 0 {
		panic("empty arguments in gen.Icmp_eq()")
	}
	bitwise_inequality := Xor(io, a, b)
	return Not(io, []base.Wire{treeOr0(io, bitwise_inequality...)})
}

func Icmp_ugt(io GenVM, a, b []base.Wire) []base.Wire {
	return io.Icmp_ugt(a, b)
}

func Icmp_ult(io GenVM, a, b []base.Wire) []base.Wire {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_ult()")
	}
	return Icmp_ugt(io, b, a)
}

func Icmp_sgt(io GenVM, a, b []base.Wire) []base.Wire {
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

func Icmp_slt(io GenVM, a, b []base.Wire) []base.Wire {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_slt()")
	}
	return Icmp_sgt(io, b, a)
}

func Icmp_uge(io GenVM, a, b []base.Wire) []base.Wire {
	return io.Icmp_uge(a, b)
}

func Icmp_ule(io GenVM, a, b []base.Wire) []base.Wire {
	if len(a) != len(b) {
		panic("argument mismatch in Icmp_ule()")
	}
	return Icmp_uge(io, b, a)
}

func Select(io GenVM, s, a, b []base.Wire) []base.Wire {
	return io.Select(s, a, b)
}

func Mask(io GenVM, s, a []base.Wire) []base.Wire {
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

func LoadDebug(io GenVM, mask, loc, eltsize []base.Wire) []base.Wire {
	if len(mask) != 1 {
		panic("LoadDebug")
	}
	r_eltsize := int(RevealUint32(io, eltsize))
	if !Reveal(io, mask)[0] {
		return Uint(io, 0, r_eltsize*8)
	}
	return Load(io, loc, eltsize)[:r_eltsize*8]
}

func StoreDebug(io GenVM, mask, loc, eltsize, val []base.Wire) {
	if len(mask) != 1 {
		panic("StoreDebug")
	}
	if !Reveal(io, mask)[0] {
		return
	}
	Store(io, loc, eltsize, val)
}

func Unsupported(x string) []base.Wire {
	panic(x)
}

// NB Printf() reveals the active block as well as the values of its arguments
func Printf(io GenVM, mask []base.Wire, f string, args ...[]base.Wire) {
	if len(mask) != 1 {
		panic("Printf")
	}
	if !Reveal(io, mask)[0] {
		return
	}
	fargs := make([]interface{}, len(args))
	for i := 0; i < len(args); i++ {
		fargs[i] = RevealUint64(io, args[i])
	}
	fmt.Printf(f, fargs...)
}

// NB Gen_int() reveals the active block
func Gen_int(io GenVM, mask []base.Wire, next_arg func() uint64) []base.Wire {
	if len(mask) != 1 {
		panic("Gen_int")
	}
	if !Reveal(io, mask)[0] {
		return Uint(io, 0, 32)
	}
	return BT(io, next_arg(), 32)
}

// NB Eval_int() reveals the active block
func Eval_int(io GenVM, mask []base.Wire) []base.Wire {
	if len(mask) != 1 {
		panic("Eval_int")
	}
	if !Reveal(io, mask)[0] {
		return Uint(io, 0, 32)
	}
	return OT(io, 32)
}

// Switch(io, s, dflt, c0, c1, ...) tests s.
// If s == 0 it returns c0, if s == 1 it returns c1, etc.
// If s is not the index of any c, it returns dflt.
func Switch(io GenVM, s, dflt []base.Wire, cases ...[]base.Wire) []base.Wire {
	if len(cases) == 0 {
		return dflt
	}
	masks := make([]base.Wire, len(cases))
	masked_cases := make([][]base.Wire, len(cases))
	for i := 0; i < len(cases); i++ {
		masks[i] = Icmp_eq(io, s, Uint(io, uint64(i), len(s)))[0]
		masked_cases[i] = Mask(io, []base.Wire{masks[i]}, cases[i])
	}
	m := TreeXor0(io, masks...)
	x := TreeXor(io, masked_cases...)
	return Select(io, []base.Wire{m}, x, dflt)
}

func TreeOr(io GenVM, x ...[]base.Wire) []base.Wire {
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

func Xor0(io GenVM, a, b base.Wire) base.Wire {
	result := Xor(io, []base.Wire{a}, []base.Wire{b})[0]
	return result
}

func TreeXor0(io GenVM, x ...base.Wire) base.Wire {
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

func TreeXor(io GenVM, x ...[]base.Wire) []base.Wire {
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

func or0(io GenVM, a, b base.Wire) base.Wire {
	result := Or(io, []base.Wire{a}, []base.Wire{b})[0]
	return result
}

func treeOr0(io GenVM, x ...base.Wire) base.Wire {
	switch len(x) {
	case 0:
		panic("TreeOr with no arguments")
	case 1:
		return x[0]
	case 2:
		return or0(io, x[0], x[1])
	default:
		mid := len(x) / 2
		return or0(io, treeOr0(io, x[:mid]...), treeOr0(io, x[mid:]...))
	}
	panic("unreachable")
}

func Const(io GenVM, bits ...int) []base.Wire {
	return io.Const(bits...)
}

func Uint(io GenVM, a uint64, width int) []base.Wire {
	return io.Uint(a, width)
}

func Int(io GenVM, a int64, width int) []base.Wire {
	return io.Uint(uint64(a), width)
}

func Nand(io GenVM, a, b []base.Wire) []base.Wire {
	return io.Nand(a, b)
}

func Not(io GenVM, a []base.Wire) []base.Wire {
	return io.Not(a)
}

func Reveal(io GenVM, a []base.Wire) []bool {
	return io.Reveal(a)
}

func RevealUint32(io GenVM, a []base.Wire) uint32 {
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

func RevealInt32(io GenVM, a []base.Wire) int32 {
	if len(a) > 32 {
		panic("RevealInt32: argument too large")
	}
	return int32(RevealUint32(io, a))
}

func RevealUint64(io GenVM, a []base.Wire) uint64 {
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

func OT(io GenVM, bits int) []base.Wire {
	return io.OT(bits)
}

func BT(io GenVM, a uint64, bits int) []base.Wire {
	return io.BT(a, bits)
}

func Random(io GenVM, bits int) []base.Wire {
	return io.Random(bits)
}

func Load(io GenVM, loc, eltsize []base.Wire) []base.Wire {
	return io.Load(loc, eltsize)
}

func Store(io GenVM, loc, eltsize, val []base.Wire) {
	io.Store(loc, eltsize, val)
}
