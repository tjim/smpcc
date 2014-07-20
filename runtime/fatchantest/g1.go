package main

import . "github.com/tjim/smpcc/runtime/gen"
import "github.com/tjim/smpcc/runtime/base"
import "fmt"

func initialize_ram(io GenVM) {
	ram := make([]byte, 0x78)
	// @.str at location 0 == 0x0
	ram[0x0] = 0x42
	ram[0x1] = 0x69
	ram[0x2] = 0x6e
	ram[0x3] = 0x61
	ram[0x4] = 0x72
	ram[0x5] = 0x79
	ram[0x6] = 0x20
	ram[0x7] = 0x73
	ram[0x8] = 0x65
	ram[0x9] = 0x61
	ram[0xa] = 0x72
	ram[0xb] = 0x63
	ram[0xc] = 0x68
	ram[0xd] = 0x2c
	ram[0xe] = 0x20
	ram[0xf] = 0x65
	ram[0x10] = 0x78
	ram[0x11] = 0x70
	ram[0x12] = 0x65
	ram[0x13] = 0x63
	ram[0x14] = 0x74
	ram[0x15] = 0x65
	ram[0x16] = 0x64
	ram[0x17] = 0x20
	ram[0x18] = 0x6f
	ram[0x19] = 0x75
	ram[0x1a] = 0x74
	ram[0x1b] = 0x70
	ram[0x1c] = 0x75
	ram[0x1d] = 0x74
	ram[0x1e] = 0x73
	ram[0x1f] = 0x3a
	ram[0x20] = 0x20
	ram[0x21] = 0x31
	ram[0x22] = 0x20
	ram[0x23] = 0x74
	ram[0x24] = 0x68
	ram[0x25] = 0x65
	ram[0x26] = 0x6e
	ram[0x27] = 0x20
	ram[0x28] = 0x30
	ram[0x29] = 0xa
	// @_global_x1 at location 44 == 0x2c
	ram[0x2c] = 0x1
	ram[0x30] = 0x2
	ram[0x34] = 0x3
	ram[0x38] = 0x4
	ram[0x3c] = 0x5
	ram[0x40] = 0x6
	ram[0x44] = 0x7
	// @.str1 at location 72 == 0x48
	ram[0x48] = 0x25
	ram[0x49] = 0x64
	ram[0x4a] = 0xa
	// @str at location 76 == 0x4c
	ram[0x4c] = 0x42
	ram[0x4d] = 0x69
	ram[0x4e] = 0x6e
	ram[0x4f] = 0x61
	ram[0x50] = 0x72
	ram[0x51] = 0x79
	ram[0x52] = 0x20
	ram[0x53] = 0x73
	ram[0x54] = 0x65
	ram[0x55] = 0x61
	ram[0x56] = 0x72
	ram[0x57] = 0x63
	ram[0x58] = 0x68
	ram[0x59] = 0x2c
	ram[0x5a] = 0x20
	ram[0x5b] = 0x65
	ram[0x5c] = 0x78
	ram[0x5d] = 0x70
	ram[0x5e] = 0x65
	ram[0x5f] = 0x63
	ram[0x60] = 0x74
	ram[0x61] = 0x65
	ram[0x62] = 0x64
	ram[0x63] = 0x20
	ram[0x64] = 0x6f
	ram[0x65] = 0x75
	ram[0x66] = 0x74
	ram[0x67] = 0x70
	ram[0x68] = 0x75
	ram[0x69] = 0x74
	ram[0x6a] = 0x73
	ram[0x6b] = 0x3a
	ram[0x6c] = 0x20
	ram[0x6d] = 0x31
	ram[0x6e] = 0x20
	ram[0x6f] = 0x74
	ram[0x70] = 0x68
	ram[0x71] = 0x65
	ram[0x72] = 0x6e
	ram[0x73] = 0x20
	ram[0x74] = 0x30
	InitRam(ram)
}

func gen_main(io, io0, io1, io2, io3, io4, io5, io6, io7, io8, io9, io10, io11, io12 GenVM) {

	initialize_ram(io)

	/* create output channels */
	ch0 := make(chan []base.Wire, 1)
	ch1 := make(chan []base.Wire, 1)
	ch2 := make(chan []base.Wire, 5)
	ch3 := make(chan []base.Wire, 2)
	ch4 := make(chan []base.Wire, 3)
	ch5 := make(chan []base.Wire, 1)
	ch6 := make(chan []base.Wire, 2)
	ch7 := make(chan []base.Wire, 3)
	ch8 := make(chan []base.Wire, 2)
	ch9 := make(chan []base.Wire, 3)
	ch10 := make(chan []base.Wire, 3)
	ch11 := make(chan []base.Wire, 3)
	ch12 := make(chan []base.Wire, 2)

	/* special variables */
	_attsrcAnswer := Uint(io, 0, 32)
	_attsrcIsDone := Uint(io, 0, 1)
	_attsrcMemAct := Uint(io, 0, 2)
	_attsrcMemLoc := Uint(io, 0, 64)
	_attsrcMemSize := Uint(io, 0, 32)
	_attsrcStateO := Uint(io, 0, 32)

	/* block free variables */
	_5 := Uint(io, 0, 32)
	_8 := Uint(io, 0, 32)
	___search_left_0 := Uint(io, 0, 32)
	__search___retres7_0 := Uint(io, 0, 32)
	__search_left_01 := Uint(io, 0, 32)
	__search_ret_site_0 := Uint(io, 0, 32)
	__search_right_0_ := Uint(io, 0, 32)
	__search_right_02 := Uint(io, 0, 32)
	__search_v_0 := Uint(io, 0, 32)
	_attsrcMemRes := Uint(io, 0, 64)

	done := false
	for !done {

		/* one goroutine invocation per block */
		go gen0(io0, ch0, _attsrcStateO)
		go gen1(io1, ch1, _attsrcStateO)
		go gen2(io2, ch2, _attsrcStateO, __search_left_01, __search_right_02)
		go gen3(io3, ch3, _attsrcStateO, __search_v_0, _attsrcMemRes)
		go gen4(io4, ch4, _attsrcStateO, _5, _8, __search_left_01, __search_right_02, __search_v_0)
		go gen5(io5, ch5, _attsrcStateO, __search___retres7_0, __search_ret_site_0)
		go gen6(io6, ch6, _attsrcStateO)
		go gen7(io7, ch7, _attsrcStateO, ___search_left_0, __search_right_0_)
		go gen8(io8, ch8, _attsrcStateO)
		go gen9(io9, ch9, _attsrcStateO)
		go gen10(io10, ch10, _attsrcStateO)
		go gen11(io11, ch11, _attsrcStateO)
		go gen12(io12, ch12, _attsrcStateO)

		/* mux the outputs*/
		_attsrcStateO_0 := <- ch0
		_attsrcStateO_1 := <- ch1
		mask_2 := <- ch2
		_5_2 := <- ch2
		_attsrcMemAct_2 := <- ch2
		_attsrcMemLoc_2 := <- ch2
		_attsrcMemSize_2 := <- ch2
		_attsrcStateO_2 := <- ch2
		mask_3 := <- ch3
		_8_3 := <- ch3
		_attsrcStateO_3 := <- ch3
		mask_4 := <- ch4
		___search_left_0_4 := <- ch4
		__search_right_0__4 := <- ch4
		_attsrcStateO_4 := <- ch4
		_attsrcStateO_5 := <- ch5
		_attsrcAnswer_6 := <- ch6
		_attsrcIsDone_6 := <- ch6
		mask_7 := <- ch7
		__search_left_01_7 := <- ch7
		__search_right_02_7 := <- ch7
		_attsrcStateO_7 := <- ch7
		mask_8 := <- ch8
		__search___retres7_0_8 := <- ch8
		_attsrcStateO_8 := <- ch8
		mask_9 := <- ch9
		__search_left_01_9 := <- ch9
		__search_right_02_9 := <- ch9
		_attsrcStateO_9 := <- ch9
		mask_10 := <- ch10
		__search_ret_site_0_10 := <- ch10
		__search_v_0_10 := <- ch10
		_attsrcStateO_10 := <- ch10
		mask_11 := <- ch11
		__search_ret_site_0_11 := <- ch11
		__search_v_0_11 := <- ch11
		_attsrcStateO_11 := <- ch11
		mask_12 := <- ch12
		__search___retres7_0_12 := <- ch12
		_attsrcStateO_12 := <- ch12
		_5 = Select(io, TreeXor(io, mask_2), TreeXor(io, _5_2), _5)
		_8 = Select(io, TreeXor(io, mask_3), TreeXor(io, _8_3), _8)
		___search_left_0 = Select(io, TreeXor(io, mask_4), TreeXor(io, ___search_left_0_4), ___search_left_0)
		__search___retres7_0 = Select(io, TreeXor(io, mask_8, mask_12), TreeXor(io, __search___retres7_0_8, __search___retres7_0_12), __search___retres7_0)
		__search_left_01 = Select(io, TreeXor(io, mask_7, mask_9), TreeXor(io, __search_left_01_7, __search_left_01_9), __search_left_01)
		__search_ret_site_0 = Select(io, TreeXor(io, mask_10, mask_11), TreeXor(io, __search_ret_site_0_10, __search_ret_site_0_11), __search_ret_site_0)
		__search_right_0_ = Select(io, TreeXor(io, mask_4), TreeXor(io, __search_right_0__4), __search_right_0_)
		__search_right_02 = Select(io, TreeXor(io, mask_7, mask_9), TreeXor(io, __search_right_02_7, __search_right_02_9), __search_right_02)
		__search_v_0 = Select(io, TreeXor(io, mask_10, mask_11), TreeXor(io, __search_v_0_10, __search_v_0_11), __search_v_0)
		_attsrcAnswer = TreeXor(io, _attsrcAnswer_6)
		_attsrcIsDone = TreeXor(io, _attsrcIsDone_6)
		_attsrcMemAct = TreeXor(io, _attsrcMemAct_2)
		_attsrcMemLoc = TreeXor(io, _attsrcMemLoc_2)
		_attsrcMemSize = TreeXor(io, _attsrcMemSize_2)
		_attsrcStateO = TreeXor(io, _attsrcStateO_0, _attsrcStateO_1, _attsrcStateO_2, _attsrcStateO_3, _attsrcStateO_4, _attsrcStateO_5, _attsrcStateO_7, _attsrcStateO_8, _attsrcStateO_9, _attsrcStateO_10, _attsrcStateO_11, _attsrcStateO_12)

		/* load from memory if necessary */
		if Reveal(io, Icmp_eq(io, _attsrcMemAct, Uint(io, 1, 2)))[0] {
			_attsrcMemRes = Load(io, _attsrcMemLoc, _attsrcMemSize)
		}

		/* are we done? */
		done = Reveal(io, _attsrcIsDone)[0]
	}
	answer := RevealInt32(io, _attsrcAnswer)
	fmt.Printf("gen: %v\n", answer)
	_main_done <- true
}

// <label>:0
func gen0(io GenVM, ch chan []base.Wire, block_num []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 0, 32))
	Printf(io, mask, "Binary search, expected outputs: 1 then 0\n")
	_attsrcStateO := Uint(io, 10, 32)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:1
func gen1(io GenVM, ch chan []base.Wire, block_num []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 1, 32))
	_attsrcStateO := Uint(io, 9, 32)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:2
func gen2(io GenVM, ch chan []base.Wire, block_num, __search_left_01, __search_right_02 []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 2, 32))
	_3 := Sub(io, __search_right_02, __search_left_01)
	_4 := Lshr(io, _3, 1)
	_5 := Add(io, _4, __search_left_01)
	_6 := Zext(io, _5, 64)
	_x17 := Add(io, Int(io, 0, 64), Uint(io, 44, 64))
	_x18 := Shl(io, _6, 2)
	_x19 := Add(io, _x17, _x18)
	_7 := _x19
	_attsrcMemAct := Int(io, 1, 2)
	_attsrcMemSize := Int(io, 4, 32)
	_attsrcMemLoc := _7
	_attsrcStateO := Uint(io, 3, 32)
	ch <- mask
	ch <- Mask(io, mask, _5)
	ch <- Mask(io, mask, _attsrcMemAct)
	ch <- Mask(io, mask, _attsrcMemLoc)
	ch <- Mask(io, mask, _attsrcMemSize)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:attsrcLabel16
func gen3(io GenVM, ch chan []base.Wire, block_num, __search_v_0, _attsrcMemRes []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 3, 32))
	_8 := _attsrcMemRes[:32]
	_9 := Icmp_eq(io, _8, __search_v_0)
	_attsrcStateO := Select(io, _9, Uint(io, 12, 32), Uint(io, 4, 32))
	ch <- mask
	ch <- Mask(io, mask, _8)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:10
func gen4(io GenVM, ch chan []base.Wire, block_num, _5, _8, __search_left_01, __search_right_02, __search_v_0 []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 4, 32))
	_11 := Icmp_ult(io, _8, __search_v_0)
	_12 := Add(io, _5, Int(io, 1, 32))
	___search_left_0 := Select(io, _11, _12, __search_left_01)
	__search_right_0_ := Select(io, _11, __search_right_02, _5)
	_13 := Icmp_ugt(io, __search_right_0_, ___search_left_0)
	_attsrcStateO := Select(io, _13, Uint(io, 7, 32), Uint(io, 8, 32))
	ch <- mask
	ch <- Mask(io, mask, ___search_left_0)
	ch <- Mask(io, mask, __search_right_0_)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:14
func gen5(io GenVM, ch chan []base.Wire, block_num, __search___retres7_0, __search_ret_site_0 []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 5, 32))
	_cond := Icmp_eq(io, __search_ret_site_0, Int(io, 0, 32))
	Printf(io, mask, "%d\n", __search___retres7_0)
	_attsrcStateO := Select(io, _cond, Uint(io, 11, 32), Uint(io, 6, 32))
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:16
func gen6(io GenVM, ch chan []base.Wire, block_num []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 6, 32))
	_attsrcAnswer := Int(io, 0, 32)
	_attsrcIsDone := Int(io, 1, 1)
	ch <- Mask(io, mask, _attsrcAnswer)
	ch <- Mask(io, mask, _attsrcIsDone)
}

// <label>:attsrcLabel3
func gen7(io GenVM, ch chan []base.Wire, block_num, ___search_left_0, __search_right_0_ []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 7, 32))
	_x14 := ___search_left_0
	_x15 := __search_right_0_
	__search_left_01 := _x14
	__search_right_02 := _x15
	_attsrcStateO := Uint(io, 2, 32)
	ch <- mask
	ch <- Mask(io, mask, __search_left_01)
	ch <- Mask(io, mask, __search_right_02)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:attsrcLabel5
func gen8(io GenVM, ch chan []base.Wire, block_num []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 8, 32))
	_x13 := Int(io, 0, 32)
	__search___retres7_0 := _x13
	_attsrcStateO := Uint(io, 5, 32)
	ch <- mask
	ch <- Mask(io, mask, __search___retres7_0)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:attsrcLabel2
func gen9(io GenVM, ch chan []base.Wire, block_num []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 9, 32))
	_x11 := Int(io, 0, 32)
	_x12 := Int(io, 7, 32)
	__search_left_01 := _x11
	__search_right_02 := _x12
	_attsrcStateO := Uint(io, 2, 32)
	ch <- mask
	ch <- Mask(io, mask, __search_left_01)
	ch <- Mask(io, mask, __search_right_02)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:attsrcLabel0
func gen10(io GenVM, ch chan []base.Wire, block_num []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 10, 32))
	_x9 := Int(io, 0, 32)
	_x10 := Int(io, 3, 32)
	__search_ret_site_0 := _x9
	__search_v_0 := _x10
	_attsrcStateO := Uint(io, 1, 32)
	ch <- mask
	ch <- Mask(io, mask, __search_ret_site_0)
	ch <- Mask(io, mask, __search_v_0)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:attsrcLabel1
func gen11(io GenVM, ch chan []base.Wire, block_num []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 11, 32))
	_x7 := Int(io, 1, 32)
	_x8 := Int(io, 10, 32)
	__search_ret_site_0 := _x7
	__search_v_0 := _x8
	_attsrcStateO := Uint(io, 1, 32)
	ch <- mask
	ch <- Mask(io, mask, __search_ret_site_0)
	ch <- Mask(io, mask, __search_v_0)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:attsrcLabel4
func gen12(io GenVM, ch chan []base.Wire, block_num []base.Wire) {
	mask := Icmp_eq(io, block_num, Uint(io, 12, 32))
	_x6 := Int(io, 1, 32)
	__search___retres7_0 := _x6
	_attsrcStateO := Uint(io, 5, 32)
	ch <- mask
	ch <- Mask(io, mask, __search___retres7_0)
	ch <- Mask(io, mask, _attsrcStateO)
}

