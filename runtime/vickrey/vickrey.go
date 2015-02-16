package vickrey

import . "github.com/tjim/smpcc/runtime/gmw"
import "fmt"

func initialize_ram(io Io) {
	ram := make([]byte, 0x14)
	// @.str at location 0 == 0x0
	ram[0x0] = Uint8(io, 0x42)
	ram[0x1] = Uint8(io, 0x69)
	ram[0x2] = Uint8(io, 0x64)
	ram[0x3] = Uint8(io, 0x64)
	ram[0x4] = Uint8(io, 0x65)
	ram[0x5] = Uint8(io, 0x72)
	ram[0x6] = Uint8(io, 0x20)
	ram[0x7] = Uint8(io, 0x25)
	ram[0x8] = Uint8(io, 0x64)
	ram[0x9] = Uint8(io, 0x20)
	ram[0xa] = Uint8(io, 0x70)
	ram[0xb] = Uint8(io, 0x61)
	ram[0xc] = Uint8(io, 0x79)
	ram[0xd] = Uint8(io, 0x73)
	ram[0xe] = Uint8(io, 0x20)
	ram[0xf] = Uint8(io, 0x25)
	ram[0x10] = Uint8(io, 0x64)
	ram[0x11] = Uint8(io, 0xa)
	io.InitRam(ram)
}

func blocks_main(io Io, ios []Io) {

	initialize_ram(io)

	/* create output channels */
	ch0 := make(chan uint64, 4)
	ch1 := make(chan uint64, 4)
	ch2 := make(chan uint64, 3)
	ch3 := make(chan uint64, 4)
	ch4 := make(chan uint64, 2)
	ch5 := make(chan uint64, 4)
	ch6 := make(chan uint64, 6)
	ch7 := make(chan uint64, 5)
	ch8 := make(chan uint64, 4)
	ch9 := make(chan uint64, 6)
	ch10 := make(chan uint64, 5)

	/* special variables */
	_vAnswer := Uint32(io, 0)
	_vIsDone := Uint1(io, 0)

	/* block masks */
	_block0 := Uint1(io, 0)
	_block1 := Uint1(io, 0)
	_block10 := Uint1(io, 0)
	_block2 := Uint1(io, 0)
	_block3 := Uint1(io, 0)
	_block4 := Uint1(io, 0)
	_block5 := Uint1(io, 0)
	_block6 := Uint1(io, 0)
	_block7 := Uint1(io, 0)
	_block8 := Uint1(io, 0)
	_block9 := Uint1(io, 0)
	_block0 = Uint1(io, 1)

	/* block free variables */
	_1 := Uint32(io, 0)
	_4 := Uint32(io, 0)
	_9 := Uint32(io, 0)
	___main_penultimate_0 := Uint32(io, 0)
	__main_bidder_0_lcssa := Uint32(io, 0)
	__main_bidder_04 := Uint32(io, 0)
	__main_bidder_1 := Uint32(io, 0)
	__main_i_01 := Uint32(io, 0)
	__main_penultimate_0_lcssa := Uint32(io, 0)
	__main_penultimate_02 := Uint32(io, 0)
	__main_penultimate_1 := Uint32(io, 0)
	__main_ultimate_03 := Uint32(io, 0)
	__main_ultimate_1 := Uint32(io, 0)

	done := false
	for !done {

		/* one goroutine invocation per block */
		go block0(ios[0], ch0, _block0)
		go block1(ios[1], ch1, _block1, __main_i_01, __main_ultimate_03)
		go block2(ios[2], ch2, _block2, _4, __main_penultimate_02)
		go block3(ios[3], ch3, _block3, __main_i_01)
		go block4(ios[4], ch4, _block4, __main_bidder_0_lcssa, __main_penultimate_0_lcssa)
		go block5(ios[5], ch5, _block5)
		go block6(ios[6], ch6, _block6, _1)
		go block7(ios[7], ch7, _block7, _4, __main_i_01, __main_ultimate_03)
		go block8(ios[8], ch8, _block8, __main_bidder_1, __main_penultimate_1)
		go block9(ios[9], ch9, _block9, _9, __main_bidder_1, __main_penultimate_1, __main_ultimate_1)
		go block10(ios[10], ch10, _block10, ___main_penultimate_0, __main_bidder_04, __main_ultimate_03)

		/* mux the outputs*/
		mask_0 := (<-ch0) > 0
		_1_0 := uint32(<-ch0)
		_block0_0 := (<-ch0) > 0
		_block5_0 := (<-ch0) > 0
		_block6_0 := (<-ch0) > 0
		mask_1 := (<-ch1) > 0
		_4_1 := uint32(<-ch1)
		_block1_1 := (<-ch1) > 0
		_block2_1 := (<-ch1) > 0
		_block7_1 := (<-ch1) > 0
		mask_2 := (<-ch2) > 0
		___main_penultimate_0_2 := uint32(<-ch2)
		_block10_2 := (<-ch2) > 0
		_block2_2 := (<-ch2) > 0
		mask_3 := (<-ch3) > 0
		_9_3 := uint32(<-ch3)
		_block3_3 := (<-ch3) > 0
		_block8_3 := (<-ch3) > 0
		_block9_3 := (<-ch3) > 0
		_vAnswer_4 := uint32(<-ch4)
		_vIsDone_4 := (<-ch4) > 0
		mask_5 := (<-ch5) > 0
		__main_bidder_0_lcssa_5 := uint32(<-ch5)
		__main_penultimate_0_lcssa_5 := uint32(<-ch5)
		_block4_5 := (<-ch5) > 0
		_block5_5 := (<-ch5) > 0
		mask_6 := (<-ch6) > 0
		__main_bidder_04_6 := uint32(<-ch6)
		__main_i_01_6 := uint32(<-ch6)
		__main_penultimate_02_6 := uint32(<-ch6)
		__main_ultimate_03_6 := uint32(<-ch6)
		_block1_6 := (<-ch6) > 0
		_block6_6 := (<-ch6) > 0
		mask_7 := (<-ch7) > 0
		__main_bidder_1_7 := uint32(<-ch7)
		__main_penultimate_1_7 := uint32(<-ch7)
		__main_ultimate_1_7 := uint32(<-ch7)
		_block3_7 := (<-ch7) > 0
		_block7_7 := (<-ch7) > 0
		mask_8 := (<-ch8) > 0
		__main_bidder_0_lcssa_8 := uint32(<-ch8)
		__main_penultimate_0_lcssa_8 := uint32(<-ch8)
		_block4_8 := (<-ch8) > 0
		_block8_8 := (<-ch8) > 0
		mask_9 := (<-ch9) > 0
		__main_bidder_04_9 := uint32(<-ch9)
		__main_i_01_9 := uint32(<-ch9)
		__main_penultimate_02_9 := uint32(<-ch9)
		__main_ultimate_03_9 := uint32(<-ch9)
		_block1_9 := (<-ch9) > 0
		_block9_9 := (<-ch9) > 0
		mask_10 := (<-ch10) > 0
		__main_bidder_1_10 := uint32(<-ch10)
		__main_penultimate_1_10 := uint32(<-ch10)
		__main_ultimate_1_10 := uint32(<-ch10)
		_block10_10 := (<-ch10) > 0
		_block3_10 := (<-ch10) > 0
		_1 = TreeXor32(io, _1_0, Mask32(io, Not1(io, TreeXor1(io, mask_0)), _1))
		_4 = TreeXor32(io, _4_1, Mask32(io, Not1(io, TreeXor1(io, mask_1)), _4))
		_9 = TreeXor32(io, _9_3, Mask32(io, Not1(io, TreeXor1(io, mask_3)), _9))
		___main_penultimate_0 = TreeXor32(io, ___main_penultimate_0_2, Mask32(io, Not1(io, TreeXor1(io, mask_2)), ___main_penultimate_0))
		__main_bidder_0_lcssa = TreeXor32(io, __main_bidder_0_lcssa_5, __main_bidder_0_lcssa_8, Mask32(io, Not1(io, TreeXor1(io, mask_5, mask_8)), __main_bidder_0_lcssa))
		__main_bidder_04 = TreeXor32(io, __main_bidder_04_6, __main_bidder_04_9, Mask32(io, Not1(io, TreeXor1(io, mask_6, mask_9)), __main_bidder_04))
		__main_bidder_1 = TreeXor32(io, __main_bidder_1_7, __main_bidder_1_10, Mask32(io, Not1(io, TreeXor1(io, mask_7, mask_10)), __main_bidder_1))
		__main_i_01 = TreeXor32(io, __main_i_01_6, __main_i_01_9, Mask32(io, Not1(io, TreeXor1(io, mask_6, mask_9)), __main_i_01))
		__main_penultimate_0_lcssa = TreeXor32(io, __main_penultimate_0_lcssa_5, __main_penultimate_0_lcssa_8, Mask32(io, Not1(io, TreeXor1(io, mask_5, mask_8)), __main_penultimate_0_lcssa))
		__main_penultimate_02 = TreeXor32(io, __main_penultimate_02_6, __main_penultimate_02_9, Mask32(io, Not1(io, TreeXor1(io, mask_6, mask_9)), __main_penultimate_02))
		__main_penultimate_1 = TreeXor32(io, __main_penultimate_1_7, __main_penultimate_1_10, Mask32(io, Not1(io, TreeXor1(io, mask_7, mask_10)), __main_penultimate_1))
		__main_ultimate_03 = TreeXor32(io, __main_ultimate_03_6, __main_ultimate_03_9, Mask32(io, Not1(io, TreeXor1(io, mask_6, mask_9)), __main_ultimate_03))
		__main_ultimate_1 = TreeXor32(io, __main_ultimate_1_7, __main_ultimate_1_10, Mask32(io, Not1(io, TreeXor1(io, mask_7, mask_10)), __main_ultimate_1))
		_block0 = TreeXor1(io, _block0_0, Mask1(io, Not1(io, TreeXor1(io, mask_0)), _block0))
		_block1 = TreeXor1(io, _block1_1, _block1_6, _block1_9, Mask1(io, Not1(io, TreeXor1(io, mask_1, mask_6, mask_9)), _block1))
		_block10 = TreeXor1(io, _block10_2, _block10_10, Mask1(io, Not1(io, TreeXor1(io, mask_2, mask_10)), _block10))
		_block2 = TreeXor1(io, _block2_1, _block2_2, Mask1(io, Not1(io, TreeXor1(io, mask_1, mask_2)), _block2))
		_block3 = TreeXor1(io, _block3_3, _block3_7, _block3_10, Mask1(io, Not1(io, TreeXor1(io, mask_3, mask_7, mask_10)), _block3))
		_block4 = TreeXor1(io, _block4_5, _block4_8, Mask1(io, Not1(io, TreeXor1(io, mask_5, mask_8)), _block4))
		_block5 = TreeXor1(io, _block5_0, _block5_5, Mask1(io, Not1(io, TreeXor1(io, mask_0, mask_5)), _block5))
		_block6 = TreeXor1(io, _block6_0, _block6_6, Mask1(io, Not1(io, TreeXor1(io, mask_0, mask_6)), _block6))
		_block7 = TreeXor1(io, _block7_1, _block7_7, Mask1(io, Not1(io, TreeXor1(io, mask_1, mask_7)), _block7))
		_block8 = TreeXor1(io, _block8_3, _block8_8, Mask1(io, Not1(io, TreeXor1(io, mask_3, mask_8)), _block8))
		_block9 = TreeXor1(io, _block9_3, _block9_9, Mask1(io, Not1(io, TreeXor1(io, mask_3, mask_9)), _block9))
		_vAnswer = TreeXor32(io, _vAnswer_4)
		_vIsDone = TreeXor1(io, _vIsDone_4)

		/* are we done? */
		done = Reveal1(io, _vIsDone)
	}
	answer := Reveal32(io, _vAnswer)
	fmt.Printf("%d: %v\n", io.Id(), answer)
	_main_done <- true
}

// <label>:0
func block0(io Io, ch chan uint64, mask bool) {
	_1 := Input32(io, mask, Uint32(io, 0))
	_2 := NumPeers32(io)
	_3 := Icmp_ugt32(io, _2, Uint32(io, 1))
	_block0 := Uint1(io, 0)
	_block6 := _3
	_block5 := Xor1(io, _3, Uint1(io, 1))
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, _1))
	if Mask1(io, mask, _block0) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block5) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block6) {
		ch <- 1
	} else {
		ch <- 0
	}
}

// <label>:.lr.ph
func block1(io Io, ch chan uint64, mask bool, __main_i_01 uint32, __main_ultimate_03 uint32) {
	_4 := Input32(io, mask, __main_i_01)
	_5 := Icmp_ugt32(io, _4, __main_ultimate_03)
	_block1 := Uint1(io, 0)
	_block7 := _5
	_block2 := Xor1(io, _5, Uint1(io, 1))
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, _4))
	if Mask1(io, mask, _block1) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block2) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block7) {
		ch <- 1
	} else {
		ch <- 0
	}
}

// <label>:6
func block2(io Io, ch chan uint64, mask bool, _4 uint32, __main_penultimate_02 uint32) {
	_7 := Icmp_ugt32(io, _4, __main_penultimate_02)
	___main_penultimate_0 := Select32(io, _7, _4, __main_penultimate_02)
	_block2 := Uint1(io, 0)
	_block10 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, ___main_penultimate_0))
	if Mask1(io, mask, _block10) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block2) {
		ch <- 1
	} else {
		ch <- 0
	}
}

// <label>:8
func block3(io Io, ch chan uint64, mask bool, __main_i_01 uint32) {
	_9 := Add32(io, __main_i_01, Uint32(io, 1))
	_10 := NumPeers32(io)
	_11 := Icmp_ult32(io, _9, _10)
	_block3 := Uint1(io, 0)
	_block9 := _11
	_block8 := Xor1(io, _11, Uint1(io, 1))
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, _9))
	if Mask1(io, mask, _block3) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block8) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block9) {
		ch <- 1
	} else {
		ch <- 0
	}
}

// <label>:._crit_edge
func block4(io Io, ch chan uint64, mask bool, __main_bidder_0_lcssa uint32, __main_penultimate_0_lcssa uint32) {
	Printf(io, mask, "Bidder %d pays %d\n", uint64(__main_bidder_0_lcssa), uint64(__main_penultimate_0_lcssa))
	_vAnswer := Uint32(io, 0)
	_vIsDone := Uint1(io, 1)
	ch <- uint64(Mask32(io, mask, _vAnswer))
	if Mask1(io, mask, _vIsDone) {
		ch <- 1
	} else {
		ch <- 0
	}
}

// <label>:vLabel4
func block5(io Io, ch chan uint64, mask bool) {
	_x22 := Uint32(io, 0)
	_x23 := Uint32(io, 0)
	__main_penultimate_0_lcssa := _x22
	__main_bidder_0_lcssa := _x23
	_block5 := Uint1(io, 0)
	_block4 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_bidder_0_lcssa))
	ch <- uint64(Mask32(io, mask, __main_penultimate_0_lcssa))
	if Mask1(io, mask, _block4) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block5) {
		ch <- 1
	} else {
		ch <- 0
	}
}

// <label>:vLabel1
func block6(io Io, ch chan uint64, mask bool, _1 uint32) {
	_x18 := Uint32(io, 1)
	_x19 := Uint32(io, 0)
	_x20 := _1
	_x21 := Uint32(io, 0)
	__main_i_01 := _x18
	__main_penultimate_02 := _x19
	__main_ultimate_03 := _x20
	__main_bidder_04 := _x21
	_block6 := Uint1(io, 0)
	_block1 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_bidder_04))
	ch <- uint64(Mask32(io, mask, __main_i_01))
	ch <- uint64(Mask32(io, mask, __main_penultimate_02))
	ch <- uint64(Mask32(io, mask, __main_ultimate_03))
	if Mask1(io, mask, _block1) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block6) {
		ch <- 1
	} else {
		ch <- 0
	}
}

// <label>:vLabel2
func block7(io Io, ch chan uint64, mask bool, _4 uint32, __main_i_01 uint32, __main_ultimate_03 uint32) {
	_x15 := __main_i_01
	_x16 := _4
	_x17 := __main_ultimate_03
	__main_bidder_1 := _x15
	__main_ultimate_1 := _x16
	__main_penultimate_1 := _x17
	_block7 := Uint1(io, 0)
	_block3 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_bidder_1))
	ch <- uint64(Mask32(io, mask, __main_penultimate_1))
	ch <- uint64(Mask32(io, mask, __main_ultimate_1))
	if Mask1(io, mask, _block3) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block7) {
		ch <- 1
	} else {
		ch <- 0
	}
}

// <label>:vLabel5
func block8(io Io, ch chan uint64, mask bool, __main_bidder_1 uint32, __main_penultimate_1 uint32) {
	_x13 := __main_penultimate_1
	_x14 := __main_bidder_1
	__main_penultimate_0_lcssa := _x13
	__main_bidder_0_lcssa := _x14
	_block8 := Uint1(io, 0)
	_block4 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_bidder_0_lcssa))
	ch <- uint64(Mask32(io, mask, __main_penultimate_0_lcssa))
	if Mask1(io, mask, _block4) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block8) {
		ch <- 1
	} else {
		ch <- 0
	}
}

// <label>:vLabel0
func block9(io Io, ch chan uint64, mask bool, _9 uint32, __main_bidder_1 uint32, __main_penultimate_1 uint32, __main_ultimate_1 uint32) {
	_x9 := _9
	_x10 := __main_penultimate_1
	_x11 := __main_ultimate_1
	_x12 := __main_bidder_1
	__main_i_01 := _x9
	__main_penultimate_02 := _x10
	__main_ultimate_03 := _x11
	__main_bidder_04 := _x12
	_block9 := Uint1(io, 0)
	_block1 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_bidder_04))
	ch <- uint64(Mask32(io, mask, __main_i_01))
	ch <- uint64(Mask32(io, mask, __main_penultimate_02))
	ch <- uint64(Mask32(io, mask, __main_ultimate_03))
	if Mask1(io, mask, _block1) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block9) {
		ch <- 1
	} else {
		ch <- 0
	}
}

// <label>:vLabel3
func block10(io Io, ch chan uint64, mask bool, ___main_penultimate_0 uint32, __main_bidder_04 uint32, __main_ultimate_03 uint32) {
	_x6 := __main_bidder_04
	_x7 := __main_ultimate_03
	_x8 := ___main_penultimate_0
	__main_bidder_1 := _x6
	__main_ultimate_1 := _x7
	__main_penultimate_1 := _x8
	_block10 := Uint1(io, 0)
	_block3 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_bidder_1))
	ch <- uint64(Mask32(io, mask, __main_penultimate_1))
	ch <- uint64(Mask32(io, mask, __main_ultimate_1))
	if Mask1(io, mask, _block10) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block3) {
		ch <- 1
	} else {
		ch <- 0
	}
}

var _main_done = make(chan bool, 1)

var Handle = MPC{11, blocks_main, _main_done}
