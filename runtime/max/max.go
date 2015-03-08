package max

import . "github.com/tjim/smpcc/runtime/gmw"
import "fmt"

func initialize_ram(io Io) {
	ram := make([]byte, 0x24)
	// @.str at location 0 == 0x0
	ram[0x0] = Uint8(io, 0x50)
	ram[0x1] = Uint8(io, 0x61)
	ram[0x2] = Uint8(io, 0x72)
	ram[0x3] = Uint8(io, 0x74)
	ram[0x4] = Uint8(io, 0x69)
	ram[0x5] = Uint8(io, 0x63)
	ram[0x6] = Uint8(io, 0x69)
	ram[0x7] = Uint8(io, 0x70)
	ram[0x8] = Uint8(io, 0x61)
	ram[0x9] = Uint8(io, 0x6e)
	ram[0xa] = Uint8(io, 0x74)
	ram[0xb] = Uint8(io, 0x20)
	ram[0xc] = Uint8(io, 0x25)
	ram[0xd] = Uint8(io, 0x64)
	ram[0xe] = Uint8(io, 0x20)
	ram[0xf] = Uint8(io, 0x68)
	ram[0x10] = Uint8(io, 0x61)
	ram[0x11] = Uint8(io, 0x64)
	ram[0x12] = Uint8(io, 0x20)
	ram[0x13] = Uint8(io, 0x6d)
	ram[0x14] = Uint8(io, 0x61)
	ram[0x15] = Uint8(io, 0x78)
	ram[0x16] = Uint8(io, 0x20)
	ram[0x17] = Uint8(io, 0x76)
	ram[0x18] = Uint8(io, 0x61)
	ram[0x19] = Uint8(io, 0x6c)
	ram[0x1a] = Uint8(io, 0x75)
	ram[0x1b] = Uint8(io, 0x65)
	ram[0x1c] = Uint8(io, 0x20)
	ram[0x1d] = Uint8(io, 0x25)
	ram[0x1e] = Uint8(io, 0x64)
	ram[0x1f] = Uint8(io, 0xa)
	io.InitRam(ram)
}

func blocks_main(io Io, ios []Io) {

	initialize_ram(io)

	/* create output channels */
	ch0 := make(chan uint64, 4)
	ch1 := make(chan uint64, 6)
	ch2 := make(chan uint64, 2)
	ch3 := make(chan uint64, 4)
	ch4 := make(chan uint64, 4)
	ch5 := make(chan uint64, 5)
	ch6 := make(chan uint64, 5)

	/* special variables */
	_vAnswer := Uint32(io, 0)
	_vIsDone := Uint1(io, 0)

	/* block masks */
	_block0 := Uint1(io, 0)
	_block1 := Uint1(io, 0)
	_block2 := Uint1(io, 0)
	_block3 := Uint1(io, 0)
	_block4 := Uint1(io, 0)
	_block5 := Uint1(io, 0)
	_block6 := Uint1(io, 0)
	_block0 = Uint1(io, 1)

	/* block free variables */
	_1 := Uint32(io, 0)
	_6 := Uint32(io, 0)
	___main_cur_max_0 := Uint32(io, 0)
	__main_cur_max_0_lcssa := Uint32(io, 0)
	__main_cur_max_03 := Uint32(io, 0)
	__main_i_0__main_max_i_0 := Uint32(io, 0)
	__main_i_01 := Uint32(io, 0)
	__main_max_i_0_lcssa := Uint32(io, 0)
	__main_max_i_02 := Uint32(io, 0)

	done := false
	for !done {

		/* one goroutine invocation per block */
		go block0(ios[0], ch0, _block0)
		go block1(ios[1], ch1, _block1, __main_cur_max_03, __main_i_01, __main_max_i_02)
		go block2(ios[2], ch2, _block2, __main_cur_max_0_lcssa, __main_max_i_0_lcssa)
		go block3(ios[3], ch3, _block3, ___main_cur_max_0, __main_i_0__main_max_i_0)
		go block4(ios[4], ch4, _block4, _1)
		go block5(ios[5], ch5, _block5, _1)
		go block6(ios[6], ch6, _block6, _6, ___main_cur_max_0, __main_i_0__main_max_i_0)

		/* mux the outputs*/
		mask_0 := (<-ch0) > 0
		_1_0 := uint32(<-ch0)
		_block0_0 := (<-ch0) > 0
		_block4_0 := (<-ch0) > 0
		_block5_0 := (<-ch0) > 0
		mask_1 := (<-ch1) > 0
		_6_1 := uint32(<-ch1)
		___main_cur_max_0_1 := uint32(<-ch1)
		__main_i_0__main_max_i_0_1 := uint32(<-ch1)
		_block1_1 := (<-ch1) > 0
		_block3_1 := (<-ch1) > 0
		_block6_1 := (<-ch1) > 0
		_vAnswer_2 := uint32(<-ch2)
		_vIsDone_2 := (<-ch2) > 0
		mask_3 := (<-ch3) > 0
		__main_cur_max_0_lcssa_3 := uint32(<-ch3)
		__main_max_i_0_lcssa_3 := uint32(<-ch3)
		_block2_3 := (<-ch3) > 0
		_block3_3 := (<-ch3) > 0
		mask_4 := (<-ch4) > 0
		__main_cur_max_0_lcssa_4 := uint32(<-ch4)
		__main_max_i_0_lcssa_4 := uint32(<-ch4)
		_block2_4 := (<-ch4) > 0
		_block4_4 := (<-ch4) > 0
		mask_5 := (<-ch5) > 0
		__main_cur_max_03_5 := uint32(<-ch5)
		__main_i_01_5 := uint32(<-ch5)
		__main_max_i_02_5 := uint32(<-ch5)
		_block1_5 := (<-ch5) > 0
		_block5_5 := (<-ch5) > 0
		mask_6 := (<-ch6) > 0
		__main_cur_max_03_6 := uint32(<-ch6)
		__main_i_01_6 := uint32(<-ch6)
		__main_max_i_02_6 := uint32(<-ch6)
		_block1_6 := (<-ch6) > 0
		_block6_6 := (<-ch6) > 0
		_1 = TreeXor32(io, _1_0, Mask32(io, Not1(io, TreeXor1(io, mask_0)), _1))
		_6 = TreeXor32(io, _6_1, Mask32(io, Not1(io, TreeXor1(io, mask_1)), _6))
		___main_cur_max_0 = TreeXor32(io, ___main_cur_max_0_1, Mask32(io, Not1(io, TreeXor1(io, mask_1)), ___main_cur_max_0))
		__main_cur_max_0_lcssa = TreeXor32(io, __main_cur_max_0_lcssa_3, __main_cur_max_0_lcssa_4, Mask32(io, Not1(io, TreeXor1(io, mask_3, mask_4)), __main_cur_max_0_lcssa))
		__main_cur_max_03 = TreeXor32(io, __main_cur_max_03_5, __main_cur_max_03_6, Mask32(io, Not1(io, TreeXor1(io, mask_5, mask_6)), __main_cur_max_03))
		__main_i_0__main_max_i_0 = TreeXor32(io, __main_i_0__main_max_i_0_1, Mask32(io, Not1(io, TreeXor1(io, mask_1)), __main_i_0__main_max_i_0))
		__main_i_01 = TreeXor32(io, __main_i_01_5, __main_i_01_6, Mask32(io, Not1(io, TreeXor1(io, mask_5, mask_6)), __main_i_01))
		__main_max_i_0_lcssa = TreeXor32(io, __main_max_i_0_lcssa_3, __main_max_i_0_lcssa_4, Mask32(io, Not1(io, TreeXor1(io, mask_3, mask_4)), __main_max_i_0_lcssa))
		__main_max_i_02 = TreeXor32(io, __main_max_i_02_5, __main_max_i_02_6, Mask32(io, Not1(io, TreeXor1(io, mask_5, mask_6)), __main_max_i_02))
		_block0 = TreeXor1(io, _block0_0, Mask1(io, Not1(io, TreeXor1(io, mask_0)), _block0))
		_block1 = TreeXor1(io, _block1_1, _block1_5, _block1_6, Mask1(io, Not1(io, TreeXor1(io, mask_1, mask_5, mask_6)), _block1))
		_block2 = TreeXor1(io, _block2_3, _block2_4, Mask1(io, Not1(io, TreeXor1(io, mask_3, mask_4)), _block2))
		_block3 = TreeXor1(io, _block3_1, _block3_3, Mask1(io, Not1(io, TreeXor1(io, mask_1, mask_3)), _block3))
		_block4 = TreeXor1(io, _block4_0, _block4_4, Mask1(io, Not1(io, TreeXor1(io, mask_0, mask_4)), _block4))
		_block5 = TreeXor1(io, _block5_0, _block5_5, Mask1(io, Not1(io, TreeXor1(io, mask_0, mask_5)), _block5))
		_block6 = TreeXor1(io, _block6_1, _block6_6, Mask1(io, Not1(io, TreeXor1(io, mask_1, mask_6)), _block6))
		_vAnswer = TreeXor32(io, _vAnswer_2)
		_vIsDone = TreeXor1(io, _vIsDone_2)

		/* are we done? */
		done = Reveal1(io, _vIsDone)
	}
	answer := Reveal32(io, _vAnswer)
	fmt.Printf("%d: %v\n", io.Id(), answer)
}

// <label>:0
func block0(io Io, ch chan uint64, mask bool) {
	_1 := Input32(io, mask, Uint32(io, 0))
	_2 := NumPeers32(io)
	_3 := Icmp_ugt32(io, _2, Uint32(io, 1))
	_block0 := Uint1(io, 0)
	_block5 := _3
	_block4 := Xor1(io, _3, Uint1(io, 1))
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

// <label>:.lr.ph
func block1(io Io, ch chan uint64, mask bool, __main_cur_max_03 uint32, __main_i_01 uint32, __main_max_i_02 uint32) {
	_4 := Input32(io, mask, __main_i_01)
	_5 := Icmp_ugt32(io, _4, __main_cur_max_03)
	__main_i_0__main_max_i_0 := Select32(io, _5, __main_i_01, __main_max_i_02)
	___main_cur_max_0 := Select32(io, _5, _4, __main_cur_max_03)
	_6 := Add32(io, __main_i_01, Uint32(io, 1))
	_7 := NumPeers32(io)
	_8 := Icmp_ult32(io, _6, _7)
	_block1 := Uint1(io, 0)
	_block6 := _8
	_block3 := Xor1(io, _8, Uint1(io, 1))
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, _6))
	ch <- uint64(Mask32(io, mask, ___main_cur_max_0))
	ch <- uint64(Mask32(io, mask, __main_i_0__main_max_i_0))
	if Mask1(io, mask, _block1) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block3) {
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

// <label>:._crit_edge
func block2(io Io, ch chan uint64, mask bool, __main_cur_max_0_lcssa uint32, __main_max_i_0_lcssa uint32) {
	Printf(io, mask, "Participant %d had max value %d\n", uint64(__main_max_i_0_lcssa), uint64(__main_cur_max_0_lcssa))
	_vAnswer := Uint32(io, 0)
	_vIsDone := Uint1(io, 1)
	ch <- uint64(Mask32(io, mask, _vAnswer))
	if Mask1(io, mask, _vIsDone) {
		ch <- 1
	} else {
		ch <- 0
	}
}

// <label>:vLabel3
func block3(io Io, ch chan uint64, mask bool, ___main_cur_max_0 uint32, __main_i_0__main_max_i_0 uint32) {
	_x12 := __main_i_0__main_max_i_0
	_x13 := ___main_cur_max_0
	__main_max_i_0_lcssa := _x12
	__main_cur_max_0_lcssa := _x13
	_block3 := Uint1(io, 0)
	_block2 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_cur_max_0_lcssa))
	ch <- uint64(Mask32(io, mask, __main_max_i_0_lcssa))
	if Mask1(io, mask, _block2) {
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

// <label>:vLabel2
func block4(io Io, ch chan uint64, mask bool, _1 uint32) {
	_x10 := Uint32(io, 0)
	_x11 := _1
	__main_max_i_0_lcssa := _x10
	__main_cur_max_0_lcssa := _x11
	_block4 := Uint1(io, 0)
	_block2 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_cur_max_0_lcssa))
	ch <- uint64(Mask32(io, mask, __main_max_i_0_lcssa))
	if Mask1(io, mask, _block2) {
		ch <- 1
	} else {
		ch <- 0
	}
	if Mask1(io, mask, _block4) {
		ch <- 1
	} else {
		ch <- 0
	}
}

// <label>:vLabel1
func block5(io Io, ch chan uint64, mask bool, _1 uint32) {
	_x7 := Uint32(io, 1)
	_x8 := Uint32(io, 0)
	_x9 := _1
	__main_i_01 := _x7
	__main_max_i_02 := _x8
	__main_cur_max_03 := _x9
	_block5 := Uint1(io, 0)
	_block1 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_cur_max_03))
	ch <- uint64(Mask32(io, mask, __main_i_01))
	ch <- uint64(Mask32(io, mask, __main_max_i_02))
	if Mask1(io, mask, _block1) {
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

// <label>:vLabel0
func block6(io Io, ch chan uint64, mask bool, _6 uint32, ___main_cur_max_0 uint32, __main_i_0__main_max_i_0 uint32) {
	_x4 := _6
	_x5 := __main_i_0__main_max_i_0
	_x6 := ___main_cur_max_0
	__main_i_01 := _x4
	__main_max_i_02 := _x5
	__main_cur_max_03 := _x6
	_block6 := Uint1(io, 0)
	_block1 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_cur_max_03))
	ch <- uint64(Mask32(io, mask, __main_i_01))
	ch <- uint64(Mask32(io, mask, __main_max_i_02))
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

var Handle = MPC{7, blocks_main}
