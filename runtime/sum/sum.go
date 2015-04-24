package sum

import . "github.com/tjim/smpcc/runtime/gmw"
import "fmt"

func initialize_ram(io Io) {
	ram := make([]byte, 0x8)
	// @.str at location 0 == 0x0
	ram[0x0] = Uint8(io, 0x25)
	ram[0x1] = Uint8(io, 0x64)
	ram[0x2] = Uint8(io, 0x20)
	ram[0x3] = Uint8(io, 0x25)
	ram[0x4] = Uint8(io, 0x64)
	io.InitRam(ram)
}

func blocks_main(io Io, ios []Io) {

	initialize_ram(io)

	/* create output channels */
	ch0 := make(chan uint64, 3)
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
	_8 := Uint32(io, 0)
	__main_cur_sum_females_0_lcssa := Uint32(io, 0)
	__main_cur_sum_females_02 := Uint32(io, 0)
	__main_cur_sum_females_1 := Uint32(io, 0)
	__main_cur_sum_males_0_lcssa := Uint32(io, 0)
	__main_cur_sum_males_03 := Uint32(io, 0)
	__main_cur_sum_males_1 := Uint32(io, 0)
	__main_i_01 := Uint32(io, 0)

	done := false
	for !done {

		/* one goroutine invocation per block */
		go block0(ios[0], ch0, _block0)
		go block1(ios[1], ch1, _block1, __main_cur_sum_females_02, __main_cur_sum_males_03, __main_i_01)
		go block2(ios[2], ch2, _block2, __main_cur_sum_females_0_lcssa, __main_cur_sum_males_0_lcssa)
		go block3(ios[3], ch3, _block3, __main_cur_sum_females_1, __main_cur_sum_males_1)
		go block4(ios[4], ch4, _block4)
		go block5(ios[5], ch5, _block5)
		go block6(ios[6], ch6, _block6, _8, __main_cur_sum_females_1, __main_cur_sum_males_1)

		/* mux the outputs*/
		mask_0 := (<-ch0) > 0
		_block0_0 := (<-ch0) > 0
		_block4_0 := (<-ch0) > 0
		_block5_0 := (<-ch0) > 0
		mask_1 := (<-ch1) > 0
		_8_1 := uint32(<-ch1)
		__main_cur_sum_females_1_1 := uint32(<-ch1)
		__main_cur_sum_males_1_1 := uint32(<-ch1)
		_block1_1 := (<-ch1) > 0
		_block3_1 := (<-ch1) > 0
		_block6_1 := (<-ch1) > 0
		_vAnswer_2 := uint32(<-ch2)
		_vIsDone_2 := (<-ch2) > 0
		mask_3 := (<-ch3) > 0
		__main_cur_sum_females_0_lcssa_3 := uint32(<-ch3)
		__main_cur_sum_males_0_lcssa_3 := uint32(<-ch3)
		_block2_3 := (<-ch3) > 0
		_block3_3 := (<-ch3) > 0
		mask_4 := (<-ch4) > 0
		__main_cur_sum_females_0_lcssa_4 := uint32(<-ch4)
		__main_cur_sum_males_0_lcssa_4 := uint32(<-ch4)
		_block2_4 := (<-ch4) > 0
		_block4_4 := (<-ch4) > 0
		mask_5 := (<-ch5) > 0
		__main_cur_sum_females_02_5 := uint32(<-ch5)
		__main_cur_sum_males_03_5 := uint32(<-ch5)
		__main_i_01_5 := uint32(<-ch5)
		_block1_5 := (<-ch5) > 0
		_block5_5 := (<-ch5) > 0
		mask_6 := (<-ch6) > 0
		__main_cur_sum_females_02_6 := uint32(<-ch6)
		__main_cur_sum_males_03_6 := uint32(<-ch6)
		__main_i_01_6 := uint32(<-ch6)
		_block1_6 := (<-ch6) > 0
		_block6_6 := (<-ch6) > 0
		_8 = TreeXor32(io, _8_1, Mask32(io, Not1(io, TreeXor1(io, mask_1)), _8))
		__main_cur_sum_females_0_lcssa = TreeXor32(io, __main_cur_sum_females_0_lcssa_3, __main_cur_sum_females_0_lcssa_4, Mask32(io, Not1(io, TreeXor1(io, mask_3, mask_4)), __main_cur_sum_females_0_lcssa))
		__main_cur_sum_females_02 = TreeXor32(io, __main_cur_sum_females_02_5, __main_cur_sum_females_02_6, Mask32(io, Not1(io, TreeXor1(io, mask_5, mask_6)), __main_cur_sum_females_02))
		__main_cur_sum_females_1 = TreeXor32(io, __main_cur_sum_females_1_1, Mask32(io, Not1(io, TreeXor1(io, mask_1)), __main_cur_sum_females_1))
		__main_cur_sum_males_0_lcssa = TreeXor32(io, __main_cur_sum_males_0_lcssa_3, __main_cur_sum_males_0_lcssa_4, Mask32(io, Not1(io, TreeXor1(io, mask_3, mask_4)), __main_cur_sum_males_0_lcssa))
		__main_cur_sum_males_03 = TreeXor32(io, __main_cur_sum_males_03_5, __main_cur_sum_males_03_6, Mask32(io, Not1(io, TreeXor1(io, mask_5, mask_6)), __main_cur_sum_males_03))
		__main_cur_sum_males_1 = TreeXor32(io, __main_cur_sum_males_1_1, Mask32(io, Not1(io, TreeXor1(io, mask_1)), __main_cur_sum_males_1))
		__main_i_01 = TreeXor32(io, __main_i_01_5, __main_i_01_6, Mask32(io, Not1(io, TreeXor1(io, mask_5, mask_6)), __main_i_01))
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
	_1 := NumPeers32(io)
	_2 := Icmp_eq32(io, _1, Uint32(io, 0))
	_block0 := Uint1(io, 0)
	_block4 := _2
	_block5 := Xor1(io, _2, Uint1(io, 1))
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
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
func block1(io Io, ch chan uint64, mask bool, __main_cur_sum_females_02 uint32, __main_cur_sum_males_03 uint32, __main_i_01 uint32) {
	_3 := Input32(io, mask, __main_i_01)
	_4 := Input32(io, mask, __main_i_01)
	_5 := Icmp_eq32(io, _3, Uint32(io, 1))
	_6 := Select32(io, _5, _4, Uint32(io, 0))
	__main_cur_sum_females_1 := Add32(io, _6, __main_cur_sum_females_02)
	_7 := Select32(io, _5, Uint32(io, 0), _4)
	__main_cur_sum_males_1 := Add32(io, _7, __main_cur_sum_males_03)
	_8 := Add32(io, __main_i_01, Uint32(io, 1))
	_9 := NumPeers32(io)
	_10 := Icmp_ult32(io, _8, _9)
	_block1 := Uint1(io, 0)
	_block6 := _10
	_block3 := Xor1(io, _10, Uint1(io, 1))
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, _8))
	ch <- uint64(Mask32(io, mask, __main_cur_sum_females_1))
	ch <- uint64(Mask32(io, mask, __main_cur_sum_males_1))
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
func block2(io Io, ch chan uint64, mask bool, __main_cur_sum_females_0_lcssa uint32, __main_cur_sum_males_0_lcssa uint32) {
	Printf(io, mask, "%d %d", uint64(__main_cur_sum_males_0_lcssa), uint64(__main_cur_sum_females_0_lcssa))
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
func block3(io Io, ch chan uint64, mask bool, __main_cur_sum_females_1 uint32, __main_cur_sum_males_1 uint32) {
	_x12 := __main_cur_sum_females_1
	_x13 := __main_cur_sum_males_1
	__main_cur_sum_females_0_lcssa := _x12
	__main_cur_sum_males_0_lcssa := _x13
	_block3 := Uint1(io, 0)
	_block2 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_cur_sum_females_0_lcssa))
	ch <- uint64(Mask32(io, mask, __main_cur_sum_males_0_lcssa))
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
func block4(io Io, ch chan uint64, mask bool) {
	_x10 := Uint32(io, 0)
	_x11 := Uint32(io, 0)
	__main_cur_sum_females_0_lcssa := _x10
	__main_cur_sum_males_0_lcssa := _x11
	_block4 := Uint1(io, 0)
	_block2 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_cur_sum_females_0_lcssa))
	ch <- uint64(Mask32(io, mask, __main_cur_sum_males_0_lcssa))
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
func block5(io Io, ch chan uint64, mask bool) {
	_x7 := Uint32(io, 0)
	_x8 := Uint32(io, 0)
	_x9 := Uint32(io, 0)
	__main_i_01 := _x7
	__main_cur_sum_females_02 := _x8
	__main_cur_sum_males_03 := _x9
	_block5 := Uint1(io, 0)
	_block1 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_cur_sum_females_02))
	ch <- uint64(Mask32(io, mask, __main_cur_sum_males_03))
	ch <- uint64(Mask32(io, mask, __main_i_01))
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
func block6(io Io, ch chan uint64, mask bool, _8 uint32, __main_cur_sum_females_1 uint32, __main_cur_sum_males_1 uint32) {
	_x4 := _8
	_x5 := __main_cur_sum_females_1
	_x6 := __main_cur_sum_males_1
	__main_i_01 := _x4
	__main_cur_sum_females_02 := _x5
	__main_cur_sum_males_03 := _x6
	_block6 := Uint1(io, 0)
	_block1 := Uint1(io, 1)
	if mask {
		ch <- 1
	} else {
		ch <- 0
	}
	ch <- uint64(Mask32(io, mask, __main_cur_sum_females_02))
	ch <- uint64(Mask32(io, mask, __main_cur_sum_males_03))
	ch <- uint64(Mask32(io, mask, __main_i_01))
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
