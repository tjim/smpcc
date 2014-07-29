package main

import . "github.com/tjim/smpcc/runtime/eval"
import "github.com/tjim/smpcc/runtime/base"
import "fmt"

func eval_main(ios []EvalVM) {

	/* create output channels */
	ch0 := make(chan []base.Key, 1)
	ch1 := make(chan []base.Key, 1)
	ch2 := make(chan []base.Key, 5)
	ch3 := make(chan []base.Key, 2)
	ch4 := make(chan []base.Key, 3)
	ch5 := make(chan []base.Key, 1)
	ch6 := make(chan []base.Key, 2)
	ch7 := make(chan []base.Key, 3)
	ch8 := make(chan []base.Key, 2)
	ch9 := make(chan []base.Key, 3)
	ch10 := make(chan []base.Key, 3)
	ch11 := make(chan []base.Key, 3)
	ch12 := make(chan []base.Key, 2)

	/* special variables */
	_attsrcAnswer := Uint(ios[0], 0, 32)
	_attsrcIsDone := Uint(ios[0], 0, 1)
	_attsrcMemAct := Uint(ios[0], 0, 2)
	_attsrcMemLoc := Uint(ios[0], 0, 64)
	_attsrcMemSize := Uint(ios[0], 0, 32)
	_attsrcStateO := Uint(ios[0], 0, 32)

	/* block free variables */
	_5 := Uint(ios[0], 0, 32)
	_8 := Uint(ios[0], 0, 32)
	___search_left_0 := Uint(ios[0], 0, 32)
	__search___retres7_0 := Uint(ios[0], 0, 32)
	__search_left_01 := Uint(ios[0], 0, 32)
	__search_ret_site_0 := Uint(ios[0], 0, 32)
	__search_right_0_ := Uint(ios[0], 0, 32)
	__search_right_02 := Uint(ios[0], 0, 32)
	__search_v_0 := Uint(ios[0], 0, 32)
	_attsrcMemRes := Uint(ios[0], 0, 64)

	done := false
	for !done {

		/* one goroutine invocation per block */
		go eval0(ios[1], ch0, _attsrcStateO)
		go eval1(ios[2], ch1, _attsrcStateO)
		go eval2(ios[3], ch2, _attsrcStateO, __search_left_01, __search_right_02)
		go eval3(ios[4], ch3, _attsrcStateO, __search_v_0, _attsrcMemRes)
		go eval4(ios[5], ch4, _attsrcStateO, _5, _8, __search_left_01, __search_right_02, __search_v_0)
		go eval5(ios[6], ch5, _attsrcStateO, __search___retres7_0, __search_ret_site_0)
		go eval6(ios[7], ch6, _attsrcStateO)
		go eval7(ios[8], ch7, _attsrcStateO, ___search_left_0, __search_right_0_)
		go eval8(ios[9], ch8, _attsrcStateO)
		go eval9(ios[10], ch9, _attsrcStateO)
		go eval10(ios[11], ch10, _attsrcStateO)
		go eval11(ios[12], ch11, _attsrcStateO)
		go eval12(ios[13], ch12, _attsrcStateO)

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
		_5 = Select(ios[0], TreeXor(ios[0], mask_2), TreeXor(ios[0], _5_2), _5)
		_8 = Select(ios[0], TreeXor(ios[0], mask_3), TreeXor(ios[0], _8_3), _8)
		___search_left_0 = Select(ios[0], TreeXor(ios[0], mask_4), TreeXor(ios[0], ___search_left_0_4), ___search_left_0)
		__search___retres7_0 = Select(ios[0], TreeXor(ios[0], mask_8, mask_12), TreeXor(ios[0], __search___retres7_0_8, __search___retres7_0_12), __search___retres7_0)
		__search_left_01 = Select(ios[0], TreeXor(ios[0], mask_7, mask_9), TreeXor(ios[0], __search_left_01_7, __search_left_01_9), __search_left_01)
		__search_ret_site_0 = Select(ios[0], TreeXor(ios[0], mask_10, mask_11), TreeXor(ios[0], __search_ret_site_0_10, __search_ret_site_0_11), __search_ret_site_0)
		__search_right_0_ = Select(ios[0], TreeXor(ios[0], mask_4), TreeXor(ios[0], __search_right_0__4), __search_right_0_)
		__search_right_02 = Select(ios[0], TreeXor(ios[0], mask_7, mask_9), TreeXor(ios[0], __search_right_02_7, __search_right_02_9), __search_right_02)
		__search_v_0 = Select(ios[0], TreeXor(ios[0], mask_10, mask_11), TreeXor(ios[0], __search_v_0_10, __search_v_0_11), __search_v_0)
		_attsrcAnswer = TreeXor(ios[0], _attsrcAnswer_6)
		_attsrcIsDone = TreeXor(ios[0], _attsrcIsDone_6)
		_attsrcMemAct = TreeXor(ios[0], _attsrcMemAct_2)
		_attsrcMemLoc = TreeXor(ios[0], _attsrcMemLoc_2)
		_attsrcMemSize = TreeXor(ios[0], _attsrcMemSize_2)
		_attsrcStateO = TreeXor(ios[0], _attsrcStateO_0, _attsrcStateO_1, _attsrcStateO_2, _attsrcStateO_3, _attsrcStateO_4, _attsrcStateO_5, _attsrcStateO_7, _attsrcStateO_8, _attsrcStateO_9, _attsrcStateO_10, _attsrcStateO_11, _attsrcStateO_12)

		/* load from memory if necessary */
		if Reveal(ios[0], Icmp_eq(ios[0], _attsrcMemAct, Uint(ios[0], 1, 2)))[0] {
			_attsrcMemRes = Load(ios[0], _attsrcMemLoc, _attsrcMemSize)
		}

		/* are we done? */
		done = Reveal(ios[0], _attsrcIsDone)[0]
	}
	answer := RevealInt32(ios[0], _attsrcAnswer)
	fmt.Printf("eval: %v\n", answer)
	_main_done <- true
}

// <label>:0
func eval0(io EvalVM, ch chan []base.Key, block_num []base.Key) {
	mask := Icmp_eq(io, block_num, Uint(io, 0, 32))
	Printf(io, mask, "Binary search, expected outputs: 1 then 0\n")
	_attsrcStateO := Uint(io, 10, 32)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:1
func eval1(io EvalVM, ch chan []base.Key, block_num []base.Key) {
	mask := Icmp_eq(io, block_num, Uint(io, 1, 32))
	_attsrcStateO := Uint(io, 9, 32)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:2
func eval2(io EvalVM, ch chan []base.Key, block_num, __search_left_01, __search_right_02 []base.Key) {
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
func eval3(io EvalVM, ch chan []base.Key, block_num, __search_v_0, _attsrcMemRes []base.Key) {
	mask := Icmp_eq(io, block_num, Uint(io, 3, 32))
	_8 := _attsrcMemRes[:32]
	_9 := Icmp_eq(io, _8, __search_v_0)
	_attsrcStateO := Select(io, _9, Uint(io, 12, 32), Uint(io, 4, 32))
	ch <- mask
	ch <- Mask(io, mask, _8)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:10
func eval4(io EvalVM, ch chan []base.Key, block_num, _5, _8, __search_left_01, __search_right_02, __search_v_0 []base.Key) {
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
func eval5(io EvalVM, ch chan []base.Key, block_num, __search___retres7_0, __search_ret_site_0 []base.Key) {
	mask := Icmp_eq(io, block_num, Uint(io, 5, 32))
	_cond := Icmp_eq(io, __search_ret_site_0, Int(io, 0, 32))
	Printf(io, mask, "%d\n", __search___retres7_0)
	_attsrcStateO := Select(io, _cond, Uint(io, 11, 32), Uint(io, 6, 32))
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:16
func eval6(io EvalVM, ch chan []base.Key, block_num []base.Key) {
	mask := Icmp_eq(io, block_num, Uint(io, 6, 32))
	_attsrcAnswer := Int(io, 0, 32)
	_attsrcIsDone := Int(io, 1, 1)
	ch <- Mask(io, mask, _attsrcAnswer)
	ch <- Mask(io, mask, _attsrcIsDone)
}

// <label>:attsrcLabel3
func eval7(io EvalVM, ch chan []base.Key, block_num, ___search_left_0, __search_right_0_ []base.Key) {
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
func eval8(io EvalVM, ch chan []base.Key, block_num []base.Key) {
	mask := Icmp_eq(io, block_num, Uint(io, 8, 32))
	_x13 := Int(io, 0, 32)
	__search___retres7_0 := _x13
	_attsrcStateO := Uint(io, 5, 32)
	ch <- mask
	ch <- Mask(io, mask, __search___retres7_0)
	ch <- Mask(io, mask, _attsrcStateO)
}

// <label>:attsrcLabel2
func eval9(io EvalVM, ch chan []base.Key, block_num []base.Key) {
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
func eval10(io EvalVM, ch chan []base.Key, block_num []base.Key) {
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
func eval11(io EvalVM, ch chan []base.Key, block_num []base.Key) {
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
func eval12(io EvalVM, ch chan []base.Key, block_num []base.Key) {
	mask := Icmp_eq(io, block_num, Uint(io, 12, 32))
	_x6 := Int(io, 1, 32)
	__search___retres7_0 := _x6
	_attsrcStateO := Uint(io, 5, 32)
	ch <- mask
	ch <- Mask(io, mask, __search___retres7_0)
	ch <- Mask(io, mask, _attsrcStateO)
}

