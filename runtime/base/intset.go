package base

/* Sparse sets of integers 0..N with fast reset, Briggs/Torczon */

type Set struct {
	// Invariant: 0 <= n <= len(dense) == len(sparse)
	dense  []int
	sparse []int
	n      int
}

func New(capacity int) *Set {
	return &Set{make([]int, capacity), make([]int, capacity), 0}
}

func Add(s *Set, X ...int) {
	for _, x := range X {
		if Member(s, x) {
			continue
		}
		if x < 0 || x >= len(s.dense) {
			panic("Error: intset.Add() element out of range")
		}
		s.dense[s.n] = x
		s.sparse[x] = s.n
		s.n++
	}
}

func Member(s *Set, x int) bool {
	if x < 0 || x >= len(s.dense) {
		return false
	}
	return s.sparse[x] < s.n && s.dense[s.sparse[x]] == x
}

func Elements(s *Set) []int {
	return s.dense[:s.n]
}

func Reset(s *Set, x int) {
	s.n = 0
}

func IntersectSlice(s *Set, slice []int) *Set {
	result := New(len(s.dense))
	for _, v := range slice {
		if Member(s, v) {
			Add(result, v)
		}
	}
	return result
}

func Intersect(s1, s2 *Set) *Set {
	if len(s1.dense) > len(s2.dense) { // we expect equal lengths but handle this case anyway
		s1, s2 = s2, s1
	} else if len(s1.dense) == len(s2.dense) && s1.n > s2.n {
		// force s1 to have <= elements
		s1, s2 = s2, s1
	}
	result := New(len(s2.dense)) // capacity = max capacity
	for _, v := range Elements(s1) {
		if Member(s2, v) {
			Add(result, v)
		}
	}
	return result
}
