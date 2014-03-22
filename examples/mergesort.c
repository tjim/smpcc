/* Mergesort, adapted from http://en.wikipedia.org/wiki/Merge_sort */

unsigned int min(unsigned int x, unsigned int y) {
  if (x < y) return x;
  return y;
}

void BottomUpMerge(unsigned int A[], unsigned int iLeft, unsigned int iRight, unsigned int iEnd, unsigned int B[]) {
  unsigned int i0 = iLeft;
  unsigned int i1 = iRight;
  unsigned int j;

  /* While there are elements in the left or right lists */
  for (j = iLeft; j < iEnd; j++) {
    /* If left list head exists and is <= existing right list head */
    if (i0 < iRight && (i1 >= iEnd || A[i0] <= A[i1])) {
      B[j] = A[i0];
      i0 = i0 + 1;
    }
    else {
      B[j] = A[i1];
      i1 = i1 + 1;
    }
  }
}

/* BottomUpSort
   A and B are arrays of length n
   On entry, A has the items to sort, and B is a work array
   On exit,
   the result is in A if n is an even power of 2 (1, 4, 16, ...)
   the result is in B if n is an  odd power of 2 (2, 8, 32, ...)
*/
void BottomUpSort(unsigned int n, unsigned int A[], unsigned int B[]) {
  unsigned int width;

  /* Each 1-element run in A is already "sorted". */

  /* Make successively longer sorted runs of length 2, 4, 8, 16... until whole array is sorted. */
  for (width = 1; width < n; width = 2 * width) {
    unsigned int i;

    /* Array A is full of runs of length width. */
    for (i = 0; i < n; i = i + 2 * width) {
      /* Merge two runs: A[i:i+width-1] and A[i+width:i+2*width-1] to B[] */
      /* or copy A[i:n-1] to B[] ( if(i+width >= n) ) */
      BottomUpMerge(A, i, min(i+width, n), min(i+2*width, n), B);
    }

    width = 2 * width;
    if (width >= n) break;

    /* Array B is full of runs of length width. */
    for (i = 0; i < n; i = i + 2 * width) {
      BottomUpMerge(B, i, min(i+width, n), min(i+2*width, n), A);
    }
  }
}

unsigned int a[] = {6,3,2,9};
unsigned int b[] = {0,0,0,0};

#include <stdio.h>
int main() {
  unsigned int n = 4;
  unsigned int i;
  printf("Merge sort, expected outputs: 2, 3, 6, 9\n");
  BottomUpSort(n,a,b);
  for (i = 0; i < n; i++) {
    printf("%d\n", a[i]);
  }
  return 0;
}
