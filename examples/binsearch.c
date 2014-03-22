/* binary search */

/* return whether v is in sorted array x; n is the number of elements of x */
int search(unsigned int n, unsigned int *x, unsigned int v) {
  unsigned int left = 0;
  unsigned int right = n;
  unsigned int i;
  while (right > left) {
    i = left + (right - left)/2;
    /* invariant: left <= i < right */
    if (x[i] == v)
      return 1;
    else if (x[i] < v)
      left = i+1;
    else /* (x[i] > v) */
      right = i;
  }
  return 0;
}

#include <stdio.h>
unsigned int x1[] = { 1, 2, 3, 4, 5, 6, 7 };
int main() {
  printf("Binary search, expected outputs: 1 then 0\n");
  printf("%d\n", search(7,x1,3));
  printf("%d\n", search(7,x1,10));
  return 0;
}
