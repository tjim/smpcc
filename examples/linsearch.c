/* Linear search */

/* return whether v is in sorted array x; n is the number of elements of x */
int search(unsigned int n, unsigned int *x, unsigned int v) {
  unsigned int i = 0;
  while (i<n) {
    if (x[i] == v) return 1;
    i++;
  }
  return 0;
}

#include <stdio.h>
unsigned int x1[] = { 1, 2, 3, 4, 5, 6, 7 };
int main() {
  printf("Linear search, expected outputs: 1 then 0\n");
  printf("%d\n", search(7,x1,3));
  printf("%d\n", search(7,x1,10));
  return 0;
}
