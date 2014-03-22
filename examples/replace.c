/* compile with
       clang -S -emit-llvm -O1 replace.c
   to produce replace.s
*/

typedef struct {
  unsigned int key;
  unsigned int val;
} keyval;

/* if k is a key in sorted keyval array x, replace its value with v and return 1
   if k is not a key in x, return 0 */
int replace(unsigned int n, keyval *x, unsigned int k, unsigned int v) {
  unsigned int left = 0;
  unsigned int right = n;
  unsigned int i;
  while (right > left) {
    i = left + (right - left)/2;
    /* invariant: left <= i < right */
    if (x[i].key == k) {
      x[i].val = v;
      return 1;
    }
    else if (x[i].key < k)
      left = i+1;
    else /* (x[i].key > k) */
      right = i;
  }
  return 0;
}

#include <stdio.h>
keyval x1[] = { {1, 10}, {2, 20}, {3, 30}, {4, 40}, {5, 50}, {6, 60}, {7, 70} };
int main() {
  int r = replace(7,x1,3,999);
  printf("%d, %d\n", r, x1[2].val); /* 2 b/c 0-indexed */
  return 0;
}
