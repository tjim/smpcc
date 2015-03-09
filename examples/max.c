/* Second price (Vickrey) auction
   To compile
       smpcc vickrey.c -circuitlib gmw
   To run
       go run *.go 4 5 6
   where 4, 5, 6 are the bids -- choose your own

   To test the C code
       clang vickrey.c -DTEST
       ./a.out
*/
#include <stdio.h>

extern unsigned int input(unsigned int);
extern unsigned int num_peers();

int main() {
  unsigned int cur_max = input(0);
  unsigned int max_i = 0;

  for (unsigned int i = 1; i < num_peers(); i++) {
    unsigned int val = input(i);
    if (val > cur_max) {
      cur_max = val;
      max_i = i;
    }
  }
  printf("Participant %d had max value %d\n", max_i, cur_max);
  return 0;
}
