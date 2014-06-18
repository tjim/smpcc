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
#define NUM_PARTIES 3
#include <stdio.h>
#ifdef TEST
unsigned int foo = 4;
unsigned int input(unsigned int p) { // returns 4, 5, 6 so bidder 2 should pay 5
  return foo++;
}
#else
extern unsigned int input(unsigned int);
#endif
int main() {
  unsigned int bidder = 0;
  unsigned int ultimate = input(0);
  unsigned int penultimate = 0;

  for (unsigned int i = 1; i < NUM_PARTIES; i++) {
    unsigned int bid = input(i);
    if (bid > ultimate) {
      bidder = i;
      penultimate = ultimate;
      ultimate = bid;
    }
    else if (bid > penultimate) {
      penultimate = bid;
    }
  }
  printf("Bidder %d pays %d\n", bidder, penultimate);
  return 0;
}
