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
  unsigned int bidder = 0;
  unsigned int ultimate = input(0);
  unsigned int penultimate = 0;

  for (unsigned int i = 1; i < num_peers(); i++) {
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
