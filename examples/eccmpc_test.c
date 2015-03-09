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
#include "uECC.h"

// extern unsigned int input(unsigned int);
// extern unsigned int num_peers();

int main() {
  unsigned char private_key[32] = {0}; 
  uint8_t p_hash[uECC_BYTES] = {0};
  uint8_t p_signature[uECC_BYTES*2];

  for (unsigned int i = 1; i < 5; i++) {
    unsigned char val = i;
    private_key[0] ^= val;
  }
  uECC_sign(private_key, p_hash, p_signature);

  printf("Signature:\n");
  for (unsigned int i = 0; i < 64; i++)
  {
    printf("%x ", p_signature[i]);
  }
  printf("\n");

  return 0;
}
