/* ecc signature example */
/* In this directory do

       gcc -U __SIZEOF_INT128__ -E ecc-example.c > ../ecc.c

   to obtain a single C file that you can compile with smpcc.

   Here the "-U __SIZEOF_INT128__" causes uECC.c to be compiled
   without support for 128 bit ints, which CIL cannot handle.

   You must use gcc on Linux b/c on Mac OS X you will encounter CIL
   errors.
*/

#include <stdio.h>
#include "uECC.c"

extern unsigned int input(unsigned int);
extern unsigned int num_peers();

unsigned char private_key[32];
uint8_t p_hash[uECC_BYTES];
uint8_t p_signature[uECC_BYTES*2];

int main() {
  for (unsigned int i = 1; i < num_peers(); i++) {
    unsigned char val = input(i);
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
