/* sha1 example */
/* LICENSE: LGPL */
/* Compile in nettle (http://www.lysator.liu.se/~nisse/nettle/) source directory with gcc -E */

//#define OUTPUT

#include "write-be32.c"
#include "sha1-compress.c"
#include "sha1.c"

struct sha1_ctx ctx;
uint8_t input[3] = { 'a', 'b', 'c' };
uint8_t digest[SHA1_DIGEST_SIZE];

#ifdef OUTPUT
#include <stdio.h>
#endif
int main() {
  sha1_init(&ctx);
  sha1_update(&ctx, 3, input);
  sha1_digest(&ctx, SHA1_DIGEST_SIZE, digest);
#ifdef OUTPUT
  int i;
  printf("Expected: a9993e364706816aba3e25717850c26c9cd0d89d\n");
  printf("  Actual: ");
  for (i = 0; i < SHA1_DIGEST_SIZE; i++) {
    printf("%.2x", digest[i]);
  }
  printf("\n");
#endif
  return 0;
}
