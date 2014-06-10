/* aes encryption example */
/* LICENSE: LGPL */
/* Compile in nettle (http://www.lysator.liu.se/~nisse/nettle/) source directory with gcc -E */
/* Must use gcc on Linux b/c on Mac OS X you will encounter CIL errors */

//#define OUTPUT

#include "aes-encrypt.c"
#include "aes-encrypt-table.c"
#include "aes-encrypt-internal.c"
#include "aes-set-encrypt-key.c"

unsigned char plaintext[AES_BLOCK_SIZE] = "0123456789abcdef";
unsigned char ciphertext[AES_BLOCK_SIZE];

struct aes_ctx ctx;
unsigned char key[AES_BLOCK_SIZE];

#ifdef OUTPUT
#include <stdio.h>
#endif
int main() {
  aes_set_encrypt_key(&ctx, AES_BLOCK_SIZE, key);
  aes_encrypt(&ctx, AES_BLOCK_SIZE, ciphertext, plaintext);
#ifdef OUTPUT
  printf("Expected: 14f5fe746966f292651c2288bbff4609\n");
  printf("Actual:   ");
  int i;
  for (i = 0; i < 16; i++) {
    printf("%02x", ciphertext[i]);
  }
  printf("\n");
#endif
  return 0;
}
