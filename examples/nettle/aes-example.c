/* aes encryption example */
/* LICENSE: LGPL */
/* Compile in nettle (http://www.lysator.liu.se/~nisse/nettle/) source directory with gcc -E */

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
  printf("%.*s\n", AES_BLOCK_SIZE, plaintext);
  printf("%.*s\n", AES_BLOCK_SIZE, ciphertext);
#endif
  return 0;
}
