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
  unsigned int cur_sum_males = 0;
  unsigned int cur_sum_females = 0;

  for (unsigned int i = 0; i < num_peers(); i++) {
    unsigned int gender = input(i);
    unsigned int salary = input(i);
    if (gender == 1) {
      cur_sum_females += salary;
    } else {
      cur_sum_males += salary;
    }

  }
  printf("%d %d", cur_sum_males, cur_sum_females);
  return 0;
}
