#include "assert.h"

int l, n;
int main() {
  //int k,n;

  //n = __VERIFIER_nondet_int();
  //l = __VERIFIER_nondet_int();
  //if (!(l>0)) return 0;
  //if (!(l < LARGE_INT)) return 0;
  //if (!(n < LARGE_INT)) return 0;
  for (int k=1;k<n;k++){
    for (int i=l;i<n;i++){
      __VERIFIER_assert(1<=i);
    }
    if(__VERIFIER_nondet_int())
      l = l + 1;
  }
 }
