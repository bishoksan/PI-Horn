#include "assert.h"

int k,n;
int main() {
    //k = __VERIFIER_nondet_int();
    //n = __VERIFIER_nondet_int();
    //if (!(n < LARGE_INT)) return 0;
   // if( k == n) {
   // } else {
   //     goto END;
   // }

    for (int i=0;i<n;i++) {
        for (int j=2*i;j<n;j++) {
            if( __VERIFIER_nondet_int() ) {
                for (k=j;k<n;k++) {
                    __VERIFIER_assert(k>=2*i);
                }
            }
            else {
                __VERIFIER_assert( k >= n );
                __VERIFIER_assert( k <= n );
            }
        }
    }
END:
    return 0;
}
