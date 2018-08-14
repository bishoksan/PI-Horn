// Source: Sumit Gulwani, Nebosja Jojic: "Program Verification as
// Probabilistic Inference", POPL 2007.

#include "assert.h"
 int m,n = 0;
int main() {
    int x = 0;
    // n = __VERIFIER_nondet_int();
    while(x < n) {
	if(__VERIFIER_nondet_int()) {
	    m = x;
	}
	x = x + 1;
    }
    __VERIFIER_assert((m >= 0 || n <= 0));
    __VERIFIER_assert((m < n || n <= 0));
    return 0;
}
