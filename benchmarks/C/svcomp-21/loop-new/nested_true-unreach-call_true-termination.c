#include "assert.h"
int k, n, m ;
int main() {
    //int n = __VERIFIER_nondet_int();
    //int m = __VERIFIER_nondet_int();
    int i,j;
    if (!(10 <= n && n <= 10000)) return 0;
    if (!(10 <= m && m <= 10000)) return 0;
    for (i = 0; i < n; i++) {
	for (j = 0; j < m; j++) {
	    k ++;
	}
    }
    __VERIFIER_assert(k >= 100);
    return 0;
}
