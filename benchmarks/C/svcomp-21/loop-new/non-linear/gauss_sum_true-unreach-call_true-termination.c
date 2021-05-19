#include "assert.h"
int n;
int main() {
    int  i;
    //n = __VERIFIER_nondet_int();
    if (!(1 <= n && n <= 1000)) return 0;
    int sum = 0;
    for(i = 1; i <= n; i++) {
        sum = sum + i;
    }
    __VERIFIER_assert(2*sum == n*(n+1));
    return 0;
}
