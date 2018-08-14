#include "assert.h"
 int i;
int main() {
    for (i = 0; i < LARGE_INT; i++) ;
    __VERIFIER_assert(i == LARGE_INT);
    return 0;
}
