# 1 "/media/sf_share_vm//loops/trex01_true-unreach-call_true-termination.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 31 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 32 "<command-line>" 2
# 1 "/home/bish/Desktop/VeriMAP-linux_x86_64/lib/c2map/utils.h" 1
int __VERIFIER_nondet_int();
long __VERIFIER_nondet_long();
unsigned int __VERIFIER_nondet_uint();
_Bool __VERIFIER_nondet_bool();
void __VERIFIER_assume(int expression);
void __VERIFIER_assert(int cond);
void error(void);
void errorFn(void);
void __VERIFIER_error();
# 32 "<command-line>" 2
# 1 "/media/sf_share_vm//loops/trex01_true-unreach-call_true-termination.c"
extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern void __VERIFIER_assume(int condition);

void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: __VERIFIER_error();
  }
  return;
}
_Bool __VERIFIER_nondet_bool();
int __VERIFIER_nondet_int();



void f(int d) {
  int x = __VERIFIER_nondet_int(), y = __VERIFIER_nondet_int(), k = __VERIFIER_nondet_int();

  L1:
  if (!(k <= 1073741823))
    return;
  while (z < k) { z = 2 * z; }
  __VERIFIER_assert(z>=1);
  L2:
  if (!(x <= 1000000 && x >= -1000000)) return;
  if (!(y <= 1000000 && y >= -1000000)) return;
  if (!(k <= 1000000 && k >= -1000000)) return;
  while (x > 0 && y > 0) {
    _Bool c = __VERIFIER_nondet_bool();
    if (c) {
      P1:
      x = x - d;
      y = __VERIFIER_nondet_bool();;
      z = z - 1;
    } else {
      y = y - d;
    }
  }
}

int z;
int main() {
  _Bool c = __VERIFIER_nondet_bool();
  if (c) {
    f(1);
  } else {
    f(2);
  }

  return 0;
}
