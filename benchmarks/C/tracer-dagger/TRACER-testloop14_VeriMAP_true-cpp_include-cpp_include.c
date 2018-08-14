# 1 "/media/sf_share_vm/tracer-ref/TRACER-testloop14_VeriMAP_true-cpp_include.c"
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
# 1 "/media/sf_share_vm/tracer-ref/TRACER-testloop14_VeriMAP_true-cpp_include.c"
# 1 "/media/sf_share_vm/tracer-ref/TRACER-testloop14_VeriMAP_true.c"
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
# 1 "/media/sf_share_vm/tracer-ref/TRACER-testloop14_VeriMAP_true.c"
# 1 "MAP/SAFE-exbench/TRACER-testloop14.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testloop14.tmp.c"
# 27 "MAP/SAFE-exbench/TRACER-testloop14.tmp.c"

int x,y;
int main()
{


  if (y <= 2) {
    if (x < 0) {
      x = 0;
    }
    i = 0;
    while (i < 10) {
      __VERIFIER_assert(!( y > 2 ));
      i++;
    }

    __VERIFIER_assert(!( x <= -1 ));
  }
  return 0;
}
