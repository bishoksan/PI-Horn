# 1 "/media/sf_share_vm/tracer-ref/MAP-pepm-scp-example2.c_VeriMAP_true.c"
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
# 1 "/media/sf_share_vm/tracer-ref/MAP-pepm-scp-example2.c_VeriMAP_true.c"


int n;
int k;

int incr(int int z)
{
  return k + z;
}

void main() {
    int x, y;
  while (x < n) {
    x = x + 1;
    y=incr(x);
  }

  __VERIFIER_assert( x<=y );

}
