# 1 "MAP/UNSAFE-exbench/TRACER-test1-unsafe.tmp.c"
# 1 "<command-line>"
# 1 "MAP/UNSAFE-exbench/TRACER-test1-unsafe.tmp.c"
# 20 "MAP/UNSAFE-exbench/TRACER-test1-unsafe.tmp.c"
//extern int unknown();
int x = 0;
int main(){
  //int x = 0;

  if (unknown()) x = x+1;

  __VERIFIER_assert(!( x > 1 ));

  if (unknown()) x = x+2;

  __VERIFIER_assert(!( x > 3 ));

  if (unknown()) x = x+4;

  __VERIFIER_assert(!( x >= 7 ));
  return x;
}
