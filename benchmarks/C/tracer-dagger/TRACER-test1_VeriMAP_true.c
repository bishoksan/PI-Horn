# 1 "MAP/SAFE-exbench/TRACER-test1.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-test1.tmp.c"
# 20 "MAP/SAFE-exbench/TRACER-test1.tmp.c"
//extern int unknown();
int x = 0;
void main(){
  //int x = 0;

  if (unknown()) x = x+1;
  if (unknown()) x = x+2;
  if (unknown()) x = x+4;

  __VERIFIER_assert(!( x > 7 ));
  return;
}
