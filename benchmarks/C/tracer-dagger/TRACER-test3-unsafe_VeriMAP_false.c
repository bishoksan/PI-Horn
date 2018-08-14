# 1 "MAP/UNSAFE-exbench/TRACER-test3-unsafe.tmp.c"
# 1 "<command-line>"
# 1 "MAP/UNSAFE-exbench/TRACER-test3-unsafe.tmp.c"
# 22 "MAP/UNSAFE-exbench/TRACER-test3-unsafe.tmp.c"
//extern int unknown();
int x=0;
int y=0;
void main(){
  //int x=0;
  //int y=0;

  if (unknown())
    x = 5;
  else
    y = 10;

  __VERIFIER_assert(!( x==5 || y==10 ));
  return;
}
