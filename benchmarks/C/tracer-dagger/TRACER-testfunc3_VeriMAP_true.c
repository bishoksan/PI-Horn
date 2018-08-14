# 1 "MAP/SAFE-exbench/TRACER-testfunc3.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testfunc3.tmp.c"
# 20 "MAP/SAFE-exbench/TRACER-testfunc3.tmp.c"
int foo(int a){

  int b;
  b = a+1;
  return b;
}

int init;
void main(){

  int x,y,z;

  //init=8;
  x= foo(init+1);
  y= foo(x+2);
  z= foo(y+3);

  __VERIFIER_assert(!( z != 17 ));
}
