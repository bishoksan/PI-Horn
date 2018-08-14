# 1 "MAP/SAFE-exbench/TRACER-testwp15.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testwp15.tmp.c"
# 22 "MAP/SAFE-exbench/TRACER-testwp15.tmp.c"
int z;



int foo(){
  int y1;
  if (y1>0) {
   z = 3;
  }
  else {
    y1 = 1;
  }
  return y1;
}

int x;
main()
{

  int y;

  if (x>0) {
    z = 2;
  }
  else {
    x = 0;
  }

  y= foo();

  __VERIFIER_assert(!( x+y<=0 ));
}
