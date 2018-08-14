# 1 "MAP/SAFE-exbench/TRACER-testwp14.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testwp14.tmp.c"
# 20 "MAP/SAFE-exbench/TRACER-testwp14.tmp.c"
int x;
main(){

  int y,z;

  if (x>4)
    z=4;
  else
    x=4;

  __VERIFIER_assume( x>0 );

  y=x;

  __VERIFIER_assert(y > 4);

}
