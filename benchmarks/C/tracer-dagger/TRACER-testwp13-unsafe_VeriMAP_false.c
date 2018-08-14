# 1 "MAP/SAFE-exbench/TRACER-testwp13.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testwp13.tmp.c"
# 20 "MAP/SAFE-exbench/TRACER-testwp13.tmp.c"
int x, y;
main(){

  //int x,y;

  if (x>5){
    if (y>0)
      y++;
    else
      x=x-10;

    if (x<50){
      __VERIFIER_assert(!( x < 0 ));
    }
  }
}
