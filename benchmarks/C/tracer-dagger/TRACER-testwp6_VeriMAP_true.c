# 1 "MAP/SAFE-exbench/TRACER-testwp6.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testwp6.tmp.c"
# 21 "MAP/SAFE-exbench/TRACER-testwp6.tmp.c"
int y;
void main(){
  int x;

  if (y>0)
    x=1;
  else
    x=47;

  if (x>0)
    x=x+3;
  else
    x=x+5;

  __VERIFIER_assert(!( x > 40 ));

}
