# 1 "MAP/SAFE-exbench/TRACER-testloop10.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testloop10.tmp.c"
# 20 "MAP/SAFE-exbench/TRACER-testloop10.tmp.c"
//extern int unknown();

int lock;
void main()
{
  int  old=0, new=0;

  //lock=0;
  new=old+1;

  while (new != old) {
    //lock = 1;
    old = new;
    if (unknown()) {
      lock = 0;
      new++;
      lock = 1;  
    }
  }

  __VERIFIER_assert(!( lock==0 ));
  return;
}
