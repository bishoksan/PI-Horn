# 1 "MAP/SAFE-exbench/TRACER-testloop16.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testloop16.tmp.c"
# 22 "MAP/SAFE-exbench/TRACER-testloop16.tmp.c"

int N,x;
void main()
{
  int i;

 // x = 1;
  i = 0;

  while (i<N) {
    if (x==1) {
      x=2;
    } else {
      x=1;
    }
    i++;
  }

  __VERIFIER_assert(!( x>2 ));

  return;
}
