# 1 "MAP/SAFE-exbench/TRACER-testloop17.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testloop17.tmp.c"
# 23 "MAP/SAFE-exbench/TRACER-testloop17.tmp.c"

int N;
int main()
{
  int i, j, k;

  i = 0;
  j = 0;
  k = 0;

 // __VERIFIER_assume( N > 1 );

  while (i < N) {
    if (i<1)
      k = 1;
    else
      k = 0;
    j++;
    i++;
  }

  __VERIFIER_assert(!( k>0 ));

  return 0;
}
