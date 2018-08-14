# 1 "MAP/SAFE-exbench/TRACER-testloop15.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testloop15.tmp.c"
# 32 "MAP/SAFE-exbench/TRACER-testloop15.tmp.c"

int N;
int main()
{
  int i = 0;
  //int N = 100;

  while (i<N) {
    i++;
  }

  __VERIFIER_assert(!( i>N ));

  return 0;
}
