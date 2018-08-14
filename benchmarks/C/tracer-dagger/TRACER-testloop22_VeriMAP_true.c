# 1 "MAP/SAFE-exbench/TRACER-testloop22.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testloop22.tmp.c"
# 25 "MAP/SAFE-exbench/TRACER-testloop22.tmp.c"
int  N, NONDET;
void main(){

  //int NONDET;
  int i;
  int a;
  int x;

  if (NONDET > 0) x=1; else x=2;

 LOOP:
  if (i<N){
    i=i+1;
    goto LOOP;
  }

  __VERIFIER_assert(!( x >1 ));
  return;
}
