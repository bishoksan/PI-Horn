# 1 "MAP/SAFE-exbench/TRACER-testloop21.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testloop21.tmp.c"
# 25 "MAP/SAFE-exbench/TRACER-testloop21.tmp.c"

int  N,x;
void main(){

  int NONDET;
  int i;
  int a;
  //int x;


  //x=0;
  i=0;


  if (NONDET > 0) a=1; else a=2;

 LOOP:
  if (i<N){
    i=i+1;
    goto LOOP;
  }

  __VERIFIER_assert(!( x >0 ));
  return;
}
