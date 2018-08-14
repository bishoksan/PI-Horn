# 1 "MAP/SAFE-exbench/TRACER-testabs15.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testabs15.tmp.c"
# 28 "MAP/SAFE-exbench/TRACER-testabs15.tmp.c"
int n, a, b;
main(){
  int i;
  int TRACER_NONDET;

  if(n >=0){

    i=0;
    //a=0; b=0;

    while (i < n){
      if (TRACER_NONDET){
 a=a+1;
 b=b+2;
      }
      else{
 a=a+2;
 b=b+1;
      }
      i++;
    }
    __VERIFIER_assert(!( a+b != 3*n ));
  }
}
