# 1 "MAP/SAFE-exbench/TRACER-testabs14.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testabs14.tmp.c"
# 27 "MAP/SAFE-exbench/TRACER-testabs14.tmp.c"
int n;
main(){
  int i,j;

  i=0; j=0;
    //n=10;

  while(j < n){
    j++;
  }
  while (i < n){
    i++;
  }

  __VERIFIER_assert(!( i>10 || j>10 ));

}
