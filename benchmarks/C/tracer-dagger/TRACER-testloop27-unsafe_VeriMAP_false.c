# 1 "MAP/SAFE-exbench/TRACER-testloop27.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testloop27.tmp.c"
# 22 "MAP/SAFE-exbench/TRACER-testloop27.tmp.c"
int NONDET;
main(){
  int i,n,x;

  i=0;x=0;
  while (i < 1){
     x++;
    if (NONDET > 0 ){
      x--;
    }
    i++;
  }

  __VERIFIER_assert(!( x !=0 ));


}
