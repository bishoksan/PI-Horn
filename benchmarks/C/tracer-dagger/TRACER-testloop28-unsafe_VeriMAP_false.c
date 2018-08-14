# 1 "MAP/SAFE-exbench/TRACER-testloop28.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testloop28.tmp.c"
# 20 "MAP/SAFE-exbench/TRACER-testloop28.tmp.c"
//extern int unknown();
int NONDET,n;
main(){
  int i,x;

  x=0;
  i=0;
  while (i<n) {
    if (unknown() >0){
      __VERIFIER_assert(!( x>0 ));
    }
    else{
      x = 1;
    }
    i++;
  }
}
