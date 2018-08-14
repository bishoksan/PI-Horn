# 1 "MAP/SAFE-exbench/TRACER-testabs12.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testabs12.tmp.c"
# 18 "MAP/SAFE-exbench/TRACER-testabs12.tmp.c"

int count;
main(){
  int i,n;

 // __VERIFIER_assume( count >= 0 );
  i=0;


  while (i < 100 ){
      count++;
      i++;
  }

  __VERIFIER_assert(!( (i > 100 ) || count < 0 ));
}
