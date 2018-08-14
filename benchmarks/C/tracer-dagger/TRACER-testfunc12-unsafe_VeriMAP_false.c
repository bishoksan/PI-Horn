# 1 "MAP/UNSAFE-exbench/TRACER-testfunc12-unsafe.tmp.c"
# 1 "<command-line>"
# 1 "MAP/UNSAFE-exbench/TRACER-testfunc12-unsafe.tmp.c"
# 20 "MAP/UNSAFE-exbench/TRACER-testfunc12-unsafe.tmp.c"
int foo(){
  int tmp=1;
  return tmp;
}

int p, q;
main(){
  //int x,y,p,q;
    int x,y;

  if (q>0) y=2;
  else y=3;


  if (p>0) x=1;
  else x=2;

  foo();

  __VERIFIER_assert(!( x==2 ));

}
