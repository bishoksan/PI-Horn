# 1 "MAP/UNSAFE-exbench/TRACER-testfunc13-unsafe.tmp.c"
# 1 "<command-line>"
# 1 "MAP/UNSAFE-exbench/TRACER-testfunc13-unsafe.tmp.c"
# 19 "MAP/UNSAFE-exbench/TRACER-testfunc13-unsafe.tmp.c"
//extern int unknown();

void bar(){
  int i,NONDET,q,z;
  i=0;

  if (q>0) z=4;
  else z=5;

  while (NONDET){
    i++;
  }
  return;
}

int p;
main(){
  int x;

  if (p>0) x=1;
  else x=2;

  bar();

  __VERIFIER_assert(!( x==2 ));

}
