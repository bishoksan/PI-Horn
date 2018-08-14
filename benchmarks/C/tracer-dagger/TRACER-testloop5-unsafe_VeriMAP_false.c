# 1 "MAP/SAFE-exbench/TRACER-testloop5.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testloop5.tmp.c"
# 20 "MAP/SAFE-exbench/TRACER-testloop5.tmp.c"
int i = 0;

int foo(int i)
{
  i++;
  return i;
}

void main() {

  while(foo() < 10){}

  __VERIFIER_assert(!( i == 10 ));

}
