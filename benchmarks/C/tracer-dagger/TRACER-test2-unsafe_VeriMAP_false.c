# 1 "MAP/UNSAFE-exbench/TRACER-test2-unsafe.tmp.c"
# 1 "<command-line>"
# 1 "MAP/UNSAFE-exbench/TRACER-test2-unsafe.tmp.c"
# 20 "MAP/UNSAFE-exbench/TRACER-test2-unsafe.tmp.c"
//extern int unknown();

int foo(int n){
  int i = 1;
  int sum = 0;
  int product = 1;

    if (i<=n){
      sum = sum + i;
      product = product * i;
      i = i + 1;
    }
  return sum;

}

int y;
int main(){
  //int y;

  //__VERIFIER_assume( y >0 );
  if (y > 0)
    y = y + 1;
  else{
    y = foo(y) - 1;
    y = foo(5) + 5;

  }

  __VERIFIER_assert(!( y > 0 ));
}
