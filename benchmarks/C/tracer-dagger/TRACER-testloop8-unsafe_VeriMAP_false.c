# 1 "MAP/SAFE-exbench/TRACER-testloop8.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testloop8.tmp.c"
# 20 "MAP/SAFE-exbench/TRACER-testloop8.tmp.c"
//extern int unknown();
int x, y;
void main () {
  //int x, y;

  //x = 0;
  //y = 0;
  while (unknown()) {
    x ++ ;
    y ++ ;
  }
  while (x > 0) {
    x -- ;
    y -- ;
  }
  __VERIFIER_assert(!( y == 0 ));
}
