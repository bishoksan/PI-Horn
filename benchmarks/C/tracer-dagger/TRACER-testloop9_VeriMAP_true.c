# 1 "MAP/SAFE-exbench/TRACER-testloop9.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testloop9.tmp.c"
# 19 "MAP/SAFE-exbench/TRACER-testloop9.tmp.c"
int i;
main()
{
  //int i;
  int x, y;

  //i = 0;
  x = 1;

  while (i<10) {
    if (x==1) {
      x = 2;
      y = 3;
    } else if (x==2) {
      x = 3;
      y = 4;
    } else if (x==3) {
      x = 1;
      y = 5;
    } else if (x==4) {
      x = 1;
      y = 6;
    } else {
      x = 2;
    }

    i = i + 1;
  }

  __VERIFIER_assert(!( y==6 ));




}
