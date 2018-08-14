# 1 "MAP/SAFE-exbench/TRACER-testwp12.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/TRACER-testwp12.tmp.c"
# 23 "MAP/SAFE-exbench/TRACER-testwp12.tmp.c"

int x = 0, y2,y3;
void main(){
    int y1;
    //,y2,y3;
  //int x = 0;
  int p,z;



  if (y2>0) x=x+2;
  else y2=3;
  if (p>0) z=3;
  else z=2;
  if (y3>0) x=x+4;
  else y3=3;

  __VERIFIER_assert(!( x>6 && ( y2<3 || y3<0 ) ));
  return;
}
