# 1 "MAP/SAFE-exbench/DAGGER-seesaw.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/DAGGER-seesaw.tmp.c"
# 18 "MAP/SAFE-exbench/DAGGER-seesaw.tmp.c"
int nondet() {int i; return i; }

int x;
int y;
void main()
{
 //int x;
 //int y;

 if (! (x==0)) return;
 if (! (y==0)) return;

 while (nondet())
 {
  if (nondet())
  {
   if (! (x >= 9)) return;
   x = x + 2;
   y = y + 1;
  }
  else
  {
   if (nondet())
   {
    if (!(x >= 7)) return;
    if (!(x <= 9)) return;
    x = x + 1;
    y = y + 3;
   }
   else
   {
    if (nondet())
    {
     if (! (x - 5 >=0)) return;
     if (! (x - 7 <=0)) return;
     x = x + 2;
     y = y + 1;
    }
    else
    {
     if (!(x - 4 <=0)) return;
     x = x + 1;
     y = y + 2;
    }
   }
  }
 }
 __VERIFIER_assert( -x + 2*y >= 0 );
 __VERIFIER_assert( 3*x - y >= 0 );
}
