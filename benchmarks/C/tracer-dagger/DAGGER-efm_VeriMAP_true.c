# 1 "MAP/SAFE-exbench/DAGGER-efm.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/DAGGER-efm.tmp.c"
# 18 "MAP/SAFE-exbench/DAGGER-efm.tmp.c"
int nondet() {int i; return i;}

int X1;
int X2;
int X3;
int X4;
int X5;
int X6;

void main()
{
    /*
 int X1;
 int X2;
 int X3;
 int X4;
 int X5;
 int X6;
     */

 if (! (X1>=1)) return;
 if (! (X2==0)) return;
 if (! (X3==0)) return;
 if (! (X4==1)) return;
 if (! (X5==0)) return;
 if (! (X6==0)) return;

 while(nondet())
 {
  if (nondet())
  {
   if (! (X1 >= 1)) return;
   if (! (X4 >= 1)) return;
   X1=X1-1;
   X4=X4-1;
   X2=X2+1;
   X5=X5+1;
  }
  else
  {
   if (nondet())
   {
    if (! (X2 >= 1)) return;
    if (! (X6 >= 1)) return;
    X2=X2-1;
    X3=X3+1;
   }
   else
   {
    if (nondet())
    {
     if (! (X4 >= 1)) return;
     if (! (X3 >= 1)) return;
     X3=X3-1;
     X2=X2+1;
    }
    else
    {
     if (nondet())
     {
      if (! (X3>=1)) return;
      X3=X3-1;
      X1=X1+1;
      X6=X6+X5;
      X5=0;
     }
     else
     {
      if (! (X2 >= 1)) return;
      X2 = X2 - 1;
      X1 = X1 + 1;
      X4 = X4 + X6;
      X6 = 0;
     }
    }
   }
  }
 }

 __VERIFIER_assert( X4 + X5 + X6 -1 >= 0 );
 __VERIFIER_assert( X4 + X5 + X6 -1 <= 0 );
 __VERIFIER_assert( X4 + X5 <= 1 );
 __VERIFIER_assert( X5 >= 0 );
 __VERIFIER_assert( X4 >= 0 );
 __VERIFIER_assert( X3 >= 0 );
 __VERIFIER_assert( X2 >= 0 );
 __VERIFIER_assert( X1 + X5 >= 1 );
 __VERIFIER_assert( X1 + X2 >= X4 + X5 );
 __VERIFIER_assert( X1 + X2 + X3 >= 1 );

}
