# 1 "MAP/SAFE-exbench/DAGGER-hsortprime.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/DAGGER-hsortprime.tmp.c"
# 18 "MAP/SAFE-exbench/DAGGER-hsortprime.tmp.c"
int nondet() {int i; return i;}

int n;
int l;
int r;
int i;
int j;
void main()
{
/*
 int n;
 int l;
 int r;
 int i;
 int j;
 */

 if (! (n >= 2)) return;
 if (! (r - n == 0)) return;
 if (! (i - l ==0)) return;
 if (! (j - 2*l == 0)) return;
 if (! (2*l - n >= 0)) return;
 if (! (2*l - n - 1 <= 0)) return;

 while (nondet())
 {
  if (nondet())
  {
   if (! (r -j -1 >= 0)) return;
   i = j + 1;
   j = 2*j + 2;
  }
  else
  {
   if (nondet())
   {
    if (! (j -r <=0)) return;
    i = j;
    j = 2*j;
   }
   else
   {
    if (nondet())
    {
     if (! (l >=2)) return;
     if (! (r >=2)) return;
     i = l - 1;
     j = 2 *l - 2;
     l = l - 1;
    }
    else
    {
     if (! (l <= 1)) return;
     r = r - 1;
     if (! (r >=3)) return;
     i = l;
     j = 2*l;
    }
   }
  }
 }
 __VERIFIER_assert( -2*l + r + 1 >= 0 );
 return;
}
