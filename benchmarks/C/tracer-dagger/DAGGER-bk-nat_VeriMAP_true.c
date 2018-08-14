# 1 "MAP/SAFE-exbench/DAGGER-bk-nat.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/DAGGER-bk-nat.tmp.c"
# 18 "MAP/SAFE-exbench/DAGGER-bk-nat.tmp.c"
int nondet() {int i; return i;}

int invalid;
int unowned;
int nonexclusive;
int exclusive;
void main()
{
/*
 int invalid;
 int unowned;
 int nonexclusive;
 int exclusive;
*/

 if (! (exclusive==0)) return;
 if (! (nonexclusive==0)) return;
 if (! (unowned==0)) return;
 if (! (invalid>= 1)) return;

 while (nondet())
 {
  if (nondet())
  {
   if (! (invalid >= 1)) return;
   nonexclusive=nonexclusive+exclusive;
   exclusive=0;
   invalid=invalid-1;
   unowned=unowned+1;
  }
  else
  {
   if (nondet())
   {
    if (! (nonexclusive + unowned >=1)) return;
    invalid=invalid + unowned + nonexclusive-1;
    exclusive=exclusive+1;
    unowned=0;
    nonexclusive=0;
   }
   else
   {
    if (! (invalid >= 1)) return;
    unowned=0;
    nonexclusive=0;
    exclusive=1;
    invalid=invalid+unowned+exclusive+nonexclusive-1;
   }
  }
 }

 __VERIFIER_assert( exclusive >= 0 );
 __VERIFIER_assert( nonexclusive >= 0 );
 __VERIFIER_assert( unowned >= 0 );
 __VERIFIER_assert( invalid >= 0 );
 __VERIFIER_assert( invalid + unowned + exclusive >= 1 );
}
