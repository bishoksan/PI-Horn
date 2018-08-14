# 1 "MAP/SAFE-exbench/DAGGER-ex1.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/DAGGER-ex1.tmp.c"
# 18 "MAP/SAFE-exbench/DAGGER-ex1.tmp.c"

int xa;
int ya;
int main () {

int x;
int y;
//int xa = 0;
//int ya = 0;

while (nondet()) {
 x = xa + 2*ya;
 y = -2*xa + ya;

 x++;
 if (nondet()) y = y+x;
 else y = y-x;

 xa = x - 2*y;
 ya = 2*x + y;
}

__VERIFIER_assert( xa + 2*ya >= 0 );
return 0;
}
