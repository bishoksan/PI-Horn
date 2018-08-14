# 1 "MAP/SAFE-exbench/DAGGER-fig1a.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/DAGGER-fig1a.tmp.c"
# 19 "MAP/SAFE-exbench/DAGGER-fig1a.tmp.c"

int x,y;
void main () {

//int x,y;

//x=0;
//y=0;

while (nondet()) {
x++;
y++;
}

while (x > 0 || x < 0) {
x--;
y--;
}

__VERIFIER_assert( y >= 0 && y <= 0 );

}
