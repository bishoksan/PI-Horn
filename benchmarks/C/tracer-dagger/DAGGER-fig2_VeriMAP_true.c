# 1 "MAP/SAFE-exbench/DAGGER-fig2.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/DAGGER-fig2.tmp.c"
# 19 "MAP/SAFE-exbench/DAGGER-fig2.tmp.c"

int x, y, z, w;
void main () {

//int x, y, z, w;
//x=y=z=w=0;


while (nondet() ) {

if (nondet()) {x++; y = y+2;}
else if (nondet()) {
 if (x >= 4) {x++; y = y+3; z = z+10; w = w+10;}
}
else if (x >= z && w > y) {x = -x; y = -y; }

}

__VERIFIER_assert( 3*x >= y );
}
