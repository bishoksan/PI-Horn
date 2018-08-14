/*
// inequality test, with non-deterministic body

// the backward analysis finds i in [6;13]


integer i = [0;20];
volatile integer j = [-1;2];

void main()
{
    integer k = 5;
    if (i >= 10) {
        k = k + i + j ;
    }
    else {
        k = k + i + j;
    }
    assert(k>=10 && k<=20);
}
*/


init(A,B):- A>=0, A=<20, B>= -1, B=<2.
main(A,B,C):-init(A,B),C=5.
if_entry(A,B,C):- main(A,B,C).
if_exit(A,B,C):- if_entry(A,B,D), A>=10, C=D+A+B.
if_exit(A,B,C):- if_entry(A,B,D), A=<9, C=D+A+B.
false:- if_exit(A,B,C), C=<9.
false:- if_exit(A,B,C), C>=21.

safe:- if_exit(A,B), B>=10, B=<20.

spec :- false. 
spec :- safe.