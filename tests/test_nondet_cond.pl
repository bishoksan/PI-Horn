/*
// inequality test, with non-deterministic test

// the backward analysis still finds i in [3;17]
// some values of i lead to different executions following different branches

integer i = [0;20];
volatile integer j = [9;11];

void main()
{
    integer k = 5;
    if (i >= j) {
        k = k + i - 2;
    }
    else {
        k = k + i + 2;
    }
    assert(k>=10 && k<=20);
}
*/

init(A,B):-A>=0, A=<20, B>=9, B=<11.
main(A,B,C):-init(A,B),C=5.
if_entry(A,B,C):- main(A,B,C).
if_exit(A,B,C):-if_entry(A,B,D), A>=B, C=D+A-2.
if_exit(A,B,C):-if_entry(A,B,D), A=<B-1, C=D+A+2.
false:- if_exit(A,B,C), C=<9.
false:- if_exit(A,B,C), C>=21.

safe:- if_exit(A,B), B>=10, B=<20.

spec :- false. 
spec :- safe.