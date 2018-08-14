/*
// inequality test

// the backward analysis finds i in [3;17], which lead to executions
// entering either branch

integer i = [0;20];

void main()
{
    integer k = 5;
    if (i >= 10) {
        k = k + i - 2;
    }
    else {
        k = k + i + 2;
    }
    assert(k>=10 && k<=20);
}

*/

% the backward analysis finds i in [3;17], which lead to executions entering either branch


init(A):- A>=0, A=<20.
main(A,B):- init(A),  B=5.
if_exit(A,B):-main(A,C), A>=10, B=C+A-2.
if_exit(A,B):-main(A,C), A=<9,B=C+A+2.
false:- if_exit(A,B), B=<9.
false:- if_exit(A,B), B>=21.

safe:- if_exit(A,B), B>=10, B=<20.



spec :- false. 
spec :- safe.