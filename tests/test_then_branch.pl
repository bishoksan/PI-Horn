% inequality test
% the backward analysis finds i in [10;20], selecting only executions that enter the then branch
/*
// inequality test

// the backward analysis finds i in [10;20], selecting only executions that
// enter the then branch

integer i = [0;20];

void main()
{
    integer k;
    if (i>=10) {
        k = 12;
    }
    else { k=50}
    assert(k<=30)
}

*/
        
init(A):-A>=0, A=<20.
main(A,B):-init(A).
if_entry(A,B):- main(A,B).
if_exit(A,B):-if_entry(A,_), A>=10, B=12.
if_exit(A,B):-if_entry(A,_), A=<9, B=50.
false:- if_exit(A,B), B>30.

safe:- if_exit(A,B), B=<30.
spec :- false. 
spec :- safe.