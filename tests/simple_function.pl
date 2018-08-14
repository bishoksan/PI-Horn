/*
integer y = [0;100];

integer f(integer x)
{
    if (x in [0;1]==0) return x+1;
    return x-1;
}

void main()
{
    integer z = f(y);
    assert(z>=5 && z=<60);
}

*/

/* note that issues like init predicates need to be dealt with, for example in this case, we cannot only depend on constrained facts*/
/* the correct answer should be y in [6,61]*/

/*  non linear*/
f(A, B):- 0=<A, A=<1, B=A+1.
f(A, B):- A=< -1,  B=A-1.
f(A, B):- A>= 2,  B=A-1.


init(A,B):-A>=0, A=<100.
f_call(A,B):- init(A,B), f(A,B).
false:- f_call(A,B), B=<4.
false:- f_call(A,B), B>=61.
safe:- f_call(A,B), B>=5, B=<60.




/* linear*/
/*
init(A,B):-A>=0, A=<100.
f_call(A,B):- init(A,B), 0=<A, A=<1, B=A+1.
f_call(A,B):- init(A,B), A=< -1,  B=A-1.
f_call(A,B):- init(A,B), A>= 2,  B=A-1.
false:- f_call(A,B), B=<4.
false:- f_call(A,B), B>=61.
*/
spec :- false. 
spec :- safe.