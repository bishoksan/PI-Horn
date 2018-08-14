/*
Provided by Harald: subject2.txt
int x;
int i = 3, p = x;
while (*)
{
    if (p <= 0)
        break;
    i = i + 4;
    p = p - 1;
    
    if (i >= 80)
    {
        
        abort();
    }
}

*/


init(X).
while_entry(I,P):-
    I=3, P=X, init(X).
while(I,P):-
    while_entry(I,P).
while_exit(I,P):- %due to break statement
    while(I,P), P=<0.
if2_entry(I1,P1):-
    while(I,P), P>=1, I1=I+4, P1=P-1.
false:- if2_entry(I,P), I>=80.
while(I,P):- if2_entry(I,P), I=<79.
safe:- while_exit(I,P).


spec :- false. 
spec :- safe.