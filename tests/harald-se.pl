%pre-condition that  leads to the error y-|x|=7

/*
int x, y;
if (x <0)
    x= -x;
while(x>0){
    x--;
    y--;
}
z= 4/(2y-14)

*/


init(X,Y).
if(X,Y) :-
    init(X,Y).
while_entry(X1,Y) :-
    X =< -1, 
    X1 = -X, 
    if(X,Y).
while_entry(X,Y) :-
    X>=0, 
    if(X,Y).
while(X,Y):-
    while_entry(X,Y).
while(X1,Y1) :-
    X>=1, 
    X1=X-1, 
    Y1=Y-1,
    while(X,Y).
while_exit(X,Y):-
    X=<0,
    while(X,Y).
false:-
    2*Y-14=0,
    while_exit(X,Y).
safe:-
    2*Y-14=< -1,
    while_exit(X,Y).
safe:-
    2*Y-14>=1,
    while_exit(X,Y).

spec :- false. 
spec :- safe.