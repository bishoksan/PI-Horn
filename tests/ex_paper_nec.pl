% good example
init(X,Y).
while_entry(X1,Y) :-
   init(X,Y), X=< 100, X1 = 100 -X.
while_entry(X1,Y) :-
    init(X,Y), X>=101, X1=X-100.
while(X,Y):-
    while_entry(X,Y).
while(X1,Y1) :-
    X>=1, X1=X-1, Y1=Y-2, while(X,Y).
false:-
    X=<0,
    while(X,Y),Y=< -1.
false:-
    X=<0,
    while(X,Y),Y>=1.
safe:-
    X=<0,
    while(X,Y),Y=0.
spec :- false. 
spec :- safe.