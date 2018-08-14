/*
Precond: X=<99 or Y>=100

//x=0; y=0;
while(x=<99){
    x=x+1;
    y=y+1;
}
assert(y>=100)
*/


%init(X,Y):- X=0, Y=0.
init(X,Y).
while(X,Y):- init(X,Y), X>=100.
while(X1,Y1):- while(X,Y), X=<99, X1=X+1, Y1=Y+1.
false:- while(X,Y), Y=<99.
safe:- while(X,Y), Y>=99.
spec :- false. 
spec :- safe.