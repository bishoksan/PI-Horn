/*
Precond: TRUE

if(x>=0){
    y=x
} else{
    y= -x
}

if(y>10) {
    assert(x>10 or x< -10)
}
*/



init(X,Y).
if_exit(X,Y1):- init(X,Y), X>=0, Y1=X.
if_exit(X,Y1):- init(X,Y), X=< -1, Y1= -X.
false:- if_exit(X,Y), Y>=11, X=< 10, X>= -10.
if2_exit(X,Y):- Y=<10, if_exit(X,Y).
if2_exit(X,Y):- Y>=11, if_exit(X,Y), X>=11.
if2_exit(X,Y):- Y>=11, if_exit(X,Y), X=< -9.
safe:- if2_exit(X,Y).



spec :- false. 
spec :- safe.