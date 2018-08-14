init(X,Y).
%init(X,Y):- X = 1, Y = 0.
l(X,Y):- init(X,Y).
l(X1,Y1):- X1 = X+Y, Y1 = Y+1, l(X,Y).
false :-  Y > X, Y>=0, l(X,Y).
safe :-  X >= Y, Y>=0, l(X,Y).
safe :-  Y<0, l(X,Y).



spec :- false. 
spec :- safe.