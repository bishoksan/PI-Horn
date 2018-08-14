init(X,Y, I,J).
%init(X,Y, I,J):-X = I, Y =J.

l(X,Y,I,J):- init(X,Y, I,J).
l(X,Y,I,J):- X > 0, X1 = X-1, Y1 = Y-1,
    l(X1,Y1,I,J).
l(X,Y,I,J):- X < 0, X1 = X-1, Y1 = Y-1,
    l(X1,Y1,I,J).
l_exit(X,Y,I,J):- X = 0, l(X,Y,I,J).

false :- I = J, Y > 0, l_exit(X,Y,I,J).

safe :- I > J, l_exit(X,Y,I,J).
safe :- I < J, l_exit(X,Y,I,J).
safe :- Y =< 0, l_exit(X,Y,I,J).
spec :- false. 
spec :- safe.