init(A,B).
l(A,B) :- B>A, init(A,B).
l(A,B) :- C=A+B, D=B+1, l(C,D).
false :- A=1, B=0, l(A,B).

safe :- A>1,  l(A,B).
safe :- A<1, l(A,B).
safe :-  B>0, l(A,B).
safe :- B<0, l(A,B).
spec :- false. 
spec :- safe.