new15(A,B) :- C=A, new5(A,C,B,D).
new13(A,B) :- new15(A,B).
safe :- init(A), new13(A,B).
new10(A).
new6(A,B,A,B) :- new10(A).
new5(A,B,A,B) :- C+ 1=< 3, C=B, D= 3.
new4(A,B,C,B) :- D=B, new3(A,D,C,E).
new4(A,B,C,D) :- E=B, new5(A,E,F,G), new6(F,B,C,D).
new3(A,B,C,D) :- E>= 3, E=B, F= 3, G=H- 1, H=B, I= 1, new4(A,G,C,D).
new2(A,B) :- C=A, new3(A,C,B,D).
new1(A,B) :- new2(A,B).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
