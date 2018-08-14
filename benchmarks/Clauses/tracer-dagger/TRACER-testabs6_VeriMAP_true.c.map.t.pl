new10(A,B,B) :- C>= 1, C=B, D= 0.
new10(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new9(A,B,C,A,B,C) :- D= 1, E>= 5, E=B, F= 4, new10(A,D,G).
new9(A,B,C,A,B,C) :- D= 1, E+ 1=< 4, E=B, F= 4, new10(A,D,G).
new9(A,B,C,A,B,C) :- D= 0, E= 4, E=B, F= 4, new10(A,D,G).
new8(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, I= 4, J= 1, new9(A,I,J,D,E,F).
new8(A,B,C,D,E,F) :- G=< 0, G=A, H= 0, I= 100, J= 2, new9(A,I,J,D,E,F).
new6(A,B) :- new8(A,C,D,B,E,F).
safe :- init(A), new6(A,B).
new5(A,B,B).
new4(A,B,C) :- D= 0, D=B, E= 0, new5(A,B,C).
new3(A,B,C,A,B,C) :- D= 1, E>= 5, E=B, F= 4, new4(A,D,G).
new3(A,B,C,A,B,C) :- D= 1, E+ 1=< 4, E=B, F= 4, new4(A,D,G).
new3(A,B,C,A,B,C) :- D= 0, E= 4, E=B, F= 4, new4(A,D,G).
new2(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, I= 4, J= 1, new3(A,I,J,D,E,F).
new2(A,B,C,D,E,F) :- G=< 0, G=A, H= 0, I= 100, J= 2, new3(A,I,J,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
