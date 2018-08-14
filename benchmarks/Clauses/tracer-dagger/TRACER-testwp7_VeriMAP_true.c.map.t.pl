new12(A,B,C,C) :- D>= 1, D=C, E= 0.
new12(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new11(A,B,C,A,B,C) :- D= 1, E>= 3, E=F+G, F=A, G=B, H= 2, new12(A,B,D,I).
new11(A,B,C,A,B,C) :- D= 0, E=< 2, E=F+G, F=A, G=B, H= 2, new12(A,B,D,I).
new10(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, I= 3, new11(A,B,I,D,E,F).
new10(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, I= 1, new11(A,I,C,D,E,F).
new9(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, I= 2, new10(A,B,I,D,E,F).
new9(A,B,C,D,E,F) :- G=< 0, G=A, H= 0, I= 0, new10(I,B,C,D,E,F).
new7(A,B,C,D) :- new9(A,B,E,C,D,F).
safe :- init(A,B), new7(A,B,C,D).
new6(A,B,C,C).
new5(A,B,C,D) :- E= 0, E=C, F= 0, new6(A,B,C,D).
new4(A,B,C,A,B,C) :- D= 1, E>= 3, E=F+G, F=A, G=B, H= 2, new5(A,B,D,I).
new4(A,B,C,A,B,C) :- D= 0, E=< 2, E=F+G, F=A, G=B, H= 2, new5(A,B,D,I).
new3(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, I= 3, new4(A,B,I,D,E,F).
new3(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, I= 1, new4(A,I,C,D,E,F).
new2(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, I= 2, new3(A,B,I,D,E,F).
new2(A,B,C,D,E,F) :- G=< 0, G=A, H= 0, I= 0, new3(I,B,C,D,E,F).
new1(A,B,C,D) :- new2(A,B,E,C,D,F).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
