new14(A,B,B) :- C>= 1, C=B, D= 0.
new14(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new13(A,B) :- C= 3, C=A, D= 3, E=F+ 1, F=A, G= 1, new11(E,B).
new13(A,B) :- C>= 4, C=A, D= 3, E= 0, new11(E,B).
new13(A,B) :- C+ 1=< 3, C=A, D= 3, E= 0, new11(E,B).
new12(A,B) :- C= 2, C=A, D= 2, E=F+ 1, F=A, G= 1, new11(E,B).
new12(A,B) :- C>= 3, C=A, D= 2, new13(A,B).
new12(A,B) :- C+ 1=< 2, C=A, D= 2, new13(A,B).
new11(A,A) :- B= 1, C>= 1, C=A, D= 0, new14(A,B,E).
new11(A,A) :- B= 0, C=< 0, C=A, D= 0, new14(A,B,E).
new10(A,B) :- C= 1, C=A, D= 1, E=F+ 1, F=A, G= 1, new11(E,B).
new10(A,B) :- C>= 2, C=A, D= 1, new12(A,B).
new10(A,B) :- C+ 1=< 1, C=A, D= 1, new12(A,B).
new8(A,B) :- new10(A,B).
safe :- init(A), new8(A,B).
new7(A,B,B).
new6(A,B,C) :- D= 0, D=B, E= 0, new7(A,B,C).
new5(A,B) :- C= 3, C=A, D= 3, E=F+ 1, F=A, G= 1, new3(E,B).
new5(A,B) :- C>= 4, C=A, D= 3, E= 0, new3(E,B).
new5(A,B) :- C+ 1=< 3, C=A, D= 3, E= 0, new3(E,B).
new4(A,B) :- C= 2, C=A, D= 2, E=F+ 1, F=A, G= 1, new3(E,B).
new4(A,B) :- C>= 3, C=A, D= 2, new5(A,B).
new4(A,B) :- C+ 1=< 2, C=A, D= 2, new5(A,B).
new3(A,A) :- B= 1, C>= 1, C=A, D= 0, new6(A,B,E).
new3(A,A) :- B= 0, C=< 0, C=A, D= 0, new6(A,B,E).
new2(A,B) :- C= 1, C=A, D= 1, E=F+ 1, F=A, G= 1, new3(E,B).
new2(A,B) :- C>= 2, C=A, D= 1, new4(A,B).
new2(A,B) :- C+ 1=< 1, C=A, D= 1, new4(A,B).
new1(A,B) :- new2(A,B).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
