new18(A,B,B) :- C>= 1, C=B, D= 0.
new18(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new17(A,A) :- B= 1, C= 50, C=A, D= 50, new18(A,B,E).
new17(A,A) :- B= 0, C>= 51, C=A, D= 50, new18(A,B,E).
new17(A,A) :- B= 0, C+ 1=< 50, C=A, D= 50, new18(A,B,E).
new16(A,B) :- new17(A,B).
new15(A,B) :- C= 50, C=A, D= 50, new16(A,B).
new15(A,B) :- C>= 51, C=A, D= 50, new13(A,B).
new15(A,B) :- C+ 1=< 50, C=A, D= 50, new13(A,B).
new14(A,B) :- C+ 1=< 100, C=A, D= 100, E=F+ 1, F=A, G= 1, new15(E,B).
new14(A,B) :- C>= 100, C=A, D= 100, new16(A,B).
new13(A,B) :- new14(A,B).
new12(A,B) :- new13(A,B).
new10(A,B) :- new12(A,B).
safe :- init(A), new10(A,B).
new9(A,B,B).
new8(A,B,C) :- D= 0, D=B, E= 0, new9(A,B,C).
new7(A,A) :- B= 1, C= 50, C=A, D= 50, new8(A,B,E).
new7(A,A) :- B= 0, C>= 51, C=A, D= 50, new8(A,B,E).
new7(A,A) :- B= 0, C+ 1=< 50, C=A, D= 50, new8(A,B,E).
new6(A,B) :- new7(A,B).
new5(A,B) :- C= 50, C=A, D= 50, new6(A,B).
new5(A,B) :- C>= 51, C=A, D= 50, new3(A,B).
new5(A,B) :- C+ 1=< 50, C=A, D= 50, new3(A,B).
new4(A,B) :- C+ 1=< 100, C=A, D= 100, E=F+ 1, F=A, G= 1, new5(E,B).
new4(A,B) :- C>= 100, C=A, D= 100, new6(A,B).
new3(A,B) :- new4(A,B).
new2(A,B) :- new3(A,B).
new1(A,B) :- new2(A,B).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
