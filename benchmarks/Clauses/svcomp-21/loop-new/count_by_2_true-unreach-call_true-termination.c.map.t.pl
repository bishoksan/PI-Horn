new18(A,B,B).
new16(A,B,C) :- D>= 1, D=B, E= 0, new18(A,B,C).
new16(A,B,C) :- D+ 1=< 0, D=B, E= 0, new18(A,B,C).
new15(A,A) :- B= 1, C= 1000000, C=A, D= 1000000, new16(A,B,E).
new15(A,A) :- B= 0, C>= 1000001, C=A, D= 1000000, new16(A,B,E).
new15(A,A) :- B= 0, C+ 1=< 1000000, C=A, D= 1000000, new16(A,B,E).
new14(A,B) :- C+ 1=< 1000000, C=A, D= 1000000, E=F+ 2, F=A, G= 2, new13(E,B).
new14(A,B) :- C>= 1000000, C=A, D= 1000000, new15(A,B).
new13(A,B) :- new14(A,B).
new12(A,B) :- C= 0, new13(C,B).
new10(A,B) :- new12(A,B).
safe :- init(A), new10(A,B).
new9(A).
new7(A,B,B) :- new9(A).
new6(A,B,C) :- D= 0, D=B, E= 0, new7(A,B,C).
new5(A,A) :- B= 1, C= 1000000, C=A, D= 1000000, new6(A,B,E).
new5(A,A) :- B= 0, C>= 1000001, C=A, D= 1000000, new6(A,B,E).
new5(A,A) :- B= 0, C+ 1=< 1000000, C=A, D= 1000000, new6(A,B,E).
new4(A,B) :- C+ 1=< 1000000, C=A, D= 1000000, E=F+ 2, F=A, G= 2, new3(E,B).
new4(A,B) :- C>= 1000000, C=A, D= 1000000, new5(A,B).
new3(A,B) :- new4(A,B).
new2(A,B) :- C= 0, new3(C,B).
new1(A,B) :- new2(A,B).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
