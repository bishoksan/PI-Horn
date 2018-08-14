new13(A,A).
new12(A,B) :- C= 1, D>= 5, D=A, E= 5, F=G+ 2, G=A, H= 2, new6(A,C,I), 
          new13(F,B).
new12(A,B) :- C= 0, D+ 1=< 5, D=A, E= 5, F=G+ 2, G=A, H= 2, new6(A,C,I), 
          new13(F,B).
new11(A,B) :- C>= 11, C=A, D= 10, new12(A,B).
new11(A,B) :- C=< 10, C=A, D= 10, new13(A,B).
new9(A,B) :- new11(A,B).
safe :- init(A), new9(A,B).
new8(A,B,B).
new6(A,B,B) :- C>= 1, C=B, D= 0.
new6(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new5(A,B,C) :- D= 0, D=B, E= 0, new8(A,B,C).
new3(A,A) :- B= 1, C>= 5, C=A, D= 5, new5(A,B,E).
new3(A,A) :- B= 0, C+ 1=< 5, C=A, D= 5, new5(A,B,E).
new2(A,B) :- C>= 11, C=A, D= 10, new3(A,B).
new1(A,B) :- new2(A,B).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
