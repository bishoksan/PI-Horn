new18(A,B) :- C= 1, D=< 1000000, D=A, E= 1000000, F=G+ 1, G=A, H= 1, 
          new7(A,C,I), new16(F,B).
new18(A,B) :- C= 0, D>= 1000001, D=A, E= 1000000, F=G+ 1, G=A, H= 1, 
          new7(A,C,I), new16(F,B).
new17(A,B) :- C>= 1000001, C=A, D= 1000000, new18(A,B).
new17(A,B) :- C+ 1=< 1000000, C=A, D= 1000000, new18(A,B).
new17(A,A) :- B= 1000000, B=A, C= 1000000.
new16(A,B) :- new17(A,B).
new15(A,B) :- C= 0, new16(C,B).
new13(A,B) :- new15(A,B).
safe :- init(A), new13(A,B).
new12(A).
new10(A,B,B) :- new12(A).
new9(A,B,B).
new7(A,B,C) :- D>= 1, D=B, E= 0, new9(A,B,C).
new7(A,B,C) :- D+ 1=< 0, D=B, E= 0, new9(A,B,C).
new6(A,B,C) :- D= 0, D=B, E= 0, new10(A,B,C).
new5(A,A) :- B= 1, C=< 1000000, C=A, D= 1000000, new6(A,B,E).
new5(A,B) :- C= 1, D=< 1000000, D=A, E= 1000000, F=G+ 1, G=A, H= 1, 
          new7(A,C,I), new3(F,B).
new5(A,A) :- B= 0, C>= 1000001, C=A, D= 1000000, new6(A,B,E).
new5(A,B) :- C= 0, D>= 1000001, D=A, E= 1000000, F=G+ 1, G=A, H= 1, 
          new7(A,C,I), new3(F,B).
new4(A,B) :- C>= 1000001, C=A, D= 1000000, new5(A,B).
new4(A,B) :- C+ 1=< 1000000, C=A, D= 1000000, new5(A,B).
new3(A,B) :- new4(A,B).
new2(A,B) :- C= 0, new3(C,B).
new1(A,B) :- new2(A,B).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
