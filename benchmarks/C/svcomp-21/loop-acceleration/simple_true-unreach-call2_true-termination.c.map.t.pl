new18(A,B,B).
new16(A,B,C) :- D>= 1, D=B, E= 0, new18(A,B,C).
new16(A,B,C) :- D+ 1=< 0, D=B, E= 0, new18(A,B,C).
new15(A,A) :- B= 1, C>= 268435455, C=A, A>= 0, D= 268435455, new16(A,B,E).
new15(A,A) :- B= 0, C+ 1=< 268435455, C=A, A>= 0, D= 268435455, new16(A,B,E).
new14(A,B) :- C+ 1=< 268435455, C=A, A>= 0, D= 268435455, E=F+ 1, F=A, G= 1, 
          new13(E,B).
new14(A,B) :- C>= 268435455, C=A, A>= 0, D= 268435455, new15(A,B).
new13(A,B) :- new14(A,B).
new12(A,B) :- new13(A,B).
new10(A,B) :- new12(A,B).
safe :- init(A), new10(A,B).
new9(A).
new7(A,B,B) :- new9(A).
new6(A,B,C) :- D= 0, D=B, E= 0, new7(A,B,C).
new5(A,A) :- B= 1, C>= 268435455, C=A, A>= 0, D= 268435455, new6(A,B,E).
new5(A,A) :- B= 0, C+ 1=< 268435455, C=A, A>= 0, D= 268435455, new6(A,B,E).
new4(A,B) :- C+ 1=< 268435455, C=A, A>= 0, D= 268435455, E=F+ 1, F=A, G= 1, 
          new3(E,B).
new4(A,B) :- C>= 268435455, C=A, A>= 0, D= 268435455, new5(A,B).
new3(A,B) :- new4(A,B).
new2(A,B) :- new3(A,B).
new1(A,B) :- new2(A,B).
init(A) :- A= 0.
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
