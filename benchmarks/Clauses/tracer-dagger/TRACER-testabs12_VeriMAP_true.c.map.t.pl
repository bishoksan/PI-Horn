new18(A,B,B) :- C>= 1, C=B, D= 0.
new18(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new17(A,B,C,D,E,F) :- G+ 1=< 0, G=A, H= 0, I= 0, new16(A,B,I,D,E,F).
new17(A,B,C,D,E,F) :- G>= 0, G=A, H= 0, I= 1, new16(A,B,I,D,E,F).
new16(A,B,C,A,B,C) :- D=C, new18(A,D,E).
new15(A,B,C,D,E,F) :- G>= 101, G=B, H= 100, I= 0, new16(A,B,I,D,E,F).
new15(A,B,C,D,E,F) :- G=< 100, G=B, H= 100, new17(A,B,C,D,E,F).
new14(A,B,C,D,E,F) :- G+ 1=< 100, G=B, H= 100, I=J+ 1, J=A, K= 1, L=M+ 1, M=B, 
          N= 1, new13(I,L,C,D,E,F).
new14(A,B,C,D,E,F) :- G>= 100, G=B, H= 100, new15(A,B,C,D,E,F).
new13(A,B,C,D,E,F) :- new14(A,B,C,D,E,F).
new12(A,B,C,D,E,F) :- G= 0, new13(A,G,C,D,E,F).
new10(A,B) :- new12(A,C,D,B,E,F).
safe :- init(A), new10(A,B).
new9(A,B,B).
new8(A,B,C) :- D= 0, D=B, E= 0, new9(A,B,C).
new7(A,B,C,D,E,F) :- G+ 1=< 0, G=A, H= 0, I= 0, new6(A,B,I,D,E,F).
new7(A,B,C,D,E,F) :- G>= 0, G=A, H= 0, I= 1, new6(A,B,I,D,E,F).
new6(A,B,C,A,B,C) :- D=C, new8(A,D,E).
new5(A,B,C,D,E,F) :- G>= 101, G=B, H= 100, I= 0, new6(A,B,I,D,E,F).
new5(A,B,C,D,E,F) :- G=< 100, G=B, H= 100, new7(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G+ 1=< 100, G=B, H= 100, I=J+ 1, J=A, K= 1, L=M+ 1, M=B, 
          N= 1, new3(I,L,C,D,E,F).
new4(A,B,C,D,E,F) :- G>= 100, G=B, H= 100, new5(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G= 0, new3(A,G,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
