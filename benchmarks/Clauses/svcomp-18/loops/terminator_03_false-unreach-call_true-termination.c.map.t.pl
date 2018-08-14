new24(A,B,C,D,E,F) :- G+ 1=< 100, G=A, H= 100, I=J+K, J=A, K=B, 
          new16(I,B,C,D,E,F).
new24(A,B,C,D,E,F) :- G>= 100, G=A, H= 100, new17(A,B,C,D,E,F).
new23(A,B,C,C).
new21(A,B,C,D) :- E>= 1, E=C, F= 0, new23(A,B,C,D).
new21(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new23(A,B,C,D).
new20(A,B,C,D,E,F) :- G>= 100, G=A, H= 100, I= 1, new18(A,B,I,D,E,F).
new20(A,B,C,D,E,F) :- G+ 1=< 100, G=A, H= 100, I= 0, new18(A,B,I,D,E,F).
new19(A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0, new20(A,B,C,D,E,F).
new19(A,B,C,D,E,F) :- G>= 0, G=B, H= 0, I= 0, new18(A,B,I,D,E,F).
new18(A,B,C,A,B,C) :- D=C, new21(A,B,D,E).
new17(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, I= 1, new18(A,B,I,D,E,F).
new17(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new19(A,B,C,D,E,F).
new16(A,B,C,D,E,F) :- new24(A,B,C,D,E,F).
new15(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new16(A,B,C,D,E,F).
new15(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, new17(A,B,C,D,E,F).
new13(A,B,C,D) :- new15(A,B,E,C,D,F).
safe :- init(A,B), new13(A,B,C,D).
new12(A,B,C,D,E,F) :- G+ 1=< 100, G=A, H= 100, I=J+K, J=A, K=B, 
          new3(I,B,C,D,E,F).
new12(A,B,C,D,E,F) :- G>= 100, G=A, H= 100, new4(A,B,C,D,E,F).
new11(A,B).
new9(A,B,C,C) :- new11(A,B).
new8(A,B,C,D) :- E= 0, E=C, F= 0, new9(A,B,C,D).
new7(A,B,C,D,E,F) :- G>= 100, G=A, H= 100, I= 1, new5(A,B,I,D,E,F).
new7(A,B,C,D,E,F) :- G+ 1=< 100, G=A, H= 100, I= 0, new5(A,B,I,D,E,F).
new6(A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0, new7(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G>= 0, G=B, H= 0, I= 0, new5(A,B,I,D,E,F).
new5(A,B,C,A,B,C) :- D=C, new8(A,B,D,E).
new4(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, I= 1, new5(A,B,I,D,E,F).
new4(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new12(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new3(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, new4(A,B,C,D,E,F).
new1(A,B,C,D) :- new2(A,B,E,C,D,F).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
