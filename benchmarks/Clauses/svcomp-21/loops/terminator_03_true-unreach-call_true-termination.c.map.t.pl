new27(A,B,C,D,E,F) :- G+ 1=< 100, G=A, H= 100, I=J+K, J=A, K=B, 
          new19(I,B,C,D,E,F).
new27(A,B,C,D,E,F) :- G>= 100, G=A, H= 100, new20(A,B,C,D,E,F).
new26(A,B,C,C).
new24(A,B,C,D) :- E>= 1, E=C, F= 0, new26(A,B,C,D).
new24(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new26(A,B,C,D).
new23(A,B,C,D,E,F) :- G>= 100, G=A, H= 100, I= 1, new21(A,B,I,D,E,F).
new23(A,B,C,D,E,F) :- G+ 1=< 100, G=A, H= 100, I= 0, new21(A,B,I,D,E,F).
new22(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new23(A,B,C,D,E,F).
new22(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, I= 0, new21(A,B,I,D,E,F).
new21(A,B,C,A,B,C) :- D=C, new24(A,B,D,E).
new20(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, I= 1, new21(A,B,I,D,E,F).
new20(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new22(A,B,C,D,E,F).
new19(A,B,C,D,E,F) :- new27(A,B,C,D,E,F).
new18(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new19(A,B,C,D,E,F).
new18(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, new20(A,B,C,D,E,F).
new17(A,B,C,D,E,F) :- G=< 1000000, G=B, H= 1000000, new18(A,B,C,D,E,F).
new16(A,B,C,A,B,C) :- D>= 1000001, D=B, E= 1000000.
new14(A,B,C,D) :- new16(A,B,E,C,D,F).
new14(A,B,C,D) :- new17(A,B,E,C,D,F).
safe :- init(A,B), new14(A,B,C,D).
new13(A,B,C,D,E,F) :- G+ 1=< 100, G=A, H= 100, I=J+K, J=A, K=B, 
          new4(I,B,C,D,E,F).
new13(A,B,C,D,E,F) :- G>= 100, G=A, H= 100, new5(A,B,C,D,E,F).
new12(A,B).
new10(A,B,C,C) :- new12(A,B).
new9(A,B,C,D) :- E= 0, E=C, F= 0, new10(A,B,C,D).
new8(A,B,C,D,E,F) :- G>= 100, G=A, H= 100, I= 1, new6(A,B,I,D,E,F).
new8(A,B,C,D,E,F) :- G+ 1=< 100, G=A, H= 100, I= 0, new6(A,B,I,D,E,F).
new7(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new8(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, I= 0, new6(A,B,I,D,E,F).
new6(A,B,C,A,B,C) :- D=C, new9(A,B,D,E).
new5(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, I= 1, new6(A,B,I,D,E,F).
new5(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new7(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- new13(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new4(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, new5(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G=< 1000000, G=B, H= 1000000, new3(A,B,C,D,E,F).
new1(A,B,C,D) :- new2(A,B,E,C,D,F).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
