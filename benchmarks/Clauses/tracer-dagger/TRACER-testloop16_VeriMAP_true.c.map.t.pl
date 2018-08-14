new19(A,B,C,D,E,F) :- G=H+ 1, H=C, I= 1, new13(A,B,G,D,E,F).
new17(A,B,C,C) :- D>= 1, D=C, E= 0.
new17(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new16(A,B,C,A,B,C) :- D= 1, E=< 2, E=B, F= 2, new17(A,B,D,G).
new16(A,B,C,A,B,C) :- D= 0, E>= 3, E=B, F= 2, new17(A,B,D,G).
new15(A,B,C,D,E,F) :- G= 1, G=B, H= 1, I= 2, new19(A,I,C,D,E,F).
new15(A,B,C,D,E,F) :- G>= 2, G=B, H= 1, I= 1, new19(A,I,C,D,E,F).
new15(A,B,C,D,E,F) :- G+ 1=< 1, G=B, H= 1, I= 1, new19(A,I,C,D,E,F).
new14(A,B,C,D,E,F) :- G+ 1=<H, G=C, H=A, new15(A,B,C,D,E,F).
new14(A,B,C,D,E,F) :- G>=H, G=C, H=A, new16(A,B,C,D,E,F).
new13(A,B,C,D,E,F) :- new14(A,B,C,D,E,F).
new12(A,B,C,D,E,F) :- G= 0, new13(A,B,G,D,E,F).
new10(A,B,C,D) :- new12(A,B,E,C,D,F).
safe :- init(A,B), new10(A,B,C,D).
new9(A,B,C,D,E,F) :- G=H+ 1, H=C, I= 1, new3(A,B,G,D,E,F).
new8(A,B,C,C).
new7(A,B,C,D) :- E= 0, E=C, F= 0, new8(A,B,C,D).
new6(A,B,C,A,B,C) :- D= 1, E=< 2, E=B, F= 2, new7(A,B,D,G).
new6(A,B,C,A,B,C) :- D= 0, E>= 3, E=B, F= 2, new7(A,B,D,G).
new5(A,B,C,D,E,F) :- G= 1, G=B, H= 1, I= 2, new9(A,I,C,D,E,F).
new5(A,B,C,D,E,F) :- G>= 2, G=B, H= 1, I= 1, new9(A,I,C,D,E,F).
new5(A,B,C,D,E,F) :- G+ 1=< 1, G=B, H= 1, I= 1, new9(A,I,C,D,E,F).
new4(A,B,C,D,E,F) :- G+ 1=<H, G=C, H=A, new5(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G>=H, G=C, H=A, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G= 0, new3(A,B,G,D,E,F).
new1(A,B,C,D) :- new2(A,B,E,C,D,F).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
