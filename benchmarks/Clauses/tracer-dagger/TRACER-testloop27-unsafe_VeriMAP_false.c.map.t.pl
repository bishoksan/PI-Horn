new19(A,B,C,D,E,F) :- G=H+ 1, H=B, I= 1, new13(A,G,C,D,E,F).
new17(A,B,B) :- C>= 1, C=B, D= 0.
new17(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new16(A,B,C,A,B,C) :- D= 1, E= 0, E=C, F= 0, new17(A,D,G).
new16(A,B,C,A,B,C) :- D= 0, E>= 1, E=C, F= 0, new17(A,D,G).
new16(A,B,C,A,B,C) :- D= 0, E+ 1=< 0, E=C, F= 0, new17(A,D,G).
new15(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, I=J- 1, J=C, K= 1, new19(A,B,I,D,E,F).
new15(A,B,C,D,E,F) :- G=< 0, G=A, H= 0, new19(A,B,C,D,E,F).
new14(A,B,C,D,E,F) :- G+ 1=< 1, G=B, H= 1, I=J+ 1, J=C, K= 1, 
          new15(A,B,I,D,E,F).
new14(A,B,C,D,E,F) :- G>= 1, G=B, H= 1, new16(A,B,C,D,E,F).
new13(A,B,C,D,E,F) :- new14(A,B,C,D,E,F).
new12(A,B,C,D,E,F) :- G= 0, H= 0, new13(A,G,H,D,E,F).
new10(A,B) :- new12(A,C,D,B,E,F).
safe :- init(A), new10(A,B).
new9(A,B,C,D,E,F) :- G=H+ 1, H=B, I= 1, new3(A,G,C,D,E,F).
new8(A,B,B).
new7(A,B,C) :- D= 0, D=B, E= 0, new8(A,B,C).
new6(A,B,C,A,B,C) :- D= 1, E= 0, E=C, F= 0, new7(A,D,G).
new6(A,B,C,A,B,C) :- D= 0, E>= 1, E=C, F= 0, new7(A,D,G).
new6(A,B,C,A,B,C) :- D= 0, E+ 1=< 0, E=C, F= 0, new7(A,D,G).
new5(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, I=J- 1, J=C, K= 1, new9(A,B,I,D,E,F).
new5(A,B,C,D,E,F) :- G=< 0, G=A, H= 0, new9(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G+ 1=< 1, G=B, H= 1, I=J+ 1, J=C, K= 1, new5(A,B,I,D,E,F).
new4(A,B,C,D,E,F) :- G>= 1, G=B, H= 1, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G= 0, H= 0, new3(A,G,H,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
