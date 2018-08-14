new28(A,B,C,D,E,F) :- new15(A,G), new19(G,B,C,D,E,F).
new27(A,B,C,D,E,F) :- new15(A,G), new19(G,B,C,D,E,F).
new26(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new27(A,B,C,D,E,F).
new26(A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0, new27(A,B,C,D,E,F).
new26(A,B,C,D,E,F) :- G= 0, G=B, H= 0, new28(A,B,C,D,E,F).
new25(A,B,B).
new23(A,B,C) :- D>= 1, D=B, E= 0, new25(A,B,C).
new23(A,B,C) :- D+ 1=< 0, D=B, E= 0, new25(A,B,C).
new22(A,B,C,A,B,C) :- D= 1, E= 0, E=A, F= 0, new23(A,D,G).
new22(A,B,C,A,B,C) :- D= 0, E>= 1, E=A, F= 0, new23(A,D,G).
new22(A,B,C,A,B,C) :- D= 0, E+ 1=< 0, E=A, F= 0, new23(A,D,G).
new21(A,B,C,D,E,F) :- G=H, new26(A,G,H,D,E,F).
new20(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, new21(A,B,C,D,E,F).
new20(A,B,C,D,E,F) :- G=< 0, G=A, H= 0, new22(A,B,C,D,E,F).
new19(A,B,C,D,E,F) :- new20(A,B,C,D,E,F).
new18(A,B,C,D,E,F) :- new19(A,B,C,D,E,F).
new16(A,B) :- new18(A,C,D,B,E,F).
safe :- init(A), new16(A,B).
new15(A,B) :- B=C- 1, C=A, D= 1.
new13(A,B,C,D,E,F) :- new15(A,G), new3(G,B,C,D,E,F).
new12(A,B,C,D,E,F) :- new15(A,G), new3(G,B,C,D,E,F).
new11(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new12(A,B,C,D,E,F).
new11(A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0, new12(A,B,C,D,E,F).
new11(A,B,C,D,E,F) :- G= 0, G=B, H= 0, new13(A,B,C,D,E,F).
new10(A).
new8(A,B,B) :- new10(A).
new7(A,B,C) :- D= 0, D=B, E= 0, new8(A,B,C).
new6(A,B,C,A,B,C) :- D= 1, E= 0, E=A, F= 0, new7(A,D,G).
new6(A,B,C,A,B,C) :- D= 0, E>= 1, E=A, F= 0, new7(A,D,G).
new6(A,B,C,A,B,C) :- D= 0, E+ 1=< 0, E=A, F= 0, new7(A,D,G).
new5(A,B,C,D,E,F) :- G=H, new11(A,G,H,D,E,F).
new4(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, new5(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G=< 0, G=A, H= 0, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
