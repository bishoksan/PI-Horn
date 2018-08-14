new25(A,B,C,D,E,F) :- G= 4, G=B, H= 4, I= 1, J= 6, new22(A,I,J,D,E,F).
new25(A,B,C,D,E,F) :- G>= 5, G=B, H= 4, I= 2, new22(A,I,C,D,E,F).
new25(A,B,C,D,E,F) :- G+ 1=< 4, G=B, H= 4, I= 2, new22(A,I,C,D,E,F).
new24(A,B,C,D,E,F) :- G= 3, G=B, H= 3, I= 1, J= 5, new22(A,I,J,D,E,F).
new24(A,B,C,D,E,F) :- G>= 4, G=B, H= 3, new25(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- G+ 1=< 3, G=B, H= 3, new25(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- G= 2, G=B, H= 2, I= 3, J= 4, new22(A,I,J,D,E,F).
new23(A,B,C,D,E,F) :- G>= 3, G=B, H= 2, new24(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- G+ 1=< 2, G=B, H= 2, new24(A,B,C,D,E,F).
new22(A,B,C,D,E,F) :- G=H+ 1, H=A, I= 1, new16(G,B,C,D,E,F).
new20(A,B,B) :- C>= 1, C=B, D= 0.
new20(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new19(A,B,C,A,B,C) :- D= 1, E>= 7, E=C, F= 6, new20(A,D,G).
new19(A,B,C,A,B,C) :- D= 1, E+ 1=< 6, E=C, F= 6, new20(A,D,G).
new19(A,B,C,A,B,C) :- D= 0, E= 6, E=C, F= 6, new20(A,D,G).
new18(A,B,C,D,E,F) :- G= 1, G=B, H= 1, I= 2, J= 3, new22(A,I,J,D,E,F).
new18(A,B,C,D,E,F) :- G>= 2, G=B, H= 1, new23(A,B,C,D,E,F).
new18(A,B,C,D,E,F) :- G+ 1=< 1, G=B, H= 1, new23(A,B,C,D,E,F).
new17(A,B,C,D,E,F) :- G+ 1=< 10, G=A, H= 10, new18(A,B,C,D,E,F).
new17(A,B,C,D,E,F) :- G>= 10, G=A, H= 10, new19(A,B,C,D,E,F).
new16(A,B,C,D,E,F) :- new17(A,B,C,D,E,F).
new15(A,B,C,D,E,F) :- G= 1, new16(A,G,C,D,E,F).
new13(A,B) :- new15(A,C,D,B,E,F).
safe :- init(A), new13(A,B).
new12(A,B,C,D,E,F) :- G= 4, G=B, H= 4, I= 1, J= 6, new9(A,I,J,D,E,F).
new12(A,B,C,D,E,F) :- G>= 5, G=B, H= 4, I= 2, new9(A,I,C,D,E,F).
new12(A,B,C,D,E,F) :- G+ 1=< 4, G=B, H= 4, I= 2, new9(A,I,C,D,E,F).
new11(A,B,C,D,E,F) :- G= 3, G=B, H= 3, I= 1, J= 5, new9(A,I,J,D,E,F).
new11(A,B,C,D,E,F) :- G>= 4, G=B, H= 3, new12(A,B,C,D,E,F).
new11(A,B,C,D,E,F) :- G+ 1=< 3, G=B, H= 3, new12(A,B,C,D,E,F).
new10(A,B,C,D,E,F) :- G= 2, G=B, H= 2, I= 3, J= 4, new9(A,I,J,D,E,F).
new10(A,B,C,D,E,F) :- G>= 3, G=B, H= 2, new11(A,B,C,D,E,F).
new10(A,B,C,D,E,F) :- G+ 1=< 2, G=B, H= 2, new11(A,B,C,D,E,F).
new9(A,B,C,D,E,F) :- G=H+ 1, H=A, I= 1, new3(G,B,C,D,E,F).
new8(A,B,B).
new7(A,B,C) :- D= 0, D=B, E= 0, new8(A,B,C).
new6(A,B,C,A,B,C) :- D= 1, E>= 7, E=C, F= 6, new7(A,D,G).
new6(A,B,C,A,B,C) :- D= 1, E+ 1=< 6, E=C, F= 6, new7(A,D,G).
new6(A,B,C,A,B,C) :- D= 0, E= 6, E=C, F= 6, new7(A,D,G).
new5(A,B,C,D,E,F) :- G= 1, G=B, H= 1, I= 2, J= 3, new9(A,I,J,D,E,F).
new5(A,B,C,D,E,F) :- G>= 2, G=B, H= 1, new10(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- G+ 1=< 1, G=B, H= 1, new10(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G+ 1=< 10, G=A, H= 10, new5(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G>= 10, G=A, H= 10, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G= 1, new3(A,G,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
