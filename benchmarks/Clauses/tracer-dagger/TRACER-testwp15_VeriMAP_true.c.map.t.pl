new18(A,B,C,C) :- D>= 1, D=C, E= 0.
new18(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new17(A,B,C,A,B,C) :- D= 1, E>= 1, E=F+G, F=B, G=C, H= 0, new18(A,B,D,I).
new17(A,B,C,A,B,C) :- D= 0, E=< 0, E=F+G, F=B, G=C, H= 0, new18(A,B,D,I).
new16(A,B,C,D,E,F) :- G=H, new6(A,B,I,J,K,H), new17(J,K,G,D,E,F).
new15(A,B,C,D,E,F) :- new16(A,B,C,D,E,F).
new14(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, I= 2, new15(I,B,C,D,E,F).
new14(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, I= 0, new15(A,I,C,D,E,F).
new12(A,B,C,D) :- new14(A,B,E,C,D,F).
safe :- init(A,B), new12(A,B,C,D).
new10(A,B,C,A,B,C).
new9(A,B,C,C).
new8(A,B,C,D) :- E= 0, E=C, F= 0, new9(A,B,C,D).
new7(A,B,C,A,B,C) :- D= 1, E>= 1, E=F+G, F=B, G=C, H= 0, new8(A,B,D,I).
new7(A,B,C,A,B,C) :- D= 0, E=< 0, E=F+G, F=B, G=C, H= 0, new8(A,B,D,I).
new6(A,B,C,D,E,F) :- G>= 1, G=C, H= 0, I= 3, new10(I,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G=< 0, G=C, H= 0, I= 1, new10(A,B,I,D,E,F).
new4(A,B,C,D,E,F) :- G=H, new6(A,B,I,J,K,H), new7(J,K,G,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, I= 2, new3(I,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G=< 0, G=B, H= 0, I= 0, new3(A,I,C,D,E,F).
new1(A,B,C,D) :- new2(A,B,E,C,D,F).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
