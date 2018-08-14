new18(A,B,C,C) :- D>= 1, D=C, E= 0.
new18(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new17(A,B,C,A,B,C) :- D= 1, E=<F, E=A, F=C, new18(A,B,D,G).
new17(A,B,C,A,B,C) :- D= 0, E>=F+ 1, E=A, F=C, new18(A,B,D,G).
new16(A,B,C,D,E,F) :- G=C, H=A, new10(A,B,G,H,I,J,K,L), new14(I,J,C,D,E,F).
new15(A,B,C,D,E,F) :- G+ 1=<H, G=A, H=B, I=J+ 1, J=A, K= 1, new16(I,B,C,D,E,F).
new15(A,B,C,D,E,F) :- G>=H, G=A, H=B, new17(A,B,C,D,E,F).
new14(A,B,C,D,E,F) :- new15(A,B,C,D,E,F).
new13(A,B,C,D,E,F) :- new14(A,B,C,D,E,F).
new11(A,B,C,D) :- new13(A,B,E,C,D,F).
safe :- init(A,B), new11(A,B,C,D).
new10(A,B,C,D,A,B,E,D) :- E=F+G, F=C, G=D.
new8(A,B,C,C).
new7(A,B,C,D) :- E= 0, E=C, F= 0, new8(A,B,C,D).
new6(A,B,C,A,B,C) :- D= 1, E=<F, E=A, F=C, new7(A,B,D,G).
new6(A,B,C,A,B,C) :- D= 0, E>=F+ 1, E=A, F=C, new7(A,B,D,G).
new5(A,B,C,D,E,F) :- G=C, H=A, new10(A,B,G,H,I,J,K,L), new3(I,J,C,D,E,F).
new4(A,B,C,D,E,F) :- G+ 1=<H, G=A, H=B, I=J+ 1, J=A, K= 1, new5(I,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G>=H, G=A, H=B, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B,C,D) :- new2(A,B,E,C,D,F).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
