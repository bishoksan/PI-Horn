new18(A,B,C,A,B,C) :- D=E, D=B, E=C.
new17(A,B,C,D,E,F) :- G>=H+ 1, G=B, H=A, I=J- 1, J=C, K= 1, new15(A,B,I,D,E,F).
new17(A,B,C,D,E,F) :- G=<H, G=B, H=A, I=J+ 2, J=C, K= 2, new15(A,B,I,D,E,F).
new16(A,B,C,D,E,F) :- G+ 1=<H, G=B, H= 2*I, J= 2, I=A, K=L+ 1, L=B, M= 1, 
          new17(A,K,C,D,E,F).
new16(A,B,C,D,E,F) :- G>=H, G=B, H= 2*I, J= 2, I=A, new18(A,B,C,D,E,F).
new15(A,B,C,D,E,F) :- new16(A,B,C,D,E,F).
new9(A,B,C,D,E,F) :- G= 0, H= 0, new15(A,G,H,D,E,F).
new7(A,B) :- new9(A,C,D,B,E,F).
safe :- init(A), new7(A,B).
new6(A,B,C,A,B,C) :- D>=E+ 1, D=B, E=C.
new6(A,B,C,A,B,C) :- D+ 1=<E, D=B, E=C.
new5(A,B,C,D,E,F) :- G>=H+ 1, G=B, H=A, I=J- 1, J=C, K= 1, new3(A,B,I,D,E,F).
new5(A,B,C,D,E,F) :- G=<H, G=B, H=A, I=J+ 2, J=C, K= 2, new3(A,B,I,D,E,F).
new4(A,B,C,D,E,F) :- G+ 1=<H, G=B, H= 2*I, J= 2, I=A, K=L+ 1, L=B, M= 1, 
          new5(A,K,C,D,E,F).
new4(A,B,C,D,E,F) :- G>=H, G=B, H= 2*I, J= 2, I=A, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G= 0, H= 0, new3(A,G,H,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
