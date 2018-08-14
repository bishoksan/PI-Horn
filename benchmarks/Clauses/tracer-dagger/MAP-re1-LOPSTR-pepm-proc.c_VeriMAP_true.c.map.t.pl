new21(A,B,C,A,B,C) :- D=<E, D=B, E=C.
new20(A,B,C,D,E,F) :- G>= 1, G=C, H= 0, I=J- 1, J=C, K= 1, L=M- 1, M=B, N= 1, 
          new19(A,L,I,D,E,F).
new20(A,B,C,D,E,F) :- G=< 0, G=C, H= 0, new21(A,B,C,D,E,F).
new19(A,B,C,D,E,F) :- new20(A,B,C,D,E,F).
new18(A,B,C,D,E,F) :- G+ 1=<H, G=C, H=A, I=J+ 1, J=C, K= 1, L=M+ 1, M=B, N= 1, 
          new17(A,L,I,D,E,F).
new18(A,B,C,D,E,F) :- G>=H, G=C, H=A, new19(A,B,C,D,E,F).
new17(A,B,C,D,E,F) :- new18(A,B,C,D,E,F).
new10(A,B,C,D,E,F) :- G= 0, new17(A,B,G,D,E,F).
new8(A,B,C,D) :- new10(A,B,E,C,D,F).
safe :- init(A,B), new8(A,B,C,D).
new7(A,B,C,A,B,C) :- D>=E+ 1, D=B, E=C.
new6(A,B,C,D,E,F) :- G>= 1, G=C, H= 0, I=J- 1, J=C, K= 1, L=M- 1, M=B, N= 1, 
          new5(A,L,I,D,E,F).
new6(A,B,C,D,E,F) :- G=< 0, G=C, H= 0, new7(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G+ 1=<H, G=C, H=A, I=J+ 1, J=C, K= 1, L=M+ 1, M=B, N= 1, 
          new3(A,L,I,D,E,F).
new4(A,B,C,D,E,F) :- G>=H, G=C, H=A, new5(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G= 0, new3(A,B,G,D,E,F).
new1(A,B,C,D) :- new2(A,B,E,C,D,F).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
