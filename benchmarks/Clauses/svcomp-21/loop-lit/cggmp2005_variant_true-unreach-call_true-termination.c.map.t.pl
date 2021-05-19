new20(A,B,C,C).
new18(A,B,C,D) :- E>=F+1, E=:=C, F=:=0, new20(A,B,C,D).
new18(A,B,C,D) :- E+1=<F, E=:=C, F=:=0, new20(A,B,C,D).
new17(A,B,C,A,B,C) :- D=:=1, E=:=F, E=:=A, F=:=B, new18(A,B,D,G).
new17(A,B,C,A,B,C) :- D=:=0, E>=F+1, E=:=A, F=:=B, new18(A,B,D,G).
new17(A,B,C,A,B,C) :- D=:=0, E+1=<F, E=:=A, F=:=B, new18(A,B,D,G).
new16(A,B,C,D,E,F) :- G>=H+1, G=:=C, H=:=0, I=:=J+K, J=:=A, K=:=1, L=:=M-N, 
          M=:=B, N=:=1, O=:=P-Q, P=:=C, Q=:=1, new15(I,L,O,D,E,F).
new16(A,B,C,D,E,F) :- G=<H, G=:=C, H=:=0, new17(A,B,C,D,E,F).
new15(A,B,C,D,E,F) :- new16(A,B,C,D,E,F).
new14(A,B,C,D,E,F) :- G=:=2*I, H=:=2, I=:=J, new15(A,G,J,D,E,F).
new13(A,B,C,D,E,F) :- new14(A,B,C,D,E,F).
new11(A,B,C,D) :- new13(A,B,E,C,D,F).
safe :- init(A,B), new11(A,B,C,D).
new10(A,B).
new8(A,B,C,C) :- new10(A,B).
new7(A,B,C,D) :- E=:=F, E=:=C, F=:=0, new8(A,B,C,D).
new6(A,B,C,A,B,C) :- D=:=1, E=:=F, E=:=A, F=:=B, new7(A,B,D,G).
new6(A,B,C,A,B,C) :- D=:=0, E>=F+1, E=:=A, F=:=B, new7(A,B,D,G).
new6(A,B,C,A,B,C) :- D=:=0, E+1=<F, E=:=A, F=:=B, new7(A,B,D,G).
new5(A,B,C,D,E,F) :- G>=H+1, G=:=C, H=:=0, I=:=J+K, J=:=A, K=:=1, L=:=M-N, 
          M=:=B, N=:=1, O=:=P-Q, P=:=C, Q=:=1, new4(I,L,O,D,E,F).
new5(A,B,C,D,E,F) :- G=<H, G=:=C, H=:=0, new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- new5(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G=:=2*I, H=:=2, I=:=J, new4(A,G,J,D,E,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B,C,D) :- new2(A,B,E,C,D,F).
init(A,B). % :- A=:=0, B=:=0.
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
