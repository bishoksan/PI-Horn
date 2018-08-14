new18(A,B,C,D,E,F) :- G=:=1, H=:=I, H=:=C, C>=0, I=:=J+K, J=:=A, A>=0, K=:=B, 
          B>=0, new7(A,B,C,G,L), new16(A,B,C,D,E,F).
new18(A,B,C,D,E,F) :- G=:=0, H>=I+1, H=:=C, C>=0, I=:=J+K, J=:=A, A>=0, K=:=B,
          B>=0, new7(A,B,C,G,L), new16(A,B,C,D,E,F).
new18(A,B,C,D,E,F) :- G=:=0, H+1=<I, H=:=C, C>=0, I=:=J+K, J=:=A, A>=0, K=:=B,
B>=0, new7(A,B,C,G,L), new16(A,B,C,D,E,F).
new17(A,B,C,D,E,F) :- G+1=<H, G=:=C, C>=0, H=:=268435455, I=:=J+K, J=:=A, A>=0,
          K=:=1, L=:=M+N, M=:=B, B>=0, N=:=2, O=:=P+Q, P=:=C, C>=0, Q=:=3, 
          new18(I,L,O,D,E,F).
new17(A,B,C,A,B,C) :- D>=E, D=:=C, C>=0, E=:=268435455.
new16(A,B,C,D,E,F) :- new17(A,B,C,D,E,F).
new15(A,B,C,D,E,F) :- new16(A,B,C,D,E,F).
new13(A,B,C,D,E,F) :- new15(A,B,C,D,E,F).
safe :- init(A,B,C), new13(A,B,C,D,E,F).
new12(A,B,C).
new10(A,B,C,D,D) :- new12(A,B,C).
new9(A,B,C,D,D).
new7(A,B,C,D,E) :- F>=G+1, F=:=D, G=:=0, new9(A,B,C,D,E).
new7(A,B,C,D,E) :- F+1=<G, F=:=D, G=:=0, new9(A,B,C,D,E).
new6(A,B,C,D,E) :- F=:=G, F=:=D, G=:=0, new10(A,B,C,D,E).
new5(A,B,C,A,B,C) :- D=:=1, E=:=F, E=:=C, C>=0, F=:=G+H, G=:=A, A>=0, H=:=B, 
          B>=0, new6(A,B,C,D,I).
new5(A,B,C,D,E,F) :- G=:=1, H=:=I, H=:=C, C>=0, I=:=J+K, J=:=A, A>=0, K=:=B, 
          B>=0, new7(A,B,C,G,L), new3(A,B,C,D,E,F).
new5(A,B,C,A,B,C) :- D=:=0, E>=F+1, E=:=C, C>=0, F=:=G+H, G=:=A, A>=0, H=:=B,
          B>=0, new6(A,B,C,D,I).
new5(A,B,C,A,B,C) :- D=:=0, E+1=<F, E=:=C, C>=0, F=:=G+H, G=:=A, A>=0, H=:=B,
          B>=0, new6(A,B,C,D,I).
new5(A,B,C,D,E,F) :- G=:=0, H>=I+1, H=:=C, C>=0, I=:=J+K, J=:=A, A>=0, K=:=B,
          B>=0, new7(A,B,C,G,L), new3(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- G=:=0, H+1=<I, H=:=C, C>=0, I=:=J+K, J=:=A, A>=0, K=:=B,
          B>=0, new7(A,B,C,G,L), new3(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G+1=<H, G=:=C, C>=0, H=:=268435455, I=:=J+K, J=:=A, A>=0, 
          K=:=1, L=:=M+N, M=:=B, B>=0, N=:=2, O=:=P+Q, P=:=C, C>=0, Q=:=3, 
          new5(I,L,O,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B,C,D,E,F) :- new2(A,B,C,D,E,F).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
