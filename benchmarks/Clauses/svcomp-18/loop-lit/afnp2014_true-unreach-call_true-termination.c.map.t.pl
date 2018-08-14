new24(A,B,C,D,E,F) :- G>=H+1, G=:=C, H=:=0, I=:=J+K, J=:=A, K=:=B, L=:=M+N,
          M=:=B, N=:=1, new16(I,L,C,D,E,F).
new24(A,B,C,D,E,F) :- G+1=<H, G=:=C, H=:=0, I=:=J+K, J=:=A, K=:=B, L=:=M+N,
          M=:=B, N=:=1, new16(I,L,C,D,E,F).
new24(A,B,C,D,E,F) :- G=:=H, G=:=C, H=:=0, new19(A,B,C,D,E,F).
new23(A,B,C,C).
new21(A,B,C,D) :- E>=F+1, E=:=C, F=:=0, new23(A,B,C,D).
new21(A,B,C,D) :- E+1=<F, E=:=C, F=:=0, new23(A,B,C,D).
new20(A,B,C,A,B,C) :- D=:=1, E>=F, E=:=A, F=:=B, new21(A,B,D,G).
new20(A,B,C,A,B,C) :- D=:=0, E+1=<F, E=:=A, F=:=B, new21(A,B,D,G).
new19(A,B,C,D,E,F) :- new20(A,B,C,D,E,F).
new18(A,B,C,D,E,F) :- new24(A,B,G,D,E,F).
new17(A,B,C,D,E,F) :- G+1=<H, G=:=B, H=:=1000, new18(A,B,C,D,E,F).
new17(A,B,C,D,E,F) :- G>=H, G=:=B, H=:=1000, new19(A,B,C,D,E,F).
new16(A,B,C,D,E,F) :- new17(A,B,C,D,E,F).
new15(A,B,C,D,E,F) :- new16(A,B,C,D,E,F).
new13(A,B,C,D) :- new15(A,B,E,C,D,F).
safe :- init(A,B), new13(A,B,C,D).
new12(A,B,C,D,E,F) :- G>=H+1, G=:=C, H=:=0, I=:=J+K, J=:=A, K=:=B, L=:=M+N,
          M=:=B, N=:=1, new3(I,L,C,D,E,F).
new12(A,B,C,D,E,F) :- G+1=<H, G=:=C, H=:=0, I=:=J+K, J=:=A, K=:=B, L=:=M+N,
          M=:=B, N=:=1, new3(I,L,C,D,E,F).
new12(A,B,C,D,E,F) :- G=:=H, G=:=C, H=:=0, new6(A,B,C,D,E,F).
new11(A,B).
new9(A,B,C,C) :- new11(A,B).
new8(A,B,C,D) :- E=:=F, E=:=C, F=:=0, new9(A,B,C,D).
new7(A,B,C,A,B,C) :- D=:=1, E>=F, E=:=A, F=:=B, new8(A,B,D,G).
new7(A,B,C,A,B,C) :- D=:=0, E+1=<F, E=:=A, F=:=B, new8(A,B,D,G).
new6(A,B,C,D,E,F) :- new7(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- new12(A,B,G,D,E,F).
new4(A,B,C,D,E,F) :- G+1=<H, G=:=B, H=:=1000, new5(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G>=H, G=:=B, H=:=1000, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B,C,D) :- new2(A,B,E,C,D,F).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
