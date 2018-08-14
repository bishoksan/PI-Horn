new26(A,B,C,D,E,F) :- G=<H, G=:=A, H=:=3, I=:=J+K, J=:=A, K=:=1, 
          new25(I,B,C,D,E,F).
new26(A,B,C,D,E,F) :- G>=H+1, G=:=A, H=:=3, new17(A,B,C,D,E,F).
new25(A,B,C,D,E,F) :- new26(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- G+1=<H, G=:=C, H=:=20, I=:=J+K, J=:=B, K=:=C, 
          new19(A,B,I,D,E,F).
new24(A,B,C,D,E,F) :- G>=H, G=:=C, H=:=20, new25(A,B,C,D,E,F).
new23(A,B,B).
new21(A,B,C) :- D>=E+1, D=:=B, E=:=0, new23(A,B,C).
new21(A,B,C) :- D+1=<E, D=:=B, E=:=0, new23(A,B,C).
new20(A,B,C,A,B,C) :- D=:=1, E=:=F, E=:=A, F=:=4, new21(A,D,G).
new20(A,B,C,A,B,C) :- D=:=0, E>=F+1, E=:=A, F=:=4, new21(A,D,G).
new20(A,B,C,A,B,C) :- D=:=0, E+1=<F, E=:=A, F=:=4, new21(A,D,G).
new19(A,B,C,D,E,F) :- new24(A,B,C,D,E,F).
new18(A,B,C,D,E,F) :- G=<H, G=:=B, H=:=100, I=:=J+K, J=:=B, K=:=1, 
          new19(A,I,C,D,E,F).
new18(A,B,C,D,E,F) :- G>=H+1, G=:=B, H=:=100, new20(A,B,C,D,E,F).
new17(A,B,C,D,E,F) :- new18(A,B,C,D,E,F).
new16(A,B,C,D,E,F) :- G=:=0, H=:= -100, new17(A,G,H,D,E,F).
new14(A,B) :- new16(A,C,D,B,E,F).
safe :- init(A), new14(A,B).
new13(A,B,C,D,E,F) :- G=<H, G=:=A, H=:=3, I=:=J+K, J=:=A, K=:=1, 
          new12(I,B,C,D,E,F).
new13(A,B,C,D,E,F) :- G>=H+1, G=:=A, H=:=3, new3(A,B,C,D,E,F).
new12(A,B,C,D,E,F) :- new13(A,B,C,D,E,F).
new11(A,B,C,D,E,F) :- G+1=<H, G=:=C, H=:=20, I=:=J+K, J=:=B, K=:=C, 
          new5(A,B,I,D,E,F).
new11(A,B,C,D,E,F) :- G>=H, G=:=C, H=:=20, new12(A,B,C,D,E,F).
new10(A).
new8(A,B,B) :- new10(A).
new7(A,B,C) :- D=:=E, D=:=B, E=:=0, new8(A,B,C).
new6(A,B,C,A,B,C) :- D=:=1, E=:=F, E=:=A, F=:=4, new7(A,D,G).
new6(A,B,C,A,B,C) :- D=:=0, E>=F+1, E=:=A, F=:=4, new7(A,D,G).
new6(A,B,C,A,B,C) :- D=:=0, E+1=<F, E=:=A, F=:=4, new7(A,D,G).
new5(A,B,C,D,E,F) :- new11(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G=<H, G=:=B, H=:=100, I=:=J+K, J=:=B, K=:=1, 
          new5(A,I,C,D,E,F).
new4(A,B,C,D,E,F) :- G>=H+1, G=:=B, H=:=100, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G=:=0, H=:= -100, new3(A,G,H,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A). % :- A=:=0.
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
