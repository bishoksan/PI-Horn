new18(A,B,B).
new16(A,B,C) :- D>=E+1, D=:=B, E=:=0, new18(A,B,C).
new16(A,B,C) :- D+1=<E, D=:=B, E=:=0, new18(A,B,C).
new15(A,B,A,B) :- C=:=1, D=:=E, D=:=A, E=:=6, new16(A,C,F).
new15(A,B,A,B) :- C=:=0, D>=E+1, D=:=A, E=:=6, new16(A,C,F).
new15(A,B,A,B) :- C=:=0, D+1=<E, D=:=A, E=:=6, new16(A,C,F).
new14(A,B,C,D) :- E>=F, E=:=A, F=:=B, G=:=H+I, H=:=B, I=:=2, J=:=K+L, K=:= -1, 
          L=:=A, new13(J,G,C,D).
new14(A,B,C,D) :- E+1=<F, E=:=A, F=:=B, new15(A,B,C,D).
new13(A,B,C,D) :- new14(A,B,C,D).
new12(A,B,C,D) :- E=:=1, new13(A,E,C,D).
new10(A,B) :- new12(A,C,B,D).
safe :- init(A), new10(A,B).
new9(A).
new7(A,B,B) :- new9(A).
new6(A,B,C) :- D=:=E, D=:=B, E=:=0, new7(A,B,C).
new5(A,B,A,B) :- C=:=1, D=:=E, D=:=A, E=:=6, new6(A,C,F).
new5(A,B,A,B) :- C=:=0, D>=E+1, D=:=A, E=:=6, new6(A,C,F).
new5(A,B,A,B) :- C=:=0, D+1=<E, D=:=A, E=:=6, new6(A,C,F).
new4(A,B,C,D) :- E>=F, E=:=A, F=:=B, G=:=H+I, H=:=B, I=:=2, J=:=K+L, K=:= -1, 
          L=:=A, new3(J,G,C,D).
new4(A,B,C,D) :- E+1=<F, E=:=A, F=:=B, new5(A,B,C,D).
new3(A,B,C,D) :- new4(A,B,C,D).
new2(A,B,C,D) :- E=:=1, new3(A,E,C,D).
new1(A,B) :- new2(A,C,B,D).
init(A). % :- A=:=0.
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
