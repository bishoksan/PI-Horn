new18(A,B,B).
new16(A,B,C) :- D>=E+1, D=:=B, E=:=0, new18(A,B,C).
new16(A,B,C) :- D+1=<E, D=:=B, E=:=0, new18(A,B,C).
new15(A,B,A,B) :- C=:=1, D>=E+1, D=:=A, E=:=0, new16(A,C,F).
new15(A,B,A,B) :- C=:=0, D=<E, D=:=A, E=:=0, new16(A,C,F).
new14(A,B,C,D) :- E+1=<F, E=:=B, F=:=0, G=:=H+I, H=:=B, I=:=A, J=:=K+L, K=:=A, 
          L=:=1, new13(J,G,C,D).
new14(A,B,C,D) :- E>=F, E=:=B, F=:=0, new15(A,B,C,D).
new13(A,B,C,D) :- new14(A,B,C,D).
new12(A,B,C,D) :- E=:= -50, new13(A,E,C,D).
new10(A,B) :- new12(A,C,B,D).
safe :- init(A), new10(A,B).
new9(A).
new7(A,B,B) :- new9(A).
new6(A,B,C) :- D=:=E, D=:=B, E=:=0, new7(A,B,C).
new5(A,B,A,B) :- C=:=1, D>=E+1, D=:=A, E=:=0, new6(A,C,F).
new5(A,B,A,B) :- C=:=0, D=<E, D=:=A, E=:=0, new6(A,C,F).
new4(A,B,C,D) :- E+1=<F, E=:=B, F=:=0, G=:=H+I, H=:=B, I=:=A, J=:=K+L, K=:=A, 
          L=:=1, new3(J,G,C,D).
new4(A,B,C,D) :- E>=F, E=:=B, F=:=0, new5(A,B,C,D).
new3(A,B,C,D) :- new4(A,B,C,D).
new2(A,B,C,D) :- E=:= -50, new3(A,E,C,D).
new1(A,B) :- new2(A,C,B,D).
init(A). % :- A=:=0.
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
