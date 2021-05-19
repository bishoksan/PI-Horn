new20(A,B,B).
new18(A,B,C) :- D>=E+1, D=:=B, E=:=0, new20(A,B,C).
new18(A,B,C) :- D+1=<E, D=:=B, E=:=0, new20(A,B,C).
new17(A,B,A,B) :- C=:=1, D=:=E, D=:=A, E=:=100, new18(A,C,F).
new17(A,B,A,B) :- C=:=0, D>=E+1, D=:=A, E=:=100, new18(A,C,F).
new17(A,B,A,B) :- C=:=0, D+1=<E, D=:=A, E=:=100, new18(A,C,F).
new16(A,B,C,D) :- E+1=<F, E=:=B, F=:=50, G=:=H+I, H=:=B, I=:=1, new14(A,G,C,D).
new16(A,B,C,D) :- E>=F, E=:=B, F=:=50, G=:=H+I, H=:=B, I=:=1, J=:=K+L, K=:=A, 
          L=:=1, new14(J,G,C,D).
new15(A,B,C,D) :- E+1=<F, E=:=B, F=:=100, new16(A,B,C,D).
new15(A,B,C,D) :- E>=F, E=:=B, F=:=100, new17(A,B,C,D).
new14(A,B,C,D) :- new15(A,B,C,D).
new13(A,B,C,D) :- E=:=0, new14(A,E,C,D).
new11(A,B) :- new13(A,C,B,D).
safe :- init(A), new11(A,B).
new10(A).
new8(A,B,B) :- new10(A).
new7(A,B,C) :- D=:=E, D=:=B, E=:=0, new8(A,B,C).
new6(A,B,A,B) :- C=:=1, D=:=E, D=:=A, E=:=100, new7(A,C,F).
new6(A,B,A,B) :- C=:=0, D>=E+1, D=:=A, E=:=100, new7(A,C,F).
new6(A,B,A,B) :- C=:=0, D+1=<E, D=:=A, E=:=100, new7(A,C,F).
new5(A,B,C,D) :- E+1=<F, E=:=B, F=:=50, G=:=H+I, H=:=B, I=:=1, new3(A,G,C,D).
new5(A,B,C,D) :- E>=F, E=:=B, F=:=50, G=:=H+I, H=:=B, I=:=1, J=:=K+L, K=:=A, 
          L=:=1, new3(J,G,C,D).
new4(A,B,C,D) :- E+1=<F, E=:=B, F=:=100, new5(A,B,C,D).
new4(A,B,C,D) :- E>=F, E=:=B, F=:=100, new6(A,B,C,D).
new3(A,B,C,D) :- new4(A,B,C,D).
new2(A,B,C,D) :- E=:=0, new3(A,E,C,D).
new1(A,B) :- new2(A,C,B,D).
init(A). % :- A=:=0.
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
