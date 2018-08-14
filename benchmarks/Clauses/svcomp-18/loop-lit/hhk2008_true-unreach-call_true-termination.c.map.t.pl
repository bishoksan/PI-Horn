new18(A,B,C,C).
new16(A,B,C,D) :- E>=F+1, E=:=C, F=:=0, new18(A,B,C,D).
new16(A,B,C,D) :- E+1=<F, E=:=C, F=:=0, new18(A,B,C,D).
new15(A,B,C,D,A,B,C,D) :- E=:=1, F=:=G, F=:=C, G=:=H+I, H=:=A, I=:=B, 
          new16(A,B,E,J).
new15(A,B,C,D,A,B,C,D) :- E=:=0, F>=G+1, F=:=C, G=:=H+I, H=:=A, I=:=B,
          new16(A,B,E,J).
new15(A,B,C,D,A,B,C,D) :- E=:=0, F+1=<G, F=:=C, G=:=H+I, H=:=A, I=:=B,
          new16(A,B,E,J).
new14(A,B,C,D,E,F,G,H) :- I>=J+1, I=:=D, J=:=0, K=:=L-M, L=:=D, M=:=1, N=:=O+P, 
          O=:=C, P=:=1, new13(A,B,N,K,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I=<J, I=:=D, J=:=0, new15(A,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- new14(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I=:=A, J=:=B, new13(A,B,I,J,E,F,G,H).
new10(A,B,C,D) :- new12(A,B,E,F,C,D,G,H).
safe :- init(A,B), new10(A,B,C,D).
new9(A,B).
new7(A,B,C,C) :- new9(A,B).
new6(A,B,C,D) :- E=:=F, E=:=C, F=:=0, new7(A,B,C,D).
new5(A,B,C,D,A,B,C,D) :- E=:=1, F=:=G, F=:=C, G=:=H+I, H=:=A, I=:=B, 
          new6(A,B,E,J).
new5(A,B,C,D,A,B,C,D) :- E=:=0, F>=G+1, F=:=C, G=:=H+I, H=:=A, I=:=B,
          new6(A,B,E,J).
new5(A,B,C,D,A,B,C,D) :- E=:=0, F+1=<G, F=:=C, G=:=H+I, H=:=A, I=:=B,
          new6(A,B,E,J).
new4(A,B,C,D,E,F,G,H) :- I>=J+1, I=:=D, J=:=0, K=:=L-M, L=:=D, M=:=1, N=:=O+P, 
          O=:=C, P=:=1, new3(A,B,N,K,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I=<J, I=:=D, J=:=0, new5(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I=:=A, J=:=B, new3(A,B,I,J,E,F,G,H).
new1(A,B,C,D) :- new2(A,B,E,F,C,D,G,H).
init(A,B). % :- A=:=0, B=:=0.
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
