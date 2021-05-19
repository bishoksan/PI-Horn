new26(A,B,C,D,A,B,C,D).
new25(A,B,C,D,E,F,G,H) :- I=:=1, J=:=K, J=:=A, K=:=0, new11(A,I,L), 
          new26(A,B,C,D,E,F,G,H).
new25(A,B,C,D,E,F,G,H) :- I=:=0, J>=K+1, J=:=A, K=:=0, new11(A,I,L),
          new26(A,B,C,D,E,F,G,H).
new25(A,B,C,D,E,F,G,H) :- I=:=0, J+1=<K, J=:=A, K=:=0, new11(A,I,L),
          new26(A,B,C,D,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I=:=J, I=:=B, J=:=C, new25(A,B,C,D,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I>=J+1, I=:=B, J=:=C, new26(A,B,C,D,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I+1=<J, I=:=B, J=:=C, new26(A,B,C,D,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- I>=J+1, I=:=D, J=:=0, K=:=L-M, L=:=D, M=:=1, N=:=O-P,
          O=:=A, P=:=1, new22(N,B,C,K,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- I+1=<J, I=:=D, J=:=0, K=:=L-M, L=:=D, M=:=1, N=:=O-P,
          O=:=A, P=:=1, new22(N,B,C,K,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- I=:=J, I=:=D, J=:=0, new24(A,B,C,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- new23(A,B,C,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I=:=B, J=:=K, new22(J,B,K,I,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- new21(A,I,C,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- new20(A,B,C,D,E,F,G,H).
new17(A,B) :- new19(A,C,D,E,B,F,G,H).
safe :- init(A), new17(A,B).
new16(A).
new14(A,B,B) :- new16(A).
new13(A,B,B).
new11(A,B,C) :- D>=E+1, D=:=B, E=:=0, new13(A,B,C).
new11(A,B,C) :- D+1=<E, D=:=B, E=:=0, new13(A,B,C).
new10(A,B,C) :- D=:=E, D=:=B, E=:=0, new14(A,B,C).
new8(A,B,C,D,A,B,C,D) :- E=:=1, F=:=G, F=:=A, G=:=0, new10(A,E,H).
new8(A,B,C,D,A,B,C,D) :- E=:=0, F>=G+1, F=:=A, G=:=0, new10(A,E,H).
new8(A,B,C,D,A,B,C,D) :- E=:=0, F+1=<G, F=:=A, G=:=0, new10(A,E,H).
new7(A,B,C,D,E,F,G,H) :- I=:=J, I=:=B, J=:=C, new8(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I>=J+1, I=:=D, J=:=0, K=:=L-M, L=:=D, M=:=1, N=:=O-P,
          O=:=A, P=:=1, new5(N,B,C,K,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I=<J+1, I=:=D, J=:=0, K=:=L-M, L=:=D, M=:=1, N=:=O-P,
          O=:=A, P=:=1, new5(N,B,C,K,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I=:=J, I=:=D, J=:=0, new7(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- new6(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I=:=B, J=:=K, new5(J,B,K,I,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,I,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,B,C,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A). % :- A=:=0.
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
