new24(A,B,C,D,E,F,G,H) :- I>=J, I=:=A, J=:=1, K=:=1, new23(A,B,C,K,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I+1=<J, I=:=A, J=:=1, K=:=0, new23(A,B,C,K,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- I=:=D, new9(A,B,I,J), new19(A,B,C,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I=<J, I=:=K+L, K=:=A, L=:=B, J=:=2, 
          new24(A,B,C,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I>=J+1, I=:=K+L, K=:=A, L=:=B, J=:=2, M=:=0, 
          new23(A,B,C,M,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I=<J, I=:=1, J=:=K+L, K=:=A, L=:=B, 
          new22(A,B,C,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I>=J+1, I=:=1, J=:=K+L, K=:=A, L=:=B, M=:=0, 
          new23(A,B,C,M,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I+1=<J, I=:=A, J=:=1000000, K=:=L+M, L=:=A, M=:=1, 
          N=:=O+P, O=:=C, P=:=B, Q=:=R-S, R=:=B, S=:=1, new21(K,Q,N,D,E,F,G,H).
new20(A,B,C,D,A,B,C,D) :- E>=F, E=:=A, F=:=1000000.
new19(A,B,C,D,E,F,G,H) :- new20(A,B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I=:=1, J=:=1, new19(I,B,J,D,E,F,G,H).
new16(A,B,C,D) :- new18(A,B,E,F,C,D,G,H).
safe :- init(A,B), new16(A,B,C,D).
new15(A,B,C,D,E,F,G,H) :- I>=J, I=:=A, J=:=1, K=:=1, new7(A,B,C,K,E,F,G,H).
new15(A,B,C,D,E,F,G,H) :- I+1=<J, I=:=A, J=:=1, K=:=0, new7(A,B,C,K,E,F,G,H).
new14(A,B).
new12(A,B,C,C) :- new14(A,B).
new11(A,B,C,C).
new9(A,B,C,D) :- E>=F+1, E=:=C, F=:=0, new11(A,B,C,D).
new9(A,B,C,D) :- E+1=<F, E=:=C, F=:=0, new11(A,B,C,D).
new8(A,B,C,D) :- E=:=F, E=:=C, F=:=0, new12(A,B,C,D).
new7(A,B,C,D,A,B,C,D) :- E=:=D, new8(A,B,E,F).
new7(A,B,C,D,E,F,G,H) :- I=:=D, new9(A,B,I,J), new3(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I=<J, I=:=K+L, K=:=A, L=:=B, J=:=2, 
          new15(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I>=J+1, I=:=K+L, K=:=A, L=:=B, J=:=2, M=:=0, 
          new7(A,B,C,M,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I=<J, I=:=1, J=:=K+L, K=:=A, L=:=B, 
          new6(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I>=J+1, I=:=1, J=:=K+L, K=:=A, L=:=B, M=:=0, 
          new7(A,B,C,M,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I+1=<J, I=:=A, J=:=1000000, K=:=L+M, L=:=A, M=:=1, 
          N=:=O+P, O=:=C, P=:=B, Q=:=R-S, R=:=B, S=:=1, new5(K,Q,N,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I=:=1, J=:=1, new3(I,B,J,D,E,F,G,H).
new1(A,B,C,D) :- new2(A,B,E,F,C,D,G,H).
init(A,B). % :- A=:=0, B=:=0.
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
