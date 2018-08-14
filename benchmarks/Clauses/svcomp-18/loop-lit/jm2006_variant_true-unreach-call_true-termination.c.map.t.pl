new26(A,B,C,D,E,A,B,C,D,E).
new25(A,B,C,D,E,F,G,H,I,J) :- K=:=1, L=:=M, L=:=A, M=:= -1*O, N=:= -1, O=:=B,
          new11(A,B,K,P), new26(A,B,C,D,E,F,G,H,I,J).
new25(A,B,C,D,E,F,G,H,I,J) :- K=:=0, L>=M+1, L=:=A, M=:= -1*O, N=:= -1, O=:=B,
          new11(A,B,K,P), new26(A,B,C,D,E,F,G,H,I,J).
new25(A,B,C,D,E,F,G,H,I,J) :- K=:=0, L+1=<M, L=:=A, M=:= -1*O, N=:= -1, O=:=B,
          new11(A,B,K,P), new26(A,B,C,D,E,F,G,H,I,J).
new24(A,B,C,D,E,F,G,H,I,J) :- K=:=L, K=:=C, L=:=D, new25(A,B,C,D,E,F,G,H,I,J).
new24(A,B,C,D,E,F,G,H,I,J) :- K>=L+1, K=:=C, L=:=D, new26(A,B,C,D,E,F,G,H,I,J).
new24(A,B,C,D,E,F,G,H,I,J) :- K+1=<L, K=:=C, L=:=D, new26(A,B,C,D,E,F,G,H,I,J).
new23(A,B,C,D,E,F,G,H,I,J) :- K>=L+1, K=:=E, L=:=0, M=:=N-O, N=:=E, O=:=1,
          P=:=Q-R, Q=:=A, R=:=2, S=:=T+U, T=:=B, U=:=1, 
          new22(P,S,C,D,M,F,G,H,I,J).
new23(A,B,C,D,E,F,G,H,I,J) :- K+1=<L, K=:=E, L=:=0, M=:=N-O, N=:=E, O=:=1,
          P=:=Q-R, Q=:=A, R=:=2, S=:=T+U, T=:=B, U=:=1, 
          new22(P,S,C,D,M,F,G,H,I,J).
new23(A,B,C,D,E,F,G,H,I,J) :- K=:=L, K=:=E, L=:=0, new24(A,B,C,D,E,F,G,H,I,J).
new22(A,B,C,D,E,F,G,H,I,J) :- new23(A,B,C,D,E,F,G,H,I,J).
new21(A,B,C,D,E,F,G,H,I,J) :- K=:=C, L=:=M, new22(L,B,C,M,K,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J) :- new21(A,B,K,D,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :- new20(A,B,C,D,E,F,G,H,I,J).
new17(A,B,C,D) :- new19(A,B,E,F,G,C,D,H,I,J).
safe :- init(A,B), new17(A,B,C,D).
new16(A,B).
new14(A,B,C,C) :- new16(A,B).
new13(A,B,C,C).
new11(A,B,C,D) :- E>=F+1, E=:=C, F=:=0, new13(A,B,C,D).
new11(A,B,C,D) :- E+1=<F, E=:=C, F=:=0, new13(A,B,C,D).
new10(A,B,C,D) :- E=:=F, E=:=C, F=:=0, new14(A,B,C,D).
new8(A,B,C,D,E,A,B,C,D,E) :- F=:=1, G=:=H, G=:=A, H=:= -1*J, I=:= -1, J=:=B,
          new10(A,B,F,K).
new8(A,B,C,D,E,A,B,C,D,E) :- F=:=0, G>=H+1, G=:=A, H=:= -1*J, I=:= -1, J=:=B,
          new10(A,B,F,K).
new8(A,B,C,D,E,A,B,C,D,E) :- F=:=0, G+1=<H, G=:=A, H=:= -1*J, I=:= -1, J=:=B,
          new10(A,B,F,K).
new7(A,B,C,D,E,F,G,H,I,J) :- K=:=L, K=:=C, L=:=D, new8(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K>=L+1, K=:=E, L=:=0, M=:=N-O, N=:=E, O=:=1,
          P=:=Q-R, Q=:=A, R=:=2, S=:=T+U, T=:=B, U=:=1, 
          new5(P,S,C,D,M,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K+1=<L, K=:=E, L=:=0, M=:=N-O, N=:=E, O=:=1,
          P=:=Q-R, Q=:=A, R=:=2, S=:=T+U, T=:=B, U=:=1, 
          new5(P,S,C,D,M,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K=:=L, K=:=E, L=:=0, new7(A,B,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- new6(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K=:=C, L=:=M, new5(L,B,C,M,K,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- new4(A,B,K,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- new3(A,B,C,D,E,F,G,H,I,J).
new1(A,B,C,D) :- new2(A,B,E,F,G,C,D,H,I,J).
init(A,B). % :- A=:=0, B=:=0.
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
