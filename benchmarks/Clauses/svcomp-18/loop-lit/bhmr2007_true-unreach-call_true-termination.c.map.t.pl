new24(A,B,C,D,E,F,G,H,I,J) :- K=:=L+M, L=:=D, M=:=1, new16(A,B,C,K,E,F,G,H,I,J).
new23(A,B,C,D,E,F,G,H,I,J) :- K>=L+1, K=:=E, L=:=0, M=:=N+O, N=:=B, O=:=1,
          P=:=Q+R, Q=:=C, R=:=2, new24(A,M,P,D,E,F,G,H,I,J).
new23(A,B,C,D,E,F,G,H,I,J) :- K+1=<L, K=:=E, L=:=0, M=:=N+O, N=:=B, O=:=1,
          P=:=Q+R, Q=:=C, R=:=2, new24(A,M,P,D,E,F,G,H,I,J).
new23(A,B,C,D,E,F,G,H,I,J) :- K=:=L, K=:=E, L=:=0, M=:=N+O, N=:=B, O=:=2, 
          P=:=Q+R, Q=:=C, R=:=1, new24(A,M,P,D,E,F,G,H,I,J).
new22(A,B,C,D,D).
new20(A,B,C,D,E) :- F>=G+1, F=:=D, G=:=0, new22(A,B,C,D,E).
new20(A,B,C,D,E) :- F+1=<G, F=:=D, G=:=0, new22(A,B,C,D,E).
new19(A,B,C,D,E,A,B,C,D,E) :- F=:=1, G=:=H, G=:=I+J, I=:=B, J=:=C, H=:=3*L,
          K=:=3, L=:=A, new20(A,B,C,F,M).
new19(A,B,C,D,E,A,B,C,D,E) :- F=:=0, G>=H+1, G=:=I+J, I=:=B, J=:=C, H=:=3*L,
          K=:=3, L=:=A, new20(A,B,C,F,M).
new19(A,B,C,D,E,A,B,C,D,E) :- F=:=0, G+1=<H, G=:=I+J, I=:=B, J=:=C, H=:=3*L,
          K=:=3, L=:=A, new20(A,B,C,F,M).
new18(A,B,C,D,E,F,G,H,I,J) :- new23(A,B,C,D,K,F,G,H,I,J).
new17(A,B,C,D,E,F,G,H,I,J) :- K+1=<L, K=:=D, L=:=A, new18(A,B,C,D,E,F,G,H,I,J).
new17(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=:=D, L=:=A, new19(A,B,C,D,E,F,G,H,I,J).
new16(A,B,C,D,E,F,G,H,I,J) :- new17(A,B,C,D,E,F,G,H,I,J).
new15(A,B,C,D,E,F,G,H,I,J) :- K=:=0, new16(A,B,C,K,E,F,G,H,I,J).
new13(A,B,C,D,E,F) :- new15(A,B,C,G,H,D,E,F,I,J).
safe :- init(A,B,C), new13(A,B,C,D,E,F).
new12(A,B,C,D,E,F,G,H,I,J) :- K=:=L+M, L=:=D, M=:=1, new3(A,B,C,K,E,F,G,H,I,J).
new11(A,B,C,D,E,F,G,H,I,J) :- K>=L+1, K=:=E, L=:=0, M=:=N+O, N=:=B, O=:=1,
          P=:=Q+R, Q=:=C, R=:=2, new12(A,M,P,D,E,F,G,H,I,J).
new11(A,B,C,D,E,F,G,H,I,J) :- K+1=<L, K=:=E, L=:=0, M=:=N+O, N=:=B, O=:=1,
          P=:=Q+R, Q=:=C, R=:=2, new12(A,M,P,D,E,F,G,H,I,J).
new11(A,B,C,D,E,F,G,H,I,J) :- K=:=L, K=:=E, L=:=0, M=:=N+O, N=:=B, O=:=2, 
          P=:=Q+R, Q=:=C, R=:=1, new12(A,M,P,D,E,F,G,H,I,J).
new10(A,B,C).
new8(A,B,C,D,D) :- new10(A,B,C).
new7(A,B,C,D,E) :- F=:=G, F=:=D, G=:=0, new8(A,B,C,D,E).
new6(A,B,C,D,E,A,B,C,D,E) :- F=:=1, G=:=H, G=:=I+J, I=:=B, J=:=C, H=:=3*L,
          K=:=3, L=:=A, new7(A,B,C,F,M).
new6(A,B,C,D,E,A,B,C,D,E) :- F=:=0, G>=H+1, G=:=I+J, I=:=B, J=:=C, H=:=3*L,
          K=:=3, L=:=A, new7(A,B,C,F,M).
new6(A,B,C,D,E,A,B,C,D,E) :- F=:=0, G+1=<H, G=:=I+J, I=:=B, J=:=C, H=:=3*L,
          K=:=3, L=:=A, new7(A,B,C,F,M).
new5(A,B,C,D,E,F,G,H,I,J) :- new11(A,B,C,D,K,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K+1=<L, K=:=D, L=:=A, new5(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=:=D, L=:=A, new6(A,B,C,D,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- new4(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K=:=0, new3(A,B,C,K,E,F,G,H,I,J).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,H,D,E,F,I,J).
init(A,B,C). % :- A=:=0, B=:=0, C=:=0.
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
