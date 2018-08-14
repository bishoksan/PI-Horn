new21(A,B,C,D,E,F,G,H,I,J) :- K=L+ 1, L=D, M= 1, new16(A,B,C,K,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J) :- K= 1, L=M, L=N+O, N=B, O=C, M= 3*P, Q= 3, P=A, 
          new9(A,B,C,K,R), new17(A,B,C,D,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J) :- K= 0, L>=M+ 1, L=N+O, N=B, O=C, M= 3*P, Q= 3, 
          P=A, new9(A,B,C,K,R), new17(A,B,C,D,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J) :- K= 0, L+ 1=<M, L=N+O, N=B, O=C, M= 3*P, Q= 3, 
          P=A, new9(A,B,C,K,R), new17(A,B,C,D,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, M=N+ 1, N=B, O= 1, P=Q+ 2, Q=C, 
          R= 2, new21(A,M,P,D,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=E, L= 0, M=N+ 1, N=B, O= 1, P=Q+ 2, 
          Q=C, R= 2, new21(A,M,P,D,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, M=N+ 2, N=B, O= 2, P=Q+ 1, Q=C, 
          R= 1, new21(A,M,P,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=A, new19(A,B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=D, L=A, new20(A,B,C,D,E,F,G,H,I,J).
new17(A,B,C,D,E,A,B,C,D,E).
new16(A,B,C,D,E,F,G,H,I,J) :- new18(A,B,C,D,E,F,G,H,I,J).
new15(A,B,C,D,E,F,G,H,I,J) :- K>= 0, K=A, L= 0, M= 0, 
          new16(A,B,C,M,E,F,G,H,I,J).
new15(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=A, L= 0, new17(A,B,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,F) :- new15(A,B,C,G,H,D,E,F,I,J).
safe :- init(A,B,C), new13(A,B,C,D,E,F).
new12(A,B,C,D,E,F,G,H,I,J) :- K=L+ 1, L=D, M= 1, new3(A,B,C,K,E,F,G,H,I,J).
new11(A,B,C,D,D).
new9(A,B,C,D,D) :- E>= 1, E=D, F= 0.
new9(A,B,C,D,D) :- E+ 1=< 0, E=D, F= 0.
new8(A,B,C,D,E) :- F= 0, F=D, G= 0, new11(A,B,C,D,E).
new7(A,B,C,D,E,A,B,C,D,E) :- F= 1, G=H, G=I+J, I=B, J=C, H= 3*K, L= 3, K=A, 
          new8(A,B,C,F,M).
new7(A,B,C,D,E,A,B,C,D,E) :- F= 0, G>=H+ 1, G=I+J, I=B, J=C, H= 3*K, L= 3, K=A, 
          new8(A,B,C,F,M).
new7(A,B,C,D,E,A,B,C,D,E) :- F= 0, G+ 1=<H, G=I+J, I=B, J=C, H= 3*K, L= 3, K=A, 
          new8(A,B,C,F,M).
new6(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, M=N+ 1, N=B, O= 1, P=Q+ 2, Q=C, 
          R= 2, new12(A,M,P,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=E, L= 0, M=N+ 1, N=B, O= 1, P=Q+ 2, 
          Q=C, R= 2, new12(A,M,P,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, M=N+ 2, N=B, O= 2, P=Q+ 1, Q=C, 
          R= 1, new12(A,M,P,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=A, new6(A,B,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=D, L=A, new7(A,B,C,D,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- new5(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K>= 0, K=A, L= 0, M= 0, new3(A,B,C,M,E,F,G,H,I,J).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,H,D,E,F,I,J).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
