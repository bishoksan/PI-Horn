new26(A,B,C,D,E,F,G,H,I,J) :- K=L- 1, L=A, M= 1, N=O+ 1, O=C, P= 1, 
          new17(K,B,N,D,E,F,G,H,I,J).
new25(A,B,C,C).
new23(A,B,C,D) :- E>= 1, E=C, F= 0, new25(A,B,C,D).
new23(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new25(A,B,C,D).
new22(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=D, L= 0, M= 1, new21(A,B,C,D,M,F,G,H,I,J).
new22(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=D, L= 0, M= 0, 
          new21(A,B,C,D,M,F,G,H,I,J).
new22(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=D, L= 0, M= 0, 
          new21(A,B,C,D,M,F,G,H,I,J).
new21(A,B,C,D,E,A,B,C,D,E) :- F=E, new23(A,B,F,G).
new20(A,B,C,D,E,F,G,H,I,J) :- K=L, K=D, L=M* 2, M=B, N= 2, O= 1, 
          new21(A,B,C,D,O,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J) :- K>=L+ 1, K=D, L=M* 2, M=B, N= 2, 
          new22(A,B,C,D,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=M* 2, M=B, N= 2, 
          new22(A,B,C,D,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=A, M=N+ 2, N=D, O= 2, 
          new26(A,B,C,M,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=C, L=A, new26(A,B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- K=<L, K=C, L=B, new19(A,B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- K>=L+ 1, K=C, L=B, new20(A,B,C,D,E,F,G,H,I,J).
new17(A,B,C,D,E,F,G,H,I,J) :- new18(A,B,C,D,E,F,G,H,I,J).
new16(A,B,C,D,E,F,G,H,I,J) :- K= 0, L= 1, new17(A,B,L,K,E,F,G,H,I,J).
new14(A,B,C,D) :- new16(A,B,E,F,G,C,D,H,I,J).
safe :- init(A,B), new14(A,B,C,D).
new13(A,B,C,D,E,F,G,H,I,J) :- K=L- 1, L=A, M= 1, N=O+ 1, O=C, P= 1, 
          new3(K,B,N,D,E,F,G,H,I,J).
new12(A,B).
new10(A,B,C,C) :- new12(A,B).
new9(A,B,C,D) :- E= 0, E=C, F= 0, new10(A,B,C,D).
new8(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=D, L= 0, M= 1, new7(A,B,C,D,M,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=D, L= 0, M= 0, new7(A,B,C,D,M,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=D, L= 0, M= 0, 
          new7(A,B,C,D,M,F,G,H,I,J).
new7(A,B,C,D,E,A,B,C,D,E) :- F=E, new9(A,B,F,G).
new6(A,B,C,D,E,F,G,H,I,J) :- K=L, K=D, L=M* 2, M=B, N= 2, O= 1, 
          new7(A,B,C,D,O,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K>=L+ 1, K=D, L=M* 2, M=B, N= 2, 
          new8(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=M* 2, M=B, N= 2, 
          new8(A,B,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=A, M=N+ 2, N=D, O= 2, 
          new13(A,B,C,M,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=C, L=A, new13(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K=<L, K=C, L=B, new5(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K>=L+ 1, K=C, L=B, new6(A,B,C,D,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- new4(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K= 0, L= 1, new3(A,B,L,K,E,F,G,H,I,J).
new1(A,B,C,D) :- new2(A,B,E,F,G,C,D,H,I,J).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
