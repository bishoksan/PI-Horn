new28(A,B,C,D,E,F,G,H,I,J) :- K=L+ 1, L=C, M= 1, new21(A,B,K,D,E,F,G,H,I,J).
new27(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, M=N+ 1, N=A, O= 1, 
          new28(M,B,C,D,E,F,G,H,I,J).
new27(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=E, L= 0, M=N+ 1, N=A, O= 1, 
          new28(M,B,C,D,E,F,G,H,I,J).
new27(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, new28(A,B,C,D,E,F,G,H,I,J).
new26(A,B,C,D,E,F,G,H,I,J) :- new27(A,B,C,D,K,F,G,H,I,J).
new25(A,B,C,D,E,F,G,H,I,J) :- K= 1,  1=<L, M= 1, L=D, N=O+ 1, O=D, P= 1, 
          new12(A,B,K,Q), new23(A,B,C,N,E,F,G,H,I,J).
new25(A,B,C,D,E,F,G,H,I,J) :- K= 0,  1>=L+ 1, M= 1, L=D, N=O+ 1, O=D, P= 1, 
          new12(A,B,K,Q), new23(A,B,C,N,E,F,G,H,I,J).
new24(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=B, new25(A,B,C,D,E,F,G,H,I,J).
new24(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=D, L=B, new26(A,B,C,D,E,F,G,H,I,J).
new23(A,B,C,D,E,F,G,H,I,J) :- new24(A,B,C,D,E,F,G,H,I,J).
new22(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=B, M=A, 
          new23(A,B,C,M,E,F,G,H,I,J).
new22(A,B,C,D,E,A,B,C,D,E) :- F>=G, F=C, G=B.
new21(A,B,C,D,E,F,G,H,I,J) :- new22(A,B,C,D,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J) :- K= 1, new21(A,B,K,D,E,F,G,H,I,J).
new18(A,B,C,D) :- new20(A,B,E,F,G,C,D,H,I,J).
safe :- init(A,B), new18(A,B,C,D).
new17(A,B).
new15(A,B,C,C) :- new17(A,B).
new14(A,B,C,C).
new12(A,B,C,D) :- E>= 1, E=C, F= 0, new14(A,B,C,D).
new12(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new14(A,B,C,D).
new11(A,B,C,D) :- E= 0, E=C, F= 0, new15(A,B,C,D).
new10(A,B,C,D,E,F,G,H,I,J) :- K=L+ 1, L=C, M= 1, new3(A,B,K,D,E,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, M=N+ 1, N=A, O= 1, 
          new10(M,B,C,D,E,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=E, L= 0, M=N+ 1, N=A, O= 1, 
          new10(M,B,C,D,E,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, new10(A,B,C,D,E,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- new9(A,B,C,D,K,F,G,H,I,J).
new7(A,B,C,D,E,A,B,C,D,E) :- F= 1,  1=<G, H= 1, G=D, new11(A,B,F,I).
new7(A,B,C,D,E,F,G,H,I,J) :- K= 1,  1=<L, M= 1, L=D, N=O+ 1, O=D, P= 1, 
          new12(A,B,K,Q), new5(A,B,C,N,E,F,G,H,I,J).
new7(A,B,C,D,E,A,B,C,D,E) :- F= 0,  1>=G+ 1, H= 1, G=D, new11(A,B,F,I).
new7(A,B,C,D,E,F,G,H,I,J) :- K= 0,  1>=L+ 1, M= 1, L=D, N=O+ 1, O=D, P= 1, 
          new12(A,B,K,Q), new5(A,B,C,N,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=B, new7(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=D, L=B, new8(A,B,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- new6(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=B, M=A, new5(A,B,C,M,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- new4(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K= 1, new3(A,B,K,D,E,F,G,H,I,J).
new1(A,B,C,D) :- new2(A,B,E,F,G,C,D,H,I,J).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
