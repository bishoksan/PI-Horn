new14(A,B,C,C) :- D>= 1, D=C, E= 0.
new14(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new13(A,B,C,D,E,A,B,C,D,E) :- F= 1, G=< 0, G=B, H= 0, new14(A,B,F,I).
new13(A,B,C,D,E,A,B,C,D,E) :- F= 0, G>= 1, G=B, H= 0, new14(A,B,F,I).
new12(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=A, M=N+ 1, N=D, O= 1, 
          new12(A,B,C,M,E,F,G,H,I,J).
new12(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=D, L=A, new13(A,B,C,D,E,F,G,H,I,J).
new11(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=C, L= 0, M= 1, 
          new12(A,B,C,D,M,F,G,H,I,J).
new11(A,B,C,D,E,F,G,H,I,J) :- K=< 0, K=C, L= 0, M= 2, 
          new12(A,B,C,D,M,F,G,H,I,J).
new10(A,B,C,D,E,F,G,H,I,J) :- K= 0, new11(A,B,C,K,E,F,G,H,I,J).
new8(A,B,C,D) :- new10(A,B,E,F,G,C,D,H,I,J).
safe :- init(A,B), new8(A,B,C,D).
new7(A,B,C,C).
new6(A,B,C,D) :- E= 0, E=C, F= 0, new7(A,B,C,D).
new5(A,B,C,D,E,A,B,C,D,E) :- F= 1, G=< 0, G=B, H= 0, new6(A,B,F,I).
new5(A,B,C,D,E,A,B,C,D,E) :- F= 0, G>= 1, G=B, H= 0, new6(A,B,F,I).
new4(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=A, M=N+ 1, N=D, O= 1, 
          new4(A,B,C,M,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=D, L=A, new5(A,B,C,D,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=C, L= 0, M= 1, new4(A,B,C,D,M,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- K=< 0, K=C, L= 0, M= 2, new4(A,B,C,D,M,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K= 0, new3(A,B,C,K,E,F,G,H,I,J).
new1(A,B,C,D) :- new2(A,B,E,F,G,C,D,H,I,J).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
