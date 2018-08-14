new21(A,B,C,D,E,F,G,H,I,J) :- K=L+ 1, L=C, M= 1, new16(A,B,K,D,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J) :- K= 1, L=< 0, L=D, M= 0, new10(A,B,K,N), 
          new21(A,B,C,D,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J) :- K= 0, L>= 1, L=D, M= 0, new10(A,B,K,N), 
          new21(A,B,C,D,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, new20(A,B,C,D,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :- K=< 0, K=E, L= 0, M= 1, 
          new21(A,B,C,M,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- new19(A,B,C,D,K,F,G,H,I,J).
new17(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=B, new18(A,B,C,D,E,F,G,H,I,J).
new17(A,B,C,D,E,A,B,C,D,E) :- F>=G, F=C, G=B.
new16(A,B,C,D,E,F,G,H,I,J) :- new17(A,B,C,D,E,F,G,H,I,J).
new15(A,B,C,D,E,F,G,H,I,J) :- K= 0, L= 0, new16(A,B,L,K,E,F,G,H,I,J).
new13(A,B,C,D) :- new15(A,B,E,F,G,C,D,H,I,J).
safe :- init(A,B), new13(A,B,C,D).
new12(A,B,C,C).
new10(A,B,C,C) :- D>= 1, D=C, E= 0.
new10(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new9(A,B,C,D) :- E= 0, E=C, F= 0, new12(A,B,C,D).
new8(A,B,C,D,E,F,G,H,I,J) :- K=L+ 1, L=C, M= 1, new3(A,B,K,D,E,F,G,H,I,J).
new7(A,B,C,D,E,A,B,C,D,E) :- F= 1, G=< 0, G=D, H= 0, new9(A,B,F,I).
new7(A,B,C,D,E,F,G,H,I,J) :- K= 1, L=< 0, L=D, M= 0, new10(A,B,K,N), 
          new8(A,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,A,B,C,D,E) :- F= 0, G>= 1, G=D, H= 0, new9(A,B,F,I).
new7(A,B,C,D,E,F,G,H,I,J) :- K= 0, L>= 1, L=D, M= 0, new10(A,B,K,N), 
          new8(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, new7(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K=< 0, K=E, L= 0, M= 1, new8(A,B,C,M,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- new6(A,B,C,D,K,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=B, new5(A,B,C,D,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- new4(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K= 0, L= 0, new3(A,B,L,K,E,F,G,H,I,J).
new1(A,B,C,D) :- new2(A,B,E,F,G,C,D,H,I,J).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
