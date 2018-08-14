new18(A,B,C,C) :- D>= 1, D=C, E= 0.
new18(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new17(A,B,C,D,E,A,B,C,D,E) :- F= 1, G= 0, G=D, H= 0, new18(A,B,F,I).
new17(A,B,C,D,E,A,B,C,D,E) :- F= 0, G>= 1, G=D, H= 0, new18(A,B,F,I).
new17(A,B,C,D,E,A,B,C,D,E) :- F= 0, G+ 1=< 0, G=D, H= 0, new18(A,B,F,I).
new16(A,B,C,D,E,F,G,H,I,J) :- K=A, L=M, N=O-P, O=Q-R, Q=L, R=S, P=T, 
          new6(A,B,K,U,S,T,V,M), new17(S,T,L,N,E,F,G,H,I,J).
new15(A,B,C,D,E,F,G,H,I,J) :- K=A, L=M, N=O-P, O=Q-R, Q=L, R=S, P=T, 
          new11(A,B,K,U,S,T,V,M), new17(S,T,L,N,E,F,G,H,I,J).
new14(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, new15(A,B,C,D,E,F,G,H,I,J).
new14(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=E, L= 0, new15(A,B,C,D,E,F,G,H,I,J).
new14(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, new16(A,B,C,D,E,F,G,H,I,J).
new12(A,B,C,D) :- new14(A,B,E,F,G,C,D,H,I,J).
safe :- init(A,B), new12(A,B,C,D).
new11(A,B,C,D,A,B,C,E) :- E=F+ 3, F=C, G= 3.
new9(A,B,C,C).
new8(A,B,C,D) :- E= 0, E=C, F= 0, new9(A,B,C,D).
new7(A,B,C,D,E,A,B,C,D,E) :- F= 1, G= 0, G=D, H= 0, new8(A,B,F,I).
new7(A,B,C,D,E,A,B,C,D,E) :- F= 0, G>= 1, G=D, H= 0, new8(A,B,F,I).
new7(A,B,C,D,E,A,B,C,D,E) :- F= 0, G+ 1=< 0, G=D, H= 0, new8(A,B,F,I).
new6(A,B,C,D,A,B,C,E) :- E=F+ 5, F=C, G= 5.
new4(A,B,C,D,E,F,G,H,I,J) :- K=A, L=M, N=O-P, O=Q-R, Q=L, R=S, P=T, 
          new6(A,B,K,U,S,T,V,M), new7(S,T,L,N,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- K=A, L=M, N=O-P, O=Q-R, Q=L, R=S, P=T, 
          new11(A,B,K,U,S,T,V,M), new7(S,T,L,N,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, new3(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=E, L= 0, new3(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, new4(A,B,C,D,E,F,G,H,I,J).
new1(A,B,C,D) :- new2(A,B,E,F,G,C,D,H,I,J).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
