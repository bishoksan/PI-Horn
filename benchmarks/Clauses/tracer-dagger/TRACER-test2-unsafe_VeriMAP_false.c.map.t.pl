new20(A,B,B) :- C>= 1, C=B, D= 0.
new20(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new19(A,B,C,D,E,F) :- G= 5, H=I, J=K+ 5, K=H, L= 5, new6(A,G,M,N,O,P,Q,R,I,S), 
          new17(J,B,H,D,E,F).
new18(A,B,C,D,E,F) :- G=A, H=I, J=K- 1, K=H, L= 1, new6(A,G,M,N,O,P,Q,R,I,S), 
          new19(J,H,C,D,E,F).
new17(A,B,C,A,B,C) :- D= 1, E=< 0, E=A, F= 0, new20(A,D,G).
new17(A,B,C,A,B,C) :- D= 0, E>= 1, E=A, F= 0, new20(A,D,G).
new16(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, I=J+ 1, J=A, K= 1, new17(I,B,C,D,E,F).
new16(A,B,C,D,E,F) :- G=< 0, G=A, H= 0, new18(A,B,C,D,E,F).
new14(A,B) :- new16(A,C,D,B,E,F).
safe :- init(A), new14(A,B).
new13(A,B,B).
new12(A,B,C) :- D= 0, D=B, E= 0, new13(A,B,C).
new9(A,B,C,D,E,A,B,C,D,E).
new8(A,B,C,D,E,F,G,H,I,J) :- K=<L, K=C, L=B, M=N+O, N=D, O=C, P=Q*R, Q=E, R=C, 
          S=T+ 1, T=C, U= 1, new9(A,B,S,M,P,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- K>=L+ 1, K=C, L=B, new9(A,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F) :- G= 5, H=I, J=K+ 5, K=H, L= 5, new6(A,G,M,N,O,P,Q,R,I,S), 
          new3(J,B,H,D,E,F).
new6(A,B,C,D,E,F,G,H,I,J) :- K= 1, L= 0, M= 1, new8(A,B,K,L,M,F,G,H,I,J).
new4(A,B,C,D,E,F) :- G=A, H=I, J=K- 1, K=H, L= 1, new6(A,G,M,N,O,P,Q,R,I,S), 
          new7(J,H,C,D,E,F).
new3(A,B,C,A,B,C) :- D= 1, E=< 0, E=A, F= 0, new12(A,D,G).
new3(A,B,C,A,B,C) :- D= 0, E>= 1, E=A, F= 0, new12(A,D,G).
new2(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, I=J+ 1, J=A, K= 1, new3(I,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G=< 0, G=A, H= 0, new4(A,B,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
