new22(A,B,B) :- C>= 1, C=B, D= 0.
new22(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new21(A,B,C,D,E,F,G,H) :- I= 5, J=K, L=M+ 5, M=J, N= 5, 
          new7(A,I,O,P,Q,R,S,T,K,U), new19(L,B,J,D,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I=A, J=K, L=M- 1, M=J, N= 1, 
          new7(A,I,O,P,Q,R,S,T,K,U), new21(L,J,C,D,E,F,G,H).
new19(A,B,C,D,A,B,C,D) :- E= 1, F>= 1, F=A, G= 0, new22(A,E,H).
new19(A,B,C,D,A,B,C,D) :- E= 0, F=< 0, F=A, G= 0, new22(A,E,H).
new18(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, K=L+ 1, L=A, M= 1, 
          new19(K,B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=D, J= 0, K=L+ 1, L=A, M= 1, 
          new19(K,B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I= 0, I=D, J= 0, new20(A,B,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- new18(A,B,C,I,E,F,G,H).
new15(A,B) :- new17(A,C,D,E,B,F,G,H).
safe :- init(A), new15(A,B).
new14(A,B,B).
new13(A,B,C) :- D= 0, D=B, E= 0, new14(A,B,C).
new10(A,B,C,D,E,A,B,C,D,E).
new9(A,B,C,D,E,F,G,H,I,J) :- K=<L, K=C, L=B, M=N+O, N=D, O=C, P=Q*R, Q=E, R=C, 
          S=T+ 1, T=C, U= 1, new10(A,B,S,M,P,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :- K>=L+ 1, K=C, L=B, new10(A,B,C,D,E,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H) :- I= 5, J=K, L=M+ 5, M=J, N= 5, 
          new7(A,I,O,P,Q,R,S,T,K,U), new4(L,B,J,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H,I,J) :- K= 1, L= 0, M= 1, new9(A,B,K,L,M,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H) :- I=A, J=K, L=M- 1, M=J, N= 1, 
          new7(A,I,O,P,Q,R,S,T,K,U), new8(L,J,C,D,E,F,G,H).
new4(A,B,C,D,A,B,C,D) :- E= 1, F>= 1, F=A, G= 0, new13(A,E,H).
new4(A,B,C,D,A,B,C,D) :- E= 0, F=< 0, F=A, G= 0, new13(A,E,H).
new3(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, K=L+ 1, L=A, M= 1, 
          new4(K,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=D, J= 0, K=L+ 1, L=A, M= 1, 
          new4(K,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I= 0, I=D, J= 0, new5(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,B,C,I,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
