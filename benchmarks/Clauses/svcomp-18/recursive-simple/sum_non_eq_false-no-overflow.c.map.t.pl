new20(A,B,C,D,A,B,C,D).
new18(A,B,C,D,E,F,G,H) :- I=J, I=C, J=K+L, K=A, L=B, new20(A,B,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I=A, J=B, K=L, M=K, new5(A,B,I,J,N,O,P,Q,L,R), 
          new18(O,P,M,K,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I=A, J=B, K=L, M=K, new7(A,B,I,J,N,O,P,Q,R,L), 
          new18(O,P,M,K,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- new17(A,B,C,D,E,F,G,H).
new14(A,B,C,D) :- new16(A,B,E,F,C,D,G,H).
safe :- init(A,B), new14(A,B,C,D).
new11(A,B).
new9(A,B,C,D,A,B,C,D) :- new11(A,B).
new8(A,B,C,D,E,F,G,C,D,H) :- I=J- 1, J=C, K= 1, L=M+ 1, M=D, N= 1, H=O, 
          new5(A,B,I,L,P,F,G,Q,O,R).
new8(A,B,C,D,E,F,G,C,D,H) :- I=J- 1, J=C, K= 1, L=M+ 1, M=D, N= 1, H=O, 
          new7(A,B,I,L,P,F,G,Q,R,O).
new7(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=C, L= 0, new8(A,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=C, L= 0, new8(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=C, J=K+L, K=A, L=B, new9(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=K+L, K=A, L=B, new9(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,A,B,C,D,E) :- F= 0, F=C, G= 0.
new3(A,B,C,D,E,F,G,H) :- I=A, J=B, K=L, M=K, new5(A,B,I,J,N,O,P,Q,L,R), 
          new6(O,P,M,K,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I=A, J=B, K=L, M=K, new7(A,B,I,J,N,O,P,Q,R,L), 
          new6(O,P,M,K,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,B,C,D,E,F,G,H).
new1(A,B,C,D) :- new2(A,B,E,F,C,D,G,H).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
