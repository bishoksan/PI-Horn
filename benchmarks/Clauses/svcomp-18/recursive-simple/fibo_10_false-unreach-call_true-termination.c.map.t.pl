new30(A,B,C,A,B,C).
new28(A,B,C,D,E,F) :- G>= 56, G=B, H= 55, new30(A,B,C,D,E,F).
new28(A,B,C,D,E,F) :- G+ 1=< 55, G=B, H= 55, new30(A,B,C,D,E,F).
new27(A,B,C,D,E,F) :- G=A, H= 0, I= 0, new5(A,G,J,K,L,M,N,O), 
          new28(L,I,H,D,E,F).
new27(A,B,C,D,E,F) :- G=A, H= 1, I= 1, new7(A,G,J,K,L,M,N,O), 
          new28(L,I,H,D,E,F).
new27(A,B,C,D,E,F) :- G=A, H=I+J, I=K, J=L, M=H, new8(A,G,N,O,P,Q,K,L), 
          new28(P,M,H,D,E,F).
new26(A,B,C,D,E,F) :- new27(A,B,C,D,E,F).
new24(A,B) :- new26(A,C,D,B,E,F).
safe :- init(A), new24(A,B).
new17(A).
new15(A,B,C,A,B,C) :- new17(A).
new12(A,B,C,D,A,B,C,D) :- E= 1, E=B, F= 1.
new11(A,B,C,D,E,B,C,F) :- G=H- 2, H=B, I= 2, F= 0, new5(A,G,J,K,E,L,M,N).
new11(A,B,C,D,E,B,C,F) :- G=H- 2, H=B, I= 2, F= 1, new7(A,G,J,K,E,L,M,N).
new11(A,B,C,D,E,B,C,F) :- G=H- 2, H=B, I= 2, F=J+K, J=L, K=M, 
          new8(A,G,N,O,E,P,L,M).
new10(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L= 0, new5(A,I,M,N,O,P,Q,R), 
          new11(O,B,L,D,E,F,G,H).
new10(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L= 1, new7(A,I,M,N,O,P,Q,R), 
          new11(O,B,L,D,E,F,G,H).
new10(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L=M+N, M=O, N=P, 
          new8(A,I,Q,R,S,T,O,P), new11(S,B,L,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I>= 2, I=B, J= 1, new10(A,B,C,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I+ 1=< 1, I=B, J= 1, new10(A,B,C,D,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 1, new9(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 1, new12(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F) :- G= 55, G=B, H= 55, new15(A,B,C,D,E,F).
new5(A,B,C,D,A,B,C,D) :- E+ 1=< 1, E=B, F= 1.
new3(A,B,C,D,E,F) :- G=A, H= 0, I= 0, new5(A,G,J,K,L,M,N,O), new6(L,I,H,D,E,F).
new3(A,B,C,D,E,F) :- G=A, H= 1, I= 1, new7(A,G,J,K,L,M,N,O), new6(L,I,H,D,E,F).
new3(A,B,C,D,E,F) :- G=A, H=I+J, I=K, J=L, M=H, new8(A,G,N,O,P,Q,K,L), 
          new6(P,M,H,D,E,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
