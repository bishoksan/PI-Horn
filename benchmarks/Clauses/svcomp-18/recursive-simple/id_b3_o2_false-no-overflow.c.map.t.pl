new26(A,B,C,A,B,C).
new24(A,B,C,D,E,F) :- G>= 3, G=B, H= 2, new26(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- G+ 1=< 2, G=B, H= 2, new26(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- G=A, H= 0, I= 0, new5(A,G,J,K,L,M,N,O), 
          new24(L,I,H,D,E,F).
new23(A,B,C,D,E,F) :- G=A, H= 3, I= 3, new7(A,G,J,K,L,M,N,O), 
          new24(L,I,H,D,E,F).
new23(A,B,C,D,E,F) :- G=A, H=I, J=H, new8(A,G,K,L,M,N,I,O), new24(M,J,H,D,E,F).
new22(A,B,C,D,E,F) :- new23(A,B,C,D,E,F).
new20(A,B) :- new22(A,C,D,B,E,F).
safe :- init(A), new20(A,B).
new15(A).
new13(A,B,C,A,B,C) :- new15(A).
new12(A,B,C,D,A,B,C,D) :- E>= 4, E=C, F= 3.
new11(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L= 0, M= 1, N= 0, O= 1, 
          new5(A,I,P,Q,R,S,T,U), new12(R,B,M,L,E,F,G,H).
new11(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L= 3, M= 4, N= 3, O= 1, 
          new7(A,I,P,Q,R,S,T,U), new12(R,B,M,L,E,F,G,H).
new11(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L=M, N=O+ 1, O=L, P= 1, 
          new8(A,I,Q,R,S,T,M,U), new12(S,B,N,L,E,F,G,H).
new10(A,B,C,D,A,B,C,D) :- E=< 3, E=C, F= 3.
new9(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L= 0, M= 1, N= 0, O= 1, 
          new5(A,I,P,Q,R,S,T,U), new10(R,B,M,L,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L= 3, M= 4, N= 3, O= 1, 
          new7(A,I,P,Q,R,S,T,U), new10(R,B,M,L,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L=M, N=O+ 1, O=L, P= 1, 
          new8(A,I,Q,R,S,T,M,U), new10(S,B,N,L,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, new9(A,B,C,D,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, J= 0, new9(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, new11(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, J= 0, new11(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F) :- G= 2, G=B, H= 2, new13(A,B,C,D,E,F).
new5(A,B,C,D,A,B,C,D) :- E= 0, E=B, F= 0.
new3(A,B,C,D,E,F) :- G=A, H= 0, I= 0, new5(A,G,J,K,L,M,N,O), new6(L,I,H,D,E,F).
new3(A,B,C,D,E,F) :- G=A, H= 3, I= 3, new7(A,G,J,K,L,M,N,O), new6(L,I,H,D,E,F).
new3(A,B,C,D,E,F) :- G=A, H=I, J=H, new8(A,G,K,L,M,N,I,O), new6(M,J,H,D,E,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
