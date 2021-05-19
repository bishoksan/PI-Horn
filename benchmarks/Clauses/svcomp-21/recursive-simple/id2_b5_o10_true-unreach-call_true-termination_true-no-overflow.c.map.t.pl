new38(A,B,C,A,B,C).
new36(A,B,C,D,E,F) :- G>= 11, G=B, B>= 0, H= 10, new38(A,B,C,D,E,F).
new36(A,B,C,D,E,F) :- G+ 1=< 10, G=B, B>= 0, H= 10, new38(A,B,C,D,E,F).
new35(A,B,C,D,E,F) :- G=A, A>= 0, H= 0, I= 0,  0>= 0, new5(A,G,J,K,L,M,N,O), 
          new36(L,I,H,D,E,F).
new35(A,B,C,D,E,F) :- G=A, A>= 0, H= 5, I= 5,  5>= 0, new7(A,G,J,K,L,M,N,O), 
          new36(L,I,H,D,E,F).
new35(A,B,C,D,E,F) :- G=A, A>= 0, H=I, I>= 0, J=H, H>= 0, 
          new8(A,G,K,L,M,N,I,O), new36(M,J,H,D,E,F).
new34(A,B,C,D,E,F) :- new35(A,B,C,D,E,F).
new32(A,B) :- new34(A,C,D,B,E,F).
safe :- init(A), new32(A,B).
new24(A).
new22(A,B,C,A,B,C) :- new24(A).
new21(A,B,C,D,A,B,C,D) :- E>= 6, E=C, C>= 0, F= 5.
new20(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, B>= 0, K= 1, L= 0, M= 1, N= 0,  0>= 0, 
          O= 1, new10(A,I,P,Q,R,S,T,U), new21(R,B,M,L,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, B>= 0, K= 1, L= 5, M= 6, N= 5,  5>= 0, 
          O= 1, new12(A,I,P,Q,R,S,T,U), new21(R,B,M,L,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, B>= 0, K= 1, L=M, M>= 0, N=O+ 1, O=L, 
          L>= 0, P= 1, new13(A,I,Q,R,S,T,M,U), new21(S,B,N,L,E,F,G,H).
new17(A,B,C,D,A,B,C,D) :- E>= 6, E=C, C>= 0, F= 5.
new16(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, B>= 0, K= 1, L= 0, M= 1, N= 0,  0>= 0, 
          O= 1, new5(A,I,P,Q,R,S,T,U), new17(R,B,M,L,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, B>= 0, K= 1, L= 5, M= 6, N= 5,  5>= 0, 
          O= 1, new7(A,I,P,Q,R,S,T,U), new17(R,B,M,L,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, B>= 0, K= 1, L=M, M>= 0, N=O+ 1, O=L, 
          L>= 0, P= 1, new8(A,I,Q,R,S,T,M,U), new17(S,B,N,L,E,F,G,H).
new15(A,B,C,D,A,B,C,D) :- E=< 5, E=C, C>= 0, F= 5.
new14(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, B>= 0, K= 1, L= 0, M= 1, N= 0,  0>= 0, 
          O= 1, new5(A,I,P,Q,R,S,T,U), new15(R,B,M,L,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, B>= 0, K= 1, L= 5, M= 6, N= 5,  5>= 0, 
          O= 1, new7(A,I,P,Q,R,S,T,U), new15(R,B,M,L,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, B>= 0, K= 1, L=M, M>= 0, N=O+ 1, O=L, 
          L>= 0, P= 1, new8(A,I,Q,R,S,T,M,U), new15(S,B,N,L,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I>= 1, I=B, B>= 0, J= 0, new14(A,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, B>= 0, J= 0, new14(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I>= 1, I=B, B>= 0, J= 0, new16(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, B>= 0, J= 0, new16(A,B,C,D,E,F,G,H).
new11(A,B,C,D,A,B,C,D) :- E=< 5, E=C, C>= 0, F= 5.
new10(A,B,C,D,A,B,C,D) :- E= 0, E=B, B>= 0, F= 0.
new9(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, B>= 0, K= 1, L= 0, M= 1, N= 0,  0>= 0, 
          O= 1, new10(A,I,P,Q,R,S,T,U), new11(R,B,M,L,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, B>= 0, K= 1, L= 5, M= 6, N= 5,  5>= 0, 
          O= 1, new12(A,I,P,Q,R,S,T,U), new11(R,B,M,L,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, B>= 0, K= 1, L=M, M>= 0, N=O+ 1, O=L, 
          L>= 0, P= 1, new13(A,I,Q,R,S,T,M,U), new11(S,B,N,L,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I>= 1, I=B, B>= 0, J= 0, new9(A,B,C,D,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, B>= 0, J= 0, new9(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I>= 1, I=B, B>= 0, J= 0, new20(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, B>= 0, J= 0, new20(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F) :- G= 10, G=B, B>= 0, H= 10, new22(A,B,C,D,E,F).
new5(A,B,C,D,A,B,C,D) :- E= 0, E=B, B>= 0, F= 0.
new3(A,B,C,D,E,F) :- G=A, A>= 0, H= 0, I= 0,  0>= 0, new5(A,G,J,K,L,M,N,O), 
          new6(L,I,H,D,E,F).
new3(A,B,C,D,E,F) :- G=A, A>= 0, H= 5, I= 5,  5>= 0, new7(A,G,J,K,L,M,N,O), 
          new6(L,I,H,D,E,F).
new3(A,B,C,D,E,F) :- G=A, A>= 0, H=I, I>= 0, J=H, H>= 0, new8(A,G,K,L,M,N,I,O), 
          new6(M,J,H,D,E,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
