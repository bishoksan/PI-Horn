new26(A,B,C,A,B,C).
new24(A,B,C,D,E,F) :- G>= 6, G=B, H= 5, new26(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- G+ 1=< 5, G=B, H= 5, new26(A,B,C,D,E,F).
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
new11(A,B,C,D,E,F,G,H) :- I=J- 1, K=B, J= 18446744073709551616+K, K+ 1=< 0, 
          L= 1, M= 0, N= 1, O= 0, P= 1, new5(A,I,Q,R,S,T,U,V), 
          new12(S,B,N,M,E,F,G,H).
new11(A,B,C,D,E,F,G,H) :- I=J- 1, K=B, J= 18446744073709551616+K, K+ 1=< 0, 
          L= 1, M= 3, N= 4, O= 3, P= 1, new7(A,I,Q,R,S,T,U,V), 
          new12(S,B,N,M,E,F,G,H).
new11(A,B,C,D,E,F,G,H) :- I=J- 1, K=B, J= 18446744073709551616+K, K+ 1=< 0, 
          L= 1, M=N, O=P+ 1, P=M, Q= 1, new8(A,I,R,S,T,U,N,V), 
          new12(T,B,O,M,E,F,G,H).
new11(A,B,C,D,E,F,G,H) :- I=J- 1, K=B, J=K, K>= 0, L= 1, M= 0, N= 1, O= 0, 
          P= 1, new5(A,I,Q,R,S,T,U,V), new12(S,B,N,M,E,F,G,H).
new11(A,B,C,D,E,F,G,H) :- I=J- 1, K=B, J=K, K>= 0, L= 1, M= 3, N= 4, O= 3, 
          P= 1, new7(A,I,Q,R,S,T,U,V), new12(S,B,N,M,E,F,G,H).
new11(A,B,C,D,E,F,G,H) :- I=J- 1, K=B, J=K, K>= 0, L= 1, M=N, O=P+ 1, P=M, 
          Q= 1, new8(A,I,R,S,T,U,N,V), new12(T,B,O,M,E,F,G,H).
new10(A,B,C,D,A,B,C,D) :- E=< 3, E=C, F= 3.
new9(A,B,C,D,E,F,G,H) :- I=J- 1, K=B, J= 18446744073709551616+K, K+ 1=< 0, 
          L= 1, M= 0, N= 1, O= 0, P= 1, new5(A,I,Q,R,S,T,U,V), 
          new10(S,B,N,M,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I=J- 1, K=B, J= 18446744073709551616+K, K+ 1=< 0, 
          L= 1, M= 3, N= 4, O= 3, P= 1, new7(A,I,Q,R,S,T,U,V), 
          new10(S,B,N,M,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I=J- 1, K=B, J= 18446744073709551616+K, K+ 1=< 0, 
          L= 1, M=N, O=P+ 1, P=M, Q= 1, new8(A,I,R,S,T,U,N,V), 
          new10(T,B,O,M,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I=J- 1, K=B, J=K, K>= 0, L= 1, M= 0, N= 1, O= 0, P= 1, 
          new5(A,I,Q,R,S,T,U,V), new10(S,B,N,M,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I=J- 1, K=B, J=K, K>= 0, L= 1, M= 3, N= 4, O= 3, P= 1, 
          new7(A,I,Q,R,S,T,U,V), new10(S,B,N,M,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I=J- 1, K=B, J=K, K>= 0, L= 1, M=N, O=P+ 1, P=M, Q= 1, 
          new8(A,I,R,S,T,U,N,V), new10(T,B,O,M,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, new9(A,B,C,D,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, J= 0, new9(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, new11(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, J= 0, new11(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F) :- G= 5, G=B, H= 5, new13(A,B,C,D,E,F).
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
