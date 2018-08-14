new20(A,B,C,D,A,B,C,D).
new18(A,B,C,D,E,F,G,H) :- I=J, I=C, C>= 0, K=L+M, L=A, M=B, 
          J= 18446744073709551616+K, K+ 1=< 0, new20(A,B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I=J, I=C, C>= 0, K=L+M, L=A, M=B, J=K, K>= 0, 
          new20(A,B,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I=A, J= 18446744073709551616+I, I+ 1=< 0, K=B, 
          L= 18446744073709551616+K, K+ 1=< 0, M=N, N>= 0, O=M, M>= 0, 
          new5(A,B,J,L,P,Q,R,S,N,T), new18(Q,R,O,M,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I=A, J= 18446744073709551616+I, I+ 1=< 0, K=B, 
          L= 18446744073709551616+K, K+ 1=< 0, M=N, N>= 0, O=M, M>= 0, 
          new7(A,B,J,L,P,Q,R,S,T,N), new18(Q,R,O,M,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I=A, J= 18446744073709551616+I, I+ 1=< 0, K=B, L=K, 
          K>= 0, M=N, N>= 0, O=M, M>= 0, new5(A,B,J,L,P,Q,R,S,N,T), 
          new18(Q,R,O,M,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I=A, J= 18446744073709551616+I, I+ 1=< 0, K=B, L=K, 
          K>= 0, M=N, N>= 0, O=M, M>= 0, new7(A,B,J,L,P,Q,R,S,T,N), 
          new18(Q,R,O,M,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I=A, J=I, I>= 0, K=B, L= 18446744073709551616+K, 
          K+ 1=< 0, M=N, N>= 0, O=M, M>= 0, new5(A,B,J,L,P,Q,R,S,N,T), 
          new18(Q,R,O,M,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I=A, J=I, I>= 0, K=B, L= 18446744073709551616+K, 
          K+ 1=< 0, M=N, N>= 0, O=M, M>= 0, new7(A,B,J,L,P,Q,R,S,T,N), 
          new18(Q,R,O,M,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I=A, J=I, I>= 0, K=B, L=K, K>= 0, M=N, N>= 0, O=M, 
          M>= 0, new5(A,B,J,L,P,Q,R,S,N,T), new18(Q,R,O,M,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I=A, J=I, I>= 0, K=B, L=K, K>= 0, M=N, N>= 0, O=M, 
          M>= 0, new7(A,B,J,L,P,Q,R,S,T,N), new18(Q,R,O,M,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- new17(A,B,C,D,E,F,G,H).
new14(A,B,C,D) :- new16(A,B,E,F,C,D,G,H).
safe :- init(A,B), new14(A,B,C,D).
new11(A,B).
new9(A,B,C,D,A,B,C,D) :- new11(A,B).
new8(A,B,C,D,E,F,G,C,D,H) :- I=J- 1, J=C, C>= 0, K= 1, L=M+ 1, M=D, D>= 0, 
          N= 1, H=O, O>= 0, new5(A,B,I,L,P,F,G,Q,O,R).
new8(A,B,C,D,E,F,G,C,D,H) :- I=J- 1, J=C, C>= 0, K= 1, L=M+ 1, M=D, D>= 0, 
          N= 1, H=O, O>= 0, new7(A,B,I,L,P,F,G,Q,R,O).
new7(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=C, C>= 0, L= 0, new8(A,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=C, C>= 0, L= 0, 
          new8(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=C, C>= 0, K=L+M, L=A, M=B, 
          J= 18446744073709551616+K, K+ 1=< 0, new9(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, C>= 0, K=L+M, L=A, M=B, 
          J= 18446744073709551616+K, K+ 1=< 0, new9(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=C, C>= 0, K=L+M, L=A, M=B, J=K, K>= 0, 
          new9(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, C>= 0, K=L+M, L=A, M=B, J=K, K>= 0, 
          new9(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,A,B,C,D,E) :- F= 0, F=C, C>= 0, G= 0.
new3(A,B,C,D,E,F,G,H) :- I=A, J= 18446744073709551616+I, I+ 1=< 0, K=B, 
          L= 18446744073709551616+K, K+ 1=< 0, M=N, N>= 0, O=M, M>= 0, 
          new5(A,B,J,L,P,Q,R,S,N,T), new6(Q,R,O,M,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I=A, J= 18446744073709551616+I, I+ 1=< 0, K=B, 
          L= 18446744073709551616+K, K+ 1=< 0, M=N, N>= 0, O=M, M>= 0, 
          new7(A,B,J,L,P,Q,R,S,T,N), new6(Q,R,O,M,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I=A, J= 18446744073709551616+I, I+ 1=< 0, K=B, L=K, 
          K>= 0, M=N, N>= 0, O=M, M>= 0, new5(A,B,J,L,P,Q,R,S,N,T), 
          new6(Q,R,O,M,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I=A, J= 18446744073709551616+I, I+ 1=< 0, K=B, L=K, 
          K>= 0, M=N, N>= 0, O=M, M>= 0, new7(A,B,J,L,P,Q,R,S,T,N), 
          new6(Q,R,O,M,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I=A, J=I, I>= 0, K=B, L= 18446744073709551616+K, 
          K+ 1=< 0, M=N, N>= 0, O=M, M>= 0, new5(A,B,J,L,P,Q,R,S,N,T), 
          new6(Q,R,O,M,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I=A, J=I, I>= 0, K=B, L= 18446744073709551616+K, 
          K+ 1=< 0, M=N, N>= 0, O=M, M>= 0, new7(A,B,J,L,P,Q,R,S,T,N), 
          new6(Q,R,O,M,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I=A, J=I, I>= 0, K=B, L=K, K>= 0, M=N, N>= 0, O=M, 
          M>= 0, new5(A,B,J,L,P,Q,R,S,N,T), new6(Q,R,O,M,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I=A, J=I, I>= 0, K=B, L=K, K>= 0, M=N, N>= 0, O=M, 
          M>= 0, new7(A,B,J,L,P,Q,R,S,T,N), new6(Q,R,O,M,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,B,C,D,E,F,G,H).
new1(A,B,C,D) :- new2(A,B,E,F,C,D,G,H).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
