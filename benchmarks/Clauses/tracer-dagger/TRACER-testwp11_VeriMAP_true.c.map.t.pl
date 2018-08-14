new18(A,B,C,D,D) :- E>= 1, E=D, F= 0.
new18(A,B,C,D,D) :- E+ 1=< 0, E=D, F= 0.
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 3, O=E, P= 2, Q= 0, 
          new16(A,B,C,D,E,F,Q,H,I,J,K,L,M,N).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=< 2, O=E, P= 2, Q= 1, 
          new16(A,B,C,D,E,F,Q,H,I,J,K,L,M,N).
new16(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H=G, new18(A,B,C,H,I).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 5, O=D, P= 4, Q= 0, 
          new16(A,B,C,D,E,F,Q,H,I,J,K,L,M,N).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=< 4, O=D, P= 4, 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 0, Q= 5, 
          new15(A,B,C,D,E,Q,G,H,I,J,K,L,M,N).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=< 0, O=A, P= 0, Q= 6, 
          new15(A,B,C,D,E,Q,G,H,I,J,K,L,M,N).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=C, P= 0, Q= 4, 
          new14(A,B,C,Q,E,F,G,H,I,J,K,L,M,N).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=< 0, O=C, P= 0, Q= 5, 
          new14(A,B,C,Q,E,F,G,H,I,J,K,L,M,N).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=B, P= 0, Q= 2, 
          new13(A,B,C,D,Q,F,G,H,I,J,K,L,M,N).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=< 0, O=B, P= 0, Q= 3, 
          new13(A,B,C,D,Q,F,G,H,I,J,K,L,M,N).
new10(A,B,C,D,E,F) :- new12(A,B,C,G,H,I,J,D,E,F,K,L,M,N).
safe :- init(A,B,C), new10(A,B,C,D,E,F).
new9(A,B,C,D,D).
new8(A,B,C,D,E) :- F= 0, F=D, G= 0, new9(A,B,C,D,E).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 3, O=E, P= 2, Q= 0, 
          new6(A,B,C,D,E,F,Q,H,I,J,K,L,M,N).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=< 2, O=E, P= 2, Q= 1, 
          new6(A,B,C,D,E,F,Q,H,I,J,K,L,M,N).
new6(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H=G, new8(A,B,C,H,I).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 5, O=D, P= 4, Q= 0, 
          new6(A,B,C,D,E,F,Q,H,I,J,K,L,M,N).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=< 4, O=D, P= 4, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 0, Q= 5, 
          new5(A,B,C,D,E,Q,G,H,I,J,K,L,M,N).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=< 0, O=A, P= 0, Q= 6, 
          new5(A,B,C,D,E,Q,G,H,I,J,K,L,M,N).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=C, P= 0, Q= 4, 
          new4(A,B,C,Q,E,F,G,H,I,J,K,L,M,N).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=< 0, O=C, P= 0, Q= 5, 
          new4(A,B,C,Q,E,F,G,H,I,J,K,L,M,N).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=B, P= 0, Q= 2, 
          new3(A,B,C,D,Q,F,G,H,I,J,K,L,M,N).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=< 0, O=B, P= 0, Q= 3, 
          new3(A,B,C,D,Q,F,G,H,I,J,K,L,M,N).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,H,I,J,D,E,F,K,L,M,N).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
