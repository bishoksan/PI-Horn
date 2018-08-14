new21(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, O= 0, 
          new18(A,B,C,D,E,O,G,H,I,J,K,L).
new21(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 0, M=C, N= 0, O= 1, 
          new18(A,B,C,D,E,O,G,H,I,J,K,L).
new19(A,B,C,D,D) :- E>= 1, E=D, F= 0.
new19(A,B,C,D,D) :- E+ 1=< 0, E=D, F= 0.
new18(A,B,C,D,E,F,A,B,C,D,E,F) :- G=F, new19(A,B,C,G,H).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 3, M=B, N= 3, O= 0, 
          new18(A,B,C,D,E,O,G,H,I,J,K,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 3, M=B, N= 3, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 7, M=A, N= 6, 
          new17(A,B,C,D,E,F,G,H,I,J,K,L).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 6, M=A, N= 6, O= 1, 
          new18(A,B,C,D,E,O,G,H,I,J,K,L).
new15(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, O=P+ 4, P=A, Q= 4, 
          new16(O,B,C,D,E,F,G,H,I,J,K,L).
new15(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=C, N= 0, O= 3, 
          new16(A,B,O,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, O= 3, 
          new15(A,B,C,D,O,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=D, N= 0, O= 2, 
          new15(A,B,C,D,O,F,G,H,I,J,K,L).
new13(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=B, N= 0, O=P+ 2, P=A, Q= 2, 
          new14(O,B,C,D,E,F,G,H,I,J,K,L).
new13(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=B, N= 0, O= 3, 
          new14(A,O,C,D,E,F,G,H,I,J,K,L).
new11(A,B,C,D,E,F) :- new13(A,B,C,G,H,I,D,E,F,J,K,L).
safe :- init(A,B,C), new11(A,B,C,D,E,F).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, O= 0, 
          new7(A,B,C,D,E,O,G,H,I,J,K,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 0, M=C, N= 0, O= 1, 
          new7(A,B,C,D,E,O,G,H,I,J,K,L).
new9(A,B,C,D,D).
new8(A,B,C,D,E) :- F= 0, F=D, G= 0, new9(A,B,C,D,E).
new7(A,B,C,D,E,F,A,B,C,D,E,F) :- G=F, new8(A,B,C,G,H).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 3, M=B, N= 3, O= 0, 
          new7(A,B,C,D,E,O,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 3, M=B, N= 3, 
          new10(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 7, M=A, N= 6, 
          new6(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 6, M=A, N= 6, O= 1, 
          new7(A,B,C,D,E,O,G,H,I,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, O=P+ 4, P=A, Q= 4, 
          new5(O,B,C,D,E,F,G,H,I,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=C, N= 0, O= 3, 
          new5(A,B,O,D,E,F,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, O= 3, 
          new4(A,B,C,D,O,F,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=D, N= 0, O= 2, 
          new4(A,B,C,D,O,F,G,H,I,J,K,L).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=B, N= 0, O=P+ 2, P=A, Q= 2, 
          new3(O,B,C,D,E,F,G,H,I,J,K,L).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=B, N= 0, O= 3, 
          new3(A,O,C,D,E,F,G,H,I,J,K,L).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,H,I,D,E,F,J,K,L).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
