new20(A,B,B) :- C>= 1, C=B, D= 0.
new20(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new19(A,B,C,D,A,B,C,D) :- E= 1, F=< 7, F=A, G= 7, new20(A,E,H).
new19(A,B,C,D,A,B,C,D) :- E= 0, F>= 8, F=A, G= 7, new20(A,E,H).
new18(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, K=L+ 4, L=A, M= 4, 
          new19(K,B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=D, J= 0, K=L+ 4, L=A, M= 4, 
          new19(K,B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I= 0, I=D, J= 0, new19(A,B,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- new18(A,B,C,I,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K=L+ 2, L=A, M= 2, 
          new17(K,B,C,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K=L+ 2, L=A, M= 2, 
          new17(K,B,C,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, new17(A,B,C,D,E,F,G,H).
new15(A,B,C,D,E,F,G,H) :- new16(A,B,I,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, K=L+ 1, L=A, M= 1, 
          new15(K,B,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, J= 0, K=L+ 1, L=A, M= 1, 
          new15(K,B,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I= 0, I=B, J= 0, new15(A,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- new14(A,I,C,D,E,F,G,H).
new11(A,B) :- new13(A,C,D,E,B,F,G,H).
safe :- init(A), new11(A,B).
new10(A,B,B).
new9(A,B,C) :- D= 0, D=B, E= 0, new10(A,B,C).
new8(A,B,C,D,A,B,C,D) :- E= 1, F=< 7, F=A, G= 7, new9(A,E,H).
new8(A,B,C,D,A,B,C,D) :- E= 0, F>= 8, F=A, G= 7, new9(A,E,H).
new7(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, K=L+ 4, L=A, M= 4, 
          new8(K,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=D, J= 0, K=L+ 4, L=A, M= 4, 
          new8(K,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I= 0, I=D, J= 0, new8(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- new7(A,B,C,I,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K=L+ 2, L=A, M= 2, 
          new6(K,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K=L+ 2, L=A, M= 2, 
          new6(K,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, new6(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- new5(A,B,I,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, K=L+ 1, L=A, M= 1, 
          new4(K,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, J= 0, K=L+ 1, L=A, M= 1, 
          new4(K,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I= 0, I=B, J= 0, new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,I,C,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
