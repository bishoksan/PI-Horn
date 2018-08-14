new16(A,B,B) :- C>= 1, C=B, D= 0.
new16(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new15(A,B,C,D,A,B,C,D) :- E= 1, F=< 1000, F=A, G= 1000, new16(A,E,H).
new15(A,B,C,D,A,B,C,D) :- E= 0, F>= 1001, F=A, G= 1000, new16(A,E,H).
new14(A,B,C,D,E,F,G,H) :- I=J+ 1, J=A, K= 1, new15(I,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I= 3, I=C, J= 3, K=L+ 1, L=A, M= 1, 
          new14(K,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I>= 4, I=C, J= 3, new14(A,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I+ 1=< 3, I=C, J= 3, new14(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I= 0, I=B, J= 0, new13(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, new14(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, J= 0, new14(A,B,C,D,E,F,G,H).
new11(A,B,C,D,E,F,G,H) :- I=D, J=D, new12(A,I,J,D,E,F,G,H).
new9(A,B) :- new11(A,C,D,E,B,F,G,H).
safe :- init(A), new9(A,B).
new8(A,B,B).
new7(A,B,C) :- D= 0, D=B, E= 0, new8(A,B,C).
new6(A,B,C,D,A,B,C,D) :- E= 1, F=< 1000, F=A, G= 1000, new7(A,E,H).
new6(A,B,C,D,A,B,C,D) :- E= 0, F>= 1001, F=A, G= 1000, new7(A,E,H).
new5(A,B,C,D,E,F,G,H) :- I=J+ 1, J=A, K= 1, new6(I,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I= 3, I=C, J= 3, K=L+ 1, L=A, M= 1, 
          new5(K,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I>= 4, I=C, J= 3, new5(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I+ 1=< 3, I=C, J= 3, new5(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I= 0, I=B, J= 0, new4(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, new5(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, J= 0, new5(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I=D, J=D, new3(A,I,J,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
