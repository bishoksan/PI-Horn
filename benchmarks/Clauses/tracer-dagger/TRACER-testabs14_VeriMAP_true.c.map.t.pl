new22(A,B,B) :- C>= 1, C=B, D= 0.
new22(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new21(A,B,C,D,E,F,G,H) :- I>= 11, I=C, J= 10, K= 0, new20(A,B,C,K,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I=< 10, I=C, J= 10, K= 1, new20(A,B,C,K,E,F,G,H).
new20(A,B,C,D,A,B,C,D) :- E=D, new22(A,E,F).
new19(A,B,C,D,E,F,G,H) :- I>= 11, I=B, J= 10, K= 0, new20(A,B,C,K,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- I=< 10, I=B, J= 10, new21(A,B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=B, J=A, K=L+ 1, L=B, M= 1, 
          new17(A,K,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I>=J, I=B, J=A, new19(A,B,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- new18(A,B,C,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=A, K=L+ 1, L=C, M= 1, 
          new15(A,B,K,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I>=J, I=C, J=A, new17(A,B,C,D,E,F,G,H).
new15(A,B,C,D,E,F,G,H) :- new16(A,B,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I= 0, J= 0, new15(A,I,J,D,E,F,G,H).
new12(A,B) :- new14(A,C,D,E,B,F,G,H).
safe :- init(A), new12(A,B).
new11(A,B,B).
new10(A,B,C) :- D= 0, D=B, E= 0, new11(A,B,C).
new9(A,B,C,D,E,F,G,H) :- I>= 11, I=C, J= 10, K= 0, new8(A,B,C,K,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I=< 10, I=C, J= 10, K= 1, new8(A,B,C,K,E,F,G,H).
new8(A,B,C,D,A,B,C,D) :- E=D, new10(A,E,F).
new7(A,B,C,D,E,F,G,H) :- I>= 11, I=B, J= 10, K= 0, new8(A,B,C,K,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I=< 10, I=B, J= 10, new9(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=B, J=A, K=L+ 1, L=B, M= 1, 
          new5(A,K,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I>=J, I=B, J=A, new7(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- new6(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=A, K=L+ 1, L=C, M= 1, 
          new3(A,B,K,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I>=J, I=C, J=A, new5(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I= 0, J= 0, new3(A,I,J,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
