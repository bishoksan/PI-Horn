new16(A,B,C,C) :- D>= 1, D=C, E= 0.
new16(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new15(A,B,C,D,E,F,G,H) :- I= 10, I=B, J= 10, K= 0, new14(A,B,C,K,E,F,G,H).
new15(A,B,C,D,E,F,G,H) :- I>= 11, I=B, J= 10, K= 1, new14(A,B,C,K,E,F,G,H).
new15(A,B,C,D,E,F,G,H) :- I+ 1=< 10, I=B, J= 10, K= 1, new14(A,B,C,K,E,F,G,H).
new14(A,B,C,D,A,B,C,D) :- E=D, new16(A,B,E,F).
new13(A,B,C,D,E,F,G,H) :- I= 5, I=A, J= 5, K= 0, new14(A,B,C,K,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I>= 6, I=A, J= 5, new15(A,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I+ 1=< 5, I=A, J= 5, new15(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K= 5, new13(K,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K= 5, new13(K,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, K= 10, new13(A,K,C,D,E,F,G,H).
new11(A,B,C,D,E,F,G,H) :- new12(A,B,I,D,E,F,G,H).
new9(A,B,C,D) :- new11(A,B,E,F,C,D,G,H).
safe :- init(A,B), new9(A,B,C,D).
new8(A,B,C,C).
new7(A,B,C,D) :- E= 0, E=C, F= 0, new8(A,B,C,D).
new6(A,B,C,D,E,F,G,H) :- I= 10, I=B, J= 10, K= 0, new5(A,B,C,K,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I>= 11, I=B, J= 10, K= 1, new5(A,B,C,K,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I+ 1=< 10, I=B, J= 10, K= 1, new5(A,B,C,K,E,F,G,H).
new5(A,B,C,D,A,B,C,D) :- E=D, new7(A,B,E,F).
new4(A,B,C,D,E,F,G,H) :- I= 5, I=A, J= 5, K= 0, new5(A,B,C,K,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I>= 6, I=A, J= 5, new6(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I+ 1=< 5, I=A, J= 5, new6(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K= 5, new4(K,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K= 5, new4(K,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, K= 10, new4(A,K,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,B,I,D,E,F,G,H).
new1(A,B,C,D) :- new2(A,B,E,F,C,D,G,H).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
