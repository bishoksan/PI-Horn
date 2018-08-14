new12(A,B,C,C) :- D>= 1, D=C, E= 0.
new12(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new11(A,B,C,D,A,B,C,D) :- E= 1, F=< 1, F=D, G= 1, new12(A,B,E,H).
new11(A,B,C,D,A,B,C,D) :- E= 0, F>= 2, F=D, G= 1, new12(A,B,E,H).
new10(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=A, K=L+ 1, L=C, M= 1, 
          new10(A,B,K,D,E,F,G,H).
new10(A,B,C,D,E,F,G,H) :- I>=J, I=C, J=A, new11(A,B,C,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, K= 1, new10(A,B,C,K,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I=< 0, I=B, J= 0, K= 2, new10(A,B,C,K,E,F,G,H).
new7(A,B,C,D) :- new9(A,B,E,F,C,D,G,H).
safe :- init(A,B), new7(A,B,C,D).
new6(A,B,C,C).
new5(A,B,C,D) :- E= 0, E=C, F= 0, new6(A,B,C,D).
new4(A,B,C,D,A,B,C,D) :- E= 1, F=< 1, F=D, G= 1, new5(A,B,E,H).
new4(A,B,C,D,A,B,C,D) :- E= 0, F>= 2, F=D, G= 1, new5(A,B,E,H).
new3(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=A, K=L+ 1, L=C, M= 1, 
          new3(A,B,K,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I>=J, I=C, J=A, new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, K= 1, new3(A,B,C,K,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I=< 0, I=B, J= 0, K= 2, new3(A,B,C,K,E,F,G,H).
new1(A,B,C,D) :- new2(A,B,E,F,C,D,G,H).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
