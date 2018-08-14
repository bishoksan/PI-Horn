new24(A,B,C,D,A,B,C,D) :- E>= 100, E=D, F= 100.
new22(A,B,C,D,E,F,G,H) :- I=< 100, I=D, J= 100, new24(A,B,C,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I+ 1=< 50, I=C, J= 50, K=L+ 1, L=C, M= 1, 
          new19(A,B,K,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I>= 50, I=C, J= 50, K=L+ 1, L=C, M= 1, N=O+ 1, O=D, 
          P= 1, new19(A,B,K,N,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I+ 1=< 100, I=C, J= 100, new21(A,B,C,D,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I>= 100, I=C, J= 100, new22(A,B,C,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- new20(A,B,C,D,E,F,G,H).
new11(A,B,C,D,E,F,G,H) :- new19(A,B,C,D,E,F,G,H).
new9(A,B,C,D) :- new11(A,B,E,F,C,D,G,H).
safe :- init(A,B), new9(A,B,C,D).
new8(A,B,C,D,E,F,G,H) :- I+ 1=< 100, I=D, J= 100, new7(A,B,C,D,E,F,G,H).
new7(A,B,C,D,A,B,C,D).
new6(A,B,C,D,E,F,G,H) :- I>= 101, I=D, J= 100, new7(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I=< 100, I=D, J= 100, new8(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I+ 1=< 50, I=C, J= 50, K=L+ 1, L=C, M= 1, 
          new3(A,B,K,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I>= 50, I=C, J= 50, K=L+ 1, L=C, M= 1, N=O+ 1, O=D, 
          P= 1, new3(A,B,K,N,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I+ 1=< 100, I=C, J= 100, new5(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I>= 100, I=C, J= 100, new6(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,B,C,D,E,F,G,H).
new1(A,B,C,D) :- new2(A,B,E,F,C,D,G,H).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
