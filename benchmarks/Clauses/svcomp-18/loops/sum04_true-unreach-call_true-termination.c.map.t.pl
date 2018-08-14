new22(A,B,B).
new20(A,B,C) :- D>= 1, D=B, E= 0, new22(A,B,C).
new20(A,B,C) :- D+ 1=< 0, D=B, E= 0, new22(A,B,C).
new19(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, K= 1, new18(A,B,C,K,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K= 0, new18(A,B,C,K,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K= 0, new18(A,B,C,K,E,F,G,H).
new18(A,B,C,D,A,B,C,D) :- E=D, new20(A,E,F).
new17(A,B,C,D,E,F,G,H) :- I=J, I=C, J=K* 2, K=A, L= 2, M= 1, 
          new18(A,B,C,M,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=C, J=K* 2, K=A, L= 2, 
          new19(A,B,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=K* 2, K=A, L= 2, 
          new19(A,B,C,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I=<J, I=B, J=A, K=L+ 2, L=C, M= 2, N=O+ 1, O=B, P= 1, 
          new15(A,N,K,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=B, J=A, new17(A,B,C,D,E,F,G,H).
new15(A,B,C,D,E,F,G,H) :- new16(A,B,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I= 0, J= 1, new15(A,J,I,D,E,F,G,H).
new12(A,B) :- new14(A,C,D,E,B,F,G,H).
safe :- init(A), new12(A,B).
new11(A).
new9(A,B,B) :- new11(A).
new8(A,B,C) :- D= 0, D=B, E= 0, new9(A,B,C).
new7(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, K= 1, new6(A,B,C,K,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K= 0, new6(A,B,C,K,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K= 0, new6(A,B,C,K,E,F,G,H).
new6(A,B,C,D,A,B,C,D) :- E=D, new8(A,E,F).
new5(A,B,C,D,E,F,G,H) :- I=J, I=C, J=K* 2, K=A, L= 2, M= 1, 
          new6(A,B,C,M,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=C, J=K* 2, K=A, L= 2, new7(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=K* 2, K=A, L= 2, new7(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I=<J, I=B, J=A, K=L+ 2, L=C, M= 2, N=O+ 1, O=B, P= 1, 
          new3(A,N,K,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=B, J=A, new5(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I= 0, J= 1, new3(A,J,I,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
