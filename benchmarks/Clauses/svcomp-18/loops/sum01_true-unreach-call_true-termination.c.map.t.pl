new37(A,B,C,D,A,B,C,D) :- E+ 1=< -1000, E=A, F= -1000.
new36(A,B,C,D,E,F,G,H) :- I+ 1=< 1000, I=A, J= 1000, new37(A,B,C,D,E,F,G,H).
new29(A,B,C,D,A,B,C,D) :- E>= 1000, E=A, F= 1000.
new28(A,B,B).
new26(A,B,C) :- D>= 1, D=B, E= 0, new28(A,B,C).
new26(A,B,C) :- D+ 1=< 0, D=B, E= 0, new28(A,B,C).
new25(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, K= 1, new24(A,B,C,K,E,F,G,H).
new25(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K= 0, new24(A,B,C,K,E,F,G,H).
new25(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K= 0, new24(A,B,C,K,E,F,G,H).
new24(A,B,C,D,A,B,C,D) :- E=D, new26(A,E,F).
new23(A,B,C,D,E,F,G,H) :- I=J, I=C, J=K* 2, K=A, L= 2, M= 1, 
          new24(A,B,C,M,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=C, J=K* 2, K=A, L= 2, 
          new25(A,B,C,D,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=K* 2, K=A, L= 2, 
          new25(A,B,C,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I=<J, I=B, J=A, K=L+ 2, L=C, M= 2, N=O+ 1, O=B, P= 1, 
          new21(A,N,K,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=B, J=A, new23(A,B,C,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- new22(A,B,C,D,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I>= -1000, I=A, J= -1000, K= 1, 
          new21(A,K,C,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- I+ 1=< 1000, I=A, J= 1000, new20(A,B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I= 0, new19(A,B,I,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I= 0, new29(A,B,I,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I= 0, new36(A,B,I,D,E,F,G,H).
new14(A,B) :- new16(A,C,D,E,B,F,G,H).
new14(A,B) :- new17(A,C,D,E,B,F,G,H).
new14(A,B) :- new18(A,C,D,E,B,F,G,H).
safe :- init(A), new14(A,B).
new13(A).
new11(A,B,B) :- new13(A).
new10(A,B,C) :- D= 0, D=B, E= 0, new11(A,B,C).
new9(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, K= 1, new8(A,B,C,K,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K= 0, new8(A,B,C,K,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K= 0, new8(A,B,C,K,E,F,G,H).
new8(A,B,C,D,A,B,C,D) :- E=D, new10(A,E,F).
new7(A,B,C,D,E,F,G,H) :- I=J, I=C, J=K* 2, K=A, L= 2, M= 1, 
          new8(A,B,C,M,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=C, J=K* 2, K=A, L= 2, new9(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=K* 2, K=A, L= 2, new9(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I=<J, I=B, J=A, K=L+ 2, L=C, M= 2, N=O+ 1, O=B, P= 1, 
          new5(A,N,K,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=B, J=A, new7(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- new6(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I>= -1000, I=A, J= -1000, K= 1, new5(A,K,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I+ 1=< 1000, I=A, J= 1000, new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I= 0, new3(A,B,I,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
