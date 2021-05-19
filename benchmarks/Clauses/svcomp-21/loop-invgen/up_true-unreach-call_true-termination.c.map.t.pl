new22(A,B,C,D,E,F,G,H) :- I= 1, J>= 1, J=B, K= 0, L=M+ 1, M=D, N= 1, O=P- 1, 
          P=B, Q= 1, new9(A,B,I,R), new20(A,O,C,L,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I= 0, J=< 0, J=B, K= 0, L=M+ 1, M=D, N= 1, O=P- 1, 
          P=B, Q= 1, new9(A,B,I,R), new20(A,O,C,L,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=D, J=A, new22(A,B,C,D,E,F,G,H).
new21(A,B,C,D,A,B,C,D) :- E>=F, E=D, F=A.
new20(A,B,C,D,E,F,G,H) :- new21(A,B,C,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=A, K=L+ 1, L=C, M= 1, N=O+ 1, O=B, 
          P= 1, new18(A,N,K,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- I>=J, I=C, J=A, K= 0, new20(A,B,C,K,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- new19(A,B,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I= 0, new18(A,B,I,D,E,F,G,H).
new15(A,B,C,D) :- new17(A,B,E,F,C,D,G,H).
safe :- init(A,B), new15(A,B,C,D).
new14(A,B).
new12(A,B,C,C) :- new14(A,B).
new11(A,B,C,C).
new9(A,B,C,D) :- E>= 1, E=C, F= 0, new11(A,B,C,D).
new9(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new11(A,B,C,D).
new8(A,B,C,D) :- E= 0, E=C, F= 0, new12(A,B,C,D).
new7(A,B,C,D,A,B,C,D) :- E= 1, F>= 1, F=B, G= 0, new8(A,B,E,H).
new7(A,B,C,D,E,F,G,H) :- I= 1, J>= 1, J=B, K= 0, L=M+ 1, M=D, N= 1, O=P- 1, 
          P=B, Q= 1, new9(A,B,I,R), new5(A,O,C,L,E,F,G,H).
new7(A,B,C,D,A,B,C,D) :- E= 0, F=< 0, F=B, G= 0, new8(A,B,E,H).
new7(A,B,C,D,E,F,G,H) :- I= 0, J=< 0, J=B, K= 0, L=M+ 1, M=D, N= 1, O=P- 1, 
          P=B, Q= 1, new9(A,B,I,R), new5(A,O,C,L,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=D, J=A, new7(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- new6(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=A, K=L+ 1, L=C, M= 1, N=O+ 1, O=B, 
          P= 1, new3(A,N,K,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I>=J, I=C, J=A, K= 0, new5(A,B,C,K,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I= 0, new3(A,B,I,D,E,F,G,H).
new1(A,B,C,D) :- new2(A,B,E,F,C,D,G,H).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
