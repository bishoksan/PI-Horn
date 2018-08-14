new24(A,B,C,D,E,F,G,H) :- I= 1, J>= 1, J=A, K= 0, L=M- 1, M=D, N= 1, O=P- 1, 
          P=A, Q= 1, new10(A,I,R), new22(O,B,C,L,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I= 0, J=< 0, J=A, K= 0, L=M- 1, M=D, N= 1, O=P- 1, 
          P=A, Q= 1, new10(A,I,R), new22(O,B,C,L,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, new24(A,B,C,D,E,F,G,H).
new23(A,B,C,D,A,B,C,D) :- E=< 0, E=D, F= 0.
new22(A,B,C,D,E,F,G,H) :- new23(A,B,C,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=B, K=L+ 1, L=C, M= 1, N=O+ 1, O=A, 
          P= 1, new20(N,B,K,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I>=J, I=C, J=B, K=B, new22(A,B,C,K,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- new21(A,B,C,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- new20(A,I,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I= 0, new19(A,B,I,D,E,F,G,H).
new16(A,B) :- new18(A,C,D,E,B,F,G,H).
safe :- init(A), new16(A,B).
new15(A).
new13(A,B,B) :- new15(A).
new12(A,B,B).
new10(A,B,C) :- D>= 1, D=B, E= 0, new12(A,B,C).
new10(A,B,C) :- D+ 1=< 0, D=B, E= 0, new12(A,B,C).
new9(A,B,C) :- D= 0, D=B, E= 0, new13(A,B,C).
new8(A,B,C,D,A,B,C,D) :- E= 1, F>= 1, F=A, G= 0, new9(A,E,H).
new8(A,B,C,D,E,F,G,H) :- I= 1, J>= 1, J=A, K= 0, L=M- 1, M=D, N= 1, O=P- 1, 
          P=A, Q= 1, new10(A,I,R), new6(O,B,C,L,E,F,G,H).
new8(A,B,C,D,A,B,C,D) :- E= 0, F=< 0, F=A, G= 0, new9(A,E,H).
new8(A,B,C,D,E,F,G,H) :- I= 0, J=< 0, J=A, K= 0, L=M- 1, M=D, N= 1, O=P- 1, 
          P=A, Q= 1, new10(A,I,R), new6(O,B,C,L,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, new8(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- new7(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=B, K=L+ 1, L=C, M= 1, N=O+ 1, O=A, 
          P= 1, new4(N,B,K,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I>=J, I=C, J=B, K=B, new6(A,B,C,K,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- new5(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,I,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I= 0, new3(A,B,I,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
