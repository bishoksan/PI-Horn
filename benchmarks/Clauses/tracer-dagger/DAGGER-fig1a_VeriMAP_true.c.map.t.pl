new28(A,B,C,C) :- D>= 1, D=C, E= 0.
new28(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new27(A,B,C,D,A,B,C,D) :- E=D, new28(A,B,E,F).
new26(A,B,C,D,E,F,G,H) :- I=< 0, I=B, J= 0, K= 1, new27(A,B,C,K,E,F,G,H).
new26(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, K= 0, new27(A,B,C,K,E,F,G,H).
new25(A,B,C,D,E,F,G,H) :- I>= 0, I=B, J= 0, new26(A,B,C,D,E,F,G,H).
new25(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, J= 0, K= 0, new27(A,B,C,K,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=A, J= 0, new23(A,B,C,D,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I>= 0, I=A, J= 0, new25(A,B,C,D,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- I=J- 1, J=A, K= 1, L=M- 1, M=B, N= 1, 
          new21(I,L,C,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I>= 1, I=A, J= 0, new23(A,B,C,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I=< 0, I=A, J= 0, new24(A,B,C,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- new22(A,B,C,D,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K=L+ 1, L=A, M= 1, N=O+ 1, O=B, 
          P= 1, new18(K,N,C,D,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K=L+ 1, L=A, M= 1, N=O+ 1, O=B, 
          P= 1, new18(K,N,C,D,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, new21(A,B,C,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- new20(A,B,I,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- new19(A,B,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- new18(A,B,C,D,E,F,G,H).
new15(A,B,C,D) :- new17(A,B,E,F,C,D,G,H).
safe :- init(A,B), new15(A,B,C,D).
new14(A,B,C,C).
new13(A,B,C,D) :- E= 0, E=C, F= 0, new14(A,B,C,D).
new12(A,B,C,D,A,B,C,D) :- E=D, new13(A,B,E,F).
new11(A,B,C,D,E,F,G,H) :- I=< 0, I=B, J= 0, K= 1, new12(A,B,C,K,E,F,G,H).
new11(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, K= 0, new12(A,B,C,K,E,F,G,H).
new10(A,B,C,D,E,F,G,H) :- I>= 0, I=B, J= 0, new11(A,B,C,D,E,F,G,H).
new10(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, J= 0, K= 0, new12(A,B,C,K,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=A, J= 0, new8(A,B,C,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I>= 0, I=A, J= 0, new10(A,B,C,D,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I=J- 1, J=A, K= 1, L=M- 1, M=B, N= 1, 
          new6(I,L,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I>= 1, I=A, J= 0, new8(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I=< 0, I=A, J= 0, new9(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- new7(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K=L+ 1, L=A, M= 1, N=O+ 1, O=B, 
          P= 1, new3(K,N,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K=L+ 1, L=A, M= 1, N=O+ 1, O=B, 
          P= 1, new3(K,N,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, new6(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- new5(A,B,I,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,B,C,D,E,F,G,H).
new1(A,B,C,D) :- new2(A,B,E,F,C,D,G,H).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
