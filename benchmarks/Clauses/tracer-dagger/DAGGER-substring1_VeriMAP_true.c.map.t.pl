new38(A,B,C,D,A,B,C,D) :- E>= 101, E=D, F= 100.
new33(A,B,C,D,A,B,C,D) :- E+ 1=< 0, E=C, F= 0.
new32(A,B,C,D,E,F,G,H) :- I=< 100, I=D, J= 100, new33(A,B,C,D,E,F,G,H).
new28(A,B,C,D,A,B,C,D) :- E>=F+ 1, E=C, F=D.
new27(A,B,C,D,E,F,G,H) :- I>= 0, I=C, J= 0, new28(A,B,C,D,E,F,G,H).
new26(A,B,C,D,E,F,G,H) :- I=< 100, I=D, J= 100, new27(A,B,C,D,E,F,G,H).
new24(A,B,B) :- C>= 1, C=B, D= 0.
new24(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new23(A,B,C,D,A,B,C,D) :- E= 1, F=< 100, F=A, G= 100, new24(A,E,H).
new23(A,B,C,D,A,B,C,D) :- E= 0, F>= 101, F=A, G= 100, new24(A,E,H).
new22(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=B, J=D, K=L+ 1, L=B, M= 1, N=O+ 1, O=A, 
          P= 1, new21(N,K,C,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I>=J, I=B, J=D, new23(A,B,C,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- new22(A,B,C,D,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I=<J, I=C, J=D, K=C, new21(A,K,C,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- I>= 0, I=C, J= 0, new20(A,B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I=< 100, I=D, J= 100, new19(A,B,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- I>= 0, I=D, J= 0, new18(A,B,C,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I>= 0, I=D, J= 0, new26(A,B,C,D,E,F,G,H).
new15(A,B,C,D,E,F,G,H) :- I>= 0, I=D, J= 0, new32(A,B,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I>= 0, I=D, J= 0, new38(A,B,C,D,E,F,G,H).
new13(A,B,C,D,A,B,C,D) :- E+ 1=< 0, E=D, F= 0.
new11(A,B) :- new13(A,C,D,E,B,F,G,H).
new11(A,B) :- new14(A,C,D,E,B,F,G,H).
new11(A,B) :- new15(A,C,D,E,B,F,G,H).
new11(A,B) :- new16(A,C,D,E,B,F,G,H).
new11(A,B) :- new17(A,C,D,E,B,F,G,H).
safe :- init(A), new11(A,B).
new10(A,B,B).
new9(A,B,C) :- D= 0, D=B, E= 0, new10(A,B,C).
new8(A,B,C,D,A,B,C,D) :- E= 1, F=< 100, F=A, G= 100, new9(A,E,H).
new8(A,B,C,D,A,B,C,D) :- E= 0, F>= 101, F=A, G= 100, new9(A,E,H).
new7(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=B, J=D, K=L+ 1, L=B, M= 1, N=O+ 1, O=A, 
          P= 1, new6(N,K,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I>=J, I=B, J=D, new8(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- new7(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I=<J, I=C, J=D, K=C, new6(A,K,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I>= 0, I=C, J= 0, new5(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I=< 100, I=D, J= 100, new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I>= 0, I=D, J= 0, new3(A,B,C,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
