new40(A,B,C,C).
new38(A,B,C,D) :- E>= 1, E=C, F= 0, new40(A,B,C,D).
new38(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new40(A,B,C,D).
new37(A,B,C,D,A,B,C,D) :- E= 1, F+ 1=< 200, F=A, G= 200, new38(A,B,E,H).
new37(A,B,C,D,A,B,C,D) :- E= 0, F>= 200, F=A, G= 200, new38(A,B,E,H).
new36(A,B,C,D,E,F,G,H) :- I+ 1=< 100, I=B, J= 100, new37(A,B,C,D,E,F,G,H).
new35(A,B,C,D,E,F,G,H) :- new36(A,B,C,D,E,F,G,H).
new34(A,B,C,D,E,F,G,H) :- I+ 1=< 1000000, I=A, J= 1000000, K=L+ 1, L=A, M= 1, 
          N=O+ 1, O=B, P= 1, new31(K,N,C,D,E,F,G,H).
new34(A,B,C,D,E,F,G,H) :- I>= 1000000, I=A, J= 1000000, new35(A,B,C,D,E,F,G,H).
new33(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, new34(A,B,C,D,E,F,G,H).
new33(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=D, J= 0, new34(A,B,C,D,E,F,G,H).
new33(A,B,C,D,E,F,G,H) :- I= 0, I=D, J= 0, new35(A,B,C,D,E,F,G,H).
new32(A,B,C,D,E,F,G,H) :- new33(A,B,C,I,E,F,G,H).
new31(A,B,C,D,E,F,G,H) :- new32(A,B,C,D,E,F,G,H).
new29(A,B,C,D,E,F,G,H) :- I+ 1=< 100, I=A, J= 100, new31(A,B,C,D,E,F,G,H).
new28(A,B,C,D,E,F,G,H) :- new29(A,B,C,D,E,F,G,H).
new27(A,B,C,D,E,F,G,H) :- I+ 1=< 1000000, I=A, J= 1000000, K=L+ 1, L=A, M= 1, 
          new24(K,B,C,D,E,F,G,H).
new27(A,B,C,D,E,F,G,H) :- I>= 1000000, I=A, J= 1000000, new28(A,B,C,D,E,F,G,H).
new26(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, new27(A,B,C,D,E,F,G,H).
new26(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, new27(A,B,C,D,E,F,G,H).
new26(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, new28(A,B,C,D,E,F,G,H).
new25(A,B,C,D,E,F,G,H) :- new26(A,B,I,D,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- new25(A,B,C,D,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- new24(A,B,C,D,E,F,G,H).
new21(A,B,C,D) :- new23(A,B,E,F,C,D,G,H).
safe :- init(A,B), new21(A,B,C,D).
new20(A,B).
new18(A,B,C,C) :- new20(A,B).
new17(A,B,C,D) :- E= 0, E=C, F= 0, new18(A,B,C,D).
new16(A,B,C,D,A,B,C,D) :- E= 1, F+ 1=< 200, F=A, G= 200, new17(A,B,E,H).
new16(A,B,C,D,A,B,C,D) :- E= 0, F>= 200, F=A, G= 200, new17(A,B,E,H).
new15(A,B,C,D,E,F,G,H) :- I+ 1=< 100, I=B, J= 100, new16(A,B,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- new15(A,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I+ 1=< 1000000, I=A, J= 1000000, K=L+ 1, L=A, M= 1, 
          N=O+ 1, O=B, P= 1, new10(K,N,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I>= 1000000, I=A, J= 1000000, new14(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, new13(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=D, J= 0, new13(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I= 0, I=D, J= 0, new14(A,B,C,D,E,F,G,H).
new11(A,B,C,D,E,F,G,H) :- new12(A,B,C,I,E,F,G,H).
new10(A,B,C,D,E,F,G,H) :- new11(A,B,C,D,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I+ 1=< 100, I=A, J= 100, new10(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- new8(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I+ 1=< 1000000, I=A, J= 1000000, K=L+ 1, L=A, M= 1, 
          new3(K,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I>= 1000000, I=A, J= 1000000, new7(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, new6(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, new6(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, new7(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- new5(A,B,I,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,B,C,D,E,F,G,H).
new1(A,B,C,D) :- new2(A,B,E,F,C,D,G,H).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
