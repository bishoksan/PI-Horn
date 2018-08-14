new29(A,B,C,D,A,B,C,D) :- E= 1, F+ 1=< 7, F=A, G= 7, new7(A,E,H).
new29(A,B,C,D,A,B,C,D) :- E= 0, F>= 7, F=A, G= 7, new7(A,E,H).
new28(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, K=L+ 4, L=A, M= 4, 
          new29(K,B,C,D,E,F,G,H).
new28(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=D, J= 0, K=L+ 4, L=A, M= 4, 
          new29(K,B,C,D,E,F,G,H).
new28(A,B,C,D,E,F,G,H) :- I= 0, I=D, J= 0, new29(A,B,C,D,E,F,G,H).
new27(A,B,C,D,E,F,G,H) :- new28(A,B,C,I,E,F,G,H).
new26(A,B,C,D,E,F,G,H) :- I= 1, J=< 3, J=A, K= 3, new7(A,I,L), 
          new27(A,B,C,D,E,F,G,H).
new26(A,B,C,D,E,F,G,H) :- I= 0, J>= 4, J=A, K= 3, new7(A,I,L), 
          new27(A,B,C,D,E,F,G,H).
new25(A,B,C,D,E,F,G,H) :- new26(A,B,C,D,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K=L+ 2, L=A, M= 2, 
          new25(K,B,C,D,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K=L+ 2, L=A, M= 2, 
          new25(K,B,C,D,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, new25(A,B,C,D,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- new24(A,B,I,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I= 1, J=< 1, J=A, K= 1, new7(A,I,L), 
          new23(A,B,C,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I= 0, J>= 2, J=A, K= 1, new7(A,I,L), 
          new23(A,B,C,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- new22(A,B,C,D,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, K=L+ 1, L=A, M= 1, 
          new21(K,B,C,D,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, J= 0, K=L+ 1, L=A, M= 1, 
          new21(K,B,C,D,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I= 0, I=B, J= 0, new21(A,B,C,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- new20(A,I,C,D,E,F,G,H).
new17(A,B) :- new19(A,C,D,E,B,F,G,H).
safe :- init(A), new17(A,B).
new16(A,B,B).
new14(A,B,C,D,A,B,C,D) :- E= 1, F+ 1=< 7, F=A, G= 7, new6(A,E,H).
new14(A,B,C,D,A,B,C,D) :- E= 0, F>= 7, F=A, G= 7, new6(A,E,H).
new13(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, K=L+ 4, L=A, M= 4, 
          new14(K,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=D, J= 0, K=L+ 4, L=A, M= 4, 
          new14(K,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I= 0, I=D, J= 0, new14(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- new13(A,B,C,I,E,F,G,H).
new11(A,B,C,D,A,B,C,D) :- E= 1, F=< 3, F=A, G= 3, new6(A,E,H).
new11(A,B,C,D,E,F,G,H) :- I= 1, J=< 3, J=A, K= 3, new7(A,I,L), 
          new12(A,B,C,D,E,F,G,H).
new11(A,B,C,D,A,B,C,D) :- E= 0, F>= 4, F=A, G= 3, new6(A,E,H).
new11(A,B,C,D,E,F,G,H) :- I= 0, J>= 4, J=A, K= 3, new7(A,I,L), 
          new12(A,B,C,D,E,F,G,H).
new10(A,B,C,D,E,F,G,H) :- new11(A,B,C,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K=L+ 2, L=A, M= 2, 
          new10(K,B,C,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K=L+ 2, L=A, M= 2, 
          new10(K,B,C,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, new10(A,B,C,D,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- new9(A,B,I,D,E,F,G,H).
new7(A,B,B) :- C>= 1, C=B, D= 0.
new7(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new6(A,B,C) :- D= 0, D=B, E= 0, new16(A,B,C).
new5(A,B,C,D,A,B,C,D) :- E= 1, F=< 1, F=A, G= 1, new6(A,E,H).
new5(A,B,C,D,E,F,G,H) :- I= 1, J=< 1, J=A, K= 1, new7(A,I,L), 
          new8(A,B,C,D,E,F,G,H).
new5(A,B,C,D,A,B,C,D) :- E= 0, F>= 2, F=A, G= 1, new6(A,E,H).
new5(A,B,C,D,E,F,G,H) :- I= 0, J>= 2, J=A, K= 1, new7(A,I,L), 
          new8(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- new5(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, K=L+ 1, L=A, M= 1, 
          new4(K,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=B, J= 0, K=L+ 1, L=A, M= 1, 
          new4(K,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I= 0, I=B, J= 0, new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,I,C,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
