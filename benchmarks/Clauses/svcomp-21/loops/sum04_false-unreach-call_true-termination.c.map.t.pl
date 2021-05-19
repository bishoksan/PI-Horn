new26(A,B,C,D,E,F,G,H) :- I=J+ 1, J=B, K= 1, new17(A,I,C,D,E,F,G,H).
new25(A,B,B).
new23(A,B,C) :- D>= 1, D=B, E= 0, new25(A,B,C).
new23(A,B,C) :- D+ 1=< 0, D=B, E= 0, new25(A,B,C).
new22(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, K= 1, new21(A,B,C,K,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K= 0, new21(A,B,C,K,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K= 0, new21(A,B,C,K,E,F,G,H).
new21(A,B,C,D,A,B,C,D) :- E=D, new23(A,E,F).
new20(A,B,C,D,E,F,G,H) :- I=J, I=C, J=K* 2, K=A, L= 2, M= 1, 
          new21(A,B,C,M,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=C, J=K* 2, K=A, L= 2, 
          new22(A,B,C,D,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=K* 2, K=A, L= 2, 
          new22(A,B,C,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- I+ 1=< 4, I=B, J= 4, K=L+ 2, L=C, M= 2, 
          new26(A,B,K,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- I>= 4, I=B, J= 4, new26(A,B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I=<J, I=B, J=A, new19(A,B,C,D,E,F,G,H).
new18(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=B, J=A, new20(A,B,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- new18(A,B,C,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I= 0, J= 1, new17(A,J,I,D,E,F,G,H).
new14(A,B) :- new16(A,C,D,E,B,F,G,H).
safe :- init(A), new14(A,B).
new13(A,B,C,D,E,F,G,H) :- I=J+ 1, J=B, K= 1, new3(A,I,C,D,E,F,G,H).
new12(A).
new10(A,B,B) :- new12(A).
new9(A,B,C) :- D= 0, D=B, E= 0, new10(A,B,C).
new8(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, K= 1, new7(A,B,C,K,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K= 0, new7(A,B,C,K,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K= 0, new7(A,B,C,K,E,F,G,H).
new7(A,B,C,D,A,B,C,D) :- E=D, new9(A,E,F).
new6(A,B,C,D,E,F,G,H) :- I=J, I=C, J=K* 2, K=A, L= 2, M= 1, 
          new7(A,B,C,M,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=C, J=K* 2, K=A, L= 2, new8(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=K* 2, K=A, L= 2, new8(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I+ 1=< 4, I=B, J= 4, K=L+ 2, L=C, M= 2, 
          new13(A,B,K,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I>= 4, I=B, J= 4, new13(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I=<J, I=B, J=A, new5(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=B, J=A, new6(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I= 0, J= 1, new3(A,J,I,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
