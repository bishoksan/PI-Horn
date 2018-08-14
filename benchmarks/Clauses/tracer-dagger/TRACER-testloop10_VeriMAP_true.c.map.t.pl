new19(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, K= 0, L=M+ 1, M=C, N= 1, O= 1, 
          new13(O,B,L,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=D, J= 0, K= 0, L=M+ 1, M=C, N= 1, O= 1, 
          new13(O,B,L,D,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- I= 0, I=D, J= 0, new13(A,B,C,D,E,F,G,H).
new17(A,B,B) :- C>= 1, C=B, D= 0.
new17(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new16(A,B,C,D,A,B,C,D) :- E= 1, F>= 1, F=A, G= 0, new17(A,E,H).
new16(A,B,C,D,A,B,C,D) :- E= 1, F+ 1=< 0, F=A, G= 0, new17(A,E,H).
new16(A,B,C,D,A,B,C,D) :- E= 0, F= 0, F=A, G= 0, new17(A,E,H).
new15(A,B,C,D,E,F,G,H) :- new19(A,B,C,I,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=C, J=B, K=C, new15(A,K,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=B, K=C, new15(A,K,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I=J, I=C, J=B, new16(A,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- new14(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I= 0, J= 0, K= 1, L= 0, M= 1, new13(A,I,K,D,E,F,G,H).
new10(A,B) :- new12(A,C,D,E,B,F,G,H).
safe :- init(A), new10(A,B).
new9(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, K= 0, L=M+ 1, M=C, N= 1, O= 1, 
          new3(O,B,L,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=D, J= 0, K= 0, L=M+ 1, M=C, N= 1, O= 1, 
          new3(O,B,L,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I= 0, I=D, J= 0, new3(A,B,C,D,E,F,G,H).
new8(A,B,B).
new7(A,B,C) :- D= 0, D=B, E= 0, new8(A,B,C).
new6(A,B,C,D,A,B,C,D) :- E= 1, F>= 1, F=A, G= 0, new7(A,E,H).
new6(A,B,C,D,A,B,C,D) :- E= 1, F+ 1=< 0, F=A, G= 0, new7(A,E,H).
new6(A,B,C,D,A,B,C,D) :- E= 0, F= 0, F=A, G= 0, new7(A,E,H).
new5(A,B,C,D,E,F,G,H) :- new9(A,B,C,I,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I>=J+ 1, I=C, J=B, K=C, new5(A,K,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=C, J=B, K=C, new5(A,K,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I=J, I=C, J=B, new6(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I= 0, J= 0, K= 1, L= 0, M= 1, new3(A,I,K,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
