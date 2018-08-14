new19(A,B,C,D,E,F,G,H) :- I=J+ 1, J=C, K= 1, L=M+ 1, M=B, N= 1, 
          new13(A,L,I,D,E,F,G,H).
new17(A,B,B) :- C>= 1, C=B, D= 0.
new17(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new16(A,B,C,D,A,B,C,D) :- E= 1, F=< 0, F=D, G= 0, new17(A,E,H).
new16(A,B,C,D,A,B,C,D) :- E= 0, F>= 1, F=D, G= 0, new17(A,E,H).
new15(A,B,C,D,E,F,G,H) :- I+ 1=< 1, I=B, J= 1, K= 1, new19(A,B,C,K,E,F,G,H).
new15(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 1, K= 0, new19(A,B,C,K,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=B, J=A, new15(A,B,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I>=J, I=B, J=A, new16(A,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- new14(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- I= 0, J= 0, K= 0, new13(A,I,J,K,E,F,G,H).
new10(A,B) :- new12(A,C,D,E,B,F,G,H).
safe :- init(A), new10(A,B).
new9(A,B,C,D,E,F,G,H) :- I=J+ 1, J=C, K= 1, L=M+ 1, M=B, N= 1, 
          new3(A,L,I,D,E,F,G,H).
new8(A,B,B).
new7(A,B,C) :- D= 0, D=B, E= 0, new8(A,B,C).
new6(A,B,C,D,A,B,C,D) :- E= 1, F=< 0, F=D, G= 0, new7(A,E,H).
new6(A,B,C,D,A,B,C,D) :- E= 0, F>= 1, F=D, G= 0, new7(A,E,H).
new5(A,B,C,D,E,F,G,H) :- I+ 1=< 1, I=B, J= 1, K= 1, new9(A,B,C,K,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 1, K= 0, new9(A,B,C,K,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=B, J=A, new5(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I>=J, I=B, J=A, new6(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I= 0, J= 0, K= 0, new3(A,I,J,K,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
