new18(A,B,C,C) :- D>= 1, D=C, E= 0.
new18(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new17(A,B,C,D,A,B,C,D) :- E= 1, F>= 3, F=C, G= 2, new18(A,B,E,H).
new17(A,B,C,D,A,B,C,D) :- E= 1, F+ 1=< 2, F=C, G= 2, new18(A,B,E,H).
new17(A,B,C,D,A,B,C,D) :- E= 0, F= 2, F=C, G= 2, new18(A,B,E,H).
new16(A,B,C,D,E,F,G,H) :- new7(A,B,I,J,K,L), new17(J,K,C,D,E,F,G,H).
new15(A,B,C,D,E,F,G,H) :- new16(A,B,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I>= 1, I=A, J= 0, K= 1, new15(A,B,K,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I=< 0, I=A, J= 0, K= 2, new15(A,B,K,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, K= 2, new14(A,B,C,K,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I=< 0, I=B, J= 0, K= 3, new14(A,B,C,K,E,F,G,H).
new11(A,B,C,D) :- new13(A,B,E,F,C,D,G,H).
safe :- init(A,B), new11(A,B,C,D).
new10(A,B,C,C).
new9(A,B,C,D) :- E= 0, E=C, F= 0, new10(A,B,C,D).
new8(A,B,C,D,A,B,C,D) :- E= 1, F>= 3, F=C, G= 2, new9(A,B,E,H).
new8(A,B,C,D,A,B,C,D) :- E= 1, F+ 1=< 2, F=C, G= 2, new9(A,B,E,H).
new8(A,B,C,D,A,B,C,D) :- E= 0, F= 2, F=C, G= 2, new9(A,B,E,H).
new7(A,B,C,A,B,D) :- D= 1.
new5(A,B,C,D,E,F,G,H) :- new7(A,B,I,J,K,L), new8(J,K,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- new5(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I>= 1, I=A, J= 0, K= 1, new4(A,B,K,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I=< 0, I=A, J= 0, K= 2, new4(A,B,K,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 0, K= 2, new3(A,B,C,K,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I=< 0, I=B, J= 0, K= 3, new3(A,B,C,K,E,F,G,H).
new1(A,B,C,D) :- new2(A,B,E,F,C,D,G,H).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
