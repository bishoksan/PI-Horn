new18(A,B,B) :- C>= 1, C=B, D= 0.
new18(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new17(A,B,A,B) :- C= 1, D>= 11, D=A, E= 10, new18(A,C,F).
new17(A,B,A,B) :- C= 1, D+ 1=< 10, D=A, E= 10, new18(A,C,F).
new17(A,B,A,B) :- C= 0, D= 10, D=A, E= 10, new18(A,C,F).
new16(A,B,C,D) :- E+ 1=< 10, E=B, F= 10, new14(A,B,C,D).
new16(A,B,C,D) :- E>= 10, E=B, F= 10, new17(A,B,C,D).
new15(A,B,C,D) :- E=F, new6(A,G,H,F), new16(H,E,C,D).
new14(A,B,C,D) :- new15(A,B,C,D).
new13(A,B,C,D) :- new14(A,B,C,D).
new11(A,B) :- new13(A,C,B,D).
safe :- init(A), new11(A,B).
new10(A,B,B).
new9(A,B,C) :- D= 0, D=B, E= 0, new10(A,B,C).
new8(A,B,A,B) :- C= 1, D>= 11, D=A, E= 10, new9(A,C,F).
new8(A,B,A,B) :- C= 1, D+ 1=< 10, D=A, E= 10, new9(A,C,F).
new8(A,B,A,B) :- C= 0, D= 10, D=A, E= 10, new9(A,C,F).
new7(A,B,C,D) :- E+ 1=< 10, E=B, F= 10, new3(A,B,C,D).
new7(A,B,C,D) :- E>= 10, E=B, F= 10, new8(A,B,C,D).
new6(A,B,A,C) :- C=D+ 1, D=B, E= 1.
new4(A,B,C,D) :- E=F, new6(A,G,H,F), new7(H,E,C,D).
new3(A,B,C,D) :- new4(A,B,C,D).
new2(A,B,C,D) :- new3(A,B,C,D).
new1(A,B) :- new2(A,C,B,D).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
