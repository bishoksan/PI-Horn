new15(A,B,A,B).
new14(A,B,C,D) :- E= 1, F= 0, F=B, G= 0, new7(A,E,H), new15(A,B,C,D).
new14(A,B,C,D) :- E= 0, F>= 1, F=B, G= 0, new7(A,E,H), new15(A,B,C,D).
new14(A,B,C,D) :- E= 0, F+ 1=< 0, F=B, G= 0, new7(A,E,H), new15(A,B,C,D).
new13(A,B,C,D) :- E= 0, E=B, F= 0, new14(A,B,C,D).
new13(A,B,C,D) :- E>= 1, E=B, F= 0, new15(A,B,C,D).
new13(A,B,C,D) :- E+ 1=< 0, E=B, F= 0, new15(A,B,C,D).
new12(A,B,C,D) :- E>= 1, E=A, F= 0, G= 0, new13(A,G,C,D).
new12(A,B,C,D) :- E=< 0, E=A, F= 0, G= 1, new13(A,G,C,D).
new10(A,B) :- new12(A,C,B,D).
safe :- init(A), new10(A,B).
new9(A,B,B).
new7(A,B,B) :- C>= 1, C=B, D= 0.
new7(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new6(A,B,C) :- D= 0, D=B, E= 0, new9(A,B,C).
new4(A,B,A,B) :- C= 1, D= 0, D=B, E= 0, new6(A,C,F).
new4(A,B,A,B) :- C= 0, D>= 1, D=B, E= 0, new6(A,C,F).
new4(A,B,A,B) :- C= 0, D+ 1=< 0, D=B, E= 0, new6(A,C,F).
new3(A,B,C,D) :- E= 0, E=B, F= 0, new4(A,B,C,D).
new2(A,B,C,D) :- E>= 1, E=A, F= 0, G= 0, new3(A,G,C,D).
new2(A,B,C,D) :- E=< 0, E=A, F= 0, G= 1, new3(A,G,C,D).
new1(A,B) :- new2(A,C,B,D).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
