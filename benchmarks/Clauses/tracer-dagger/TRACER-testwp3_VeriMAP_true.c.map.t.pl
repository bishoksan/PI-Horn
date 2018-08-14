new12(A,B,B) :- C>= 1, C=B, D= 0.
new12(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new11(A,B,A,B) :- C= 1, D=< 49, D=B, E= 49, new12(A,C,F).
new11(A,B,A,B) :- C= 0, D>= 50, D=B, E= 49, new12(A,C,F).
new10(A,B,C,D) :- E=F+ 1, F=B, G= 1, H=I+ 2, I=E, J= 2, new11(A,H,C,D).
new9(A,B,C,D) :- E>= 1, E=A, F= 0, G= 2, new10(A,G,C,D).
new9(A,B,C,D) :- E=< 0, E=A, F= 0, G= 47, new10(A,G,C,D).
new7(A,B) :- new9(A,C,B,D).
safe :- init(A), new7(A,B).
new6(A,B,B).
new5(A,B,C) :- D= 0, D=B, E= 0, new6(A,B,C).
new4(A,B,A,B) :- C= 1, D=< 49, D=B, E= 49, new5(A,C,F).
new4(A,B,A,B) :- C= 0, D>= 50, D=B, E= 49, new5(A,C,F).
new3(A,B,C,D) :- E=F+ 1, F=B, G= 1, H=I+ 2, I=E, J= 2, new4(A,H,C,D).
new2(A,B,C,D) :- E>= 1, E=A, F= 0, G= 2, new3(A,G,C,D).
new2(A,B,C,D) :- E=< 0, E=A, F= 0, G= 47, new3(A,G,C,D).
new1(A,B) :- new2(A,C,B,D).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
