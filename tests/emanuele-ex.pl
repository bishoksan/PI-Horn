new10(A,B,A,B) :- A+ 1=<C, C= 5.
new9(A,B,C,D,E,E) :- E=F+G, G= 42, new10(A,H,D,F).
new8(A,B,C,D,E,F) :- new9(A,B,C,D,E,F).
new6(A,B) :- new8(A,C,D,B,E,F).
safe :- init(A), new6(A,B).
new5(A,B,A,B).
new4(A,B,C,D) :- A>=E, E= 5, new5(A,B,C,D).
new3(A,B,C,D,B,C) :- new4(A,E,D,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A) :- A= 0.
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
