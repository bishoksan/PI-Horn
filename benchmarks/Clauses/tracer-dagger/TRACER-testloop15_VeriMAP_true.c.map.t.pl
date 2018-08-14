new14(A,B,B) :- C>= 1, C=B, D= 0.
new14(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new13(A,B,A,B) :- C= 1, D=<E, D=B, E=A, new14(A,C,F).
new13(A,B,A,B) :- C= 0, D>=E+ 1, D=B, E=A, new14(A,C,F).
new12(A,B,C,D) :- E+ 1=<F, E=B, F=A, G=H+ 1, H=B, I= 1, new11(A,G,C,D).
new12(A,B,C,D) :- E>=F, E=B, F=A, new13(A,B,C,D).
new11(A,B,C,D) :- new12(A,B,C,D).
new10(A,B,C,D) :- E= 0, new11(A,E,C,D).
new8(A,B) :- new10(A,C,B,D).
safe :- init(A), new8(A,B).
new7(A,B,B).
new6(A,B,C) :- D= 0, D=B, E= 0, new7(A,B,C).
new5(A,B,A,B) :- C= 1, D=<E, D=B, E=A, new6(A,C,F).
new5(A,B,A,B) :- C= 0, D>=E+ 1, D=B, E=A, new6(A,C,F).
new4(A,B,C,D) :- E+ 1=<F, E=B, F=A, G=H+ 1, H=B, I= 1, new3(A,G,C,D).
new4(A,B,C,D) :- E>=F, E=B, F=A, new5(A,B,C,D).
new3(A,B,C,D) :- new4(A,B,C,D).
new2(A,B,C,D) :- E= 0, new3(A,E,C,D).
new1(A,B) :- new2(A,C,B,D).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
