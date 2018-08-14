new17(A,B,C,D) :- E= 1, F>= 0, F=A, G= 0, new8(A,B,E,H), new15(A,B,C,D).
new17(A,B,C,D) :- E= 0, F+ 1=< 0, F=A, G= 0, new8(A,B,E,H), new15(A,B,C,D).
new16(A,B,C,D) :- E+ 1=< 50, E=A, F= 50, new17(A,B,C,D).
new16(A,B,C,D) :- E>= 50, E=A, F= 50, new15(A,B,C,D).
new15(A,B,A,B).
new14(A,B,C,D) :- E>= 1, E=B, F= 0, G=H+ 1, H=B, I= 1, new16(A,G,C,D).
new14(A,B,C,D) :- E=< 0, E=B, F= 0, G=H- 10, H=A, I= 10, new16(G,B,C,D).
new13(A,B,C,D) :- E>= 6, E=A, F= 5, new14(A,B,C,D).
new13(A,B,C,D) :- E=< 5, E=A, F= 5, new15(A,B,C,D).
new11(A,B,C,D) :- new13(A,B,C,D).
safe :- init(A,B), new11(A,B,C,D).
new10(A,B,C,C).
new8(A,B,C,C) :- D>= 1, D=C, E= 0.
new8(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new7(A,B,C,D) :- E= 0, E=C, F= 0, new10(A,B,C,D).
new6(A,B,A,B) :- C= 1, D>= 0, D=A, E= 0, new7(A,B,C,F).
new6(A,B,A,B) :- C= 0, D+ 1=< 0, D=A, E= 0, new7(A,B,C,F).
new5(A,B,C,D) :- E+ 1=< 50, E=A, F= 50, new6(A,B,C,D).
new3(A,B,C,D) :- E>= 1, E=B, F= 0, G=H+ 1, H=B, I= 1, new5(A,G,C,D).
new3(A,B,C,D) :- E=< 0, E=B, F= 0, G=H- 10, H=A, I= 10, new5(G,B,C,D).
new2(A,B,C,D) :- E>= 6, E=A, F= 5, new3(A,B,C,D).
new1(A,B,C,D) :- new2(A,B,C,D).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
