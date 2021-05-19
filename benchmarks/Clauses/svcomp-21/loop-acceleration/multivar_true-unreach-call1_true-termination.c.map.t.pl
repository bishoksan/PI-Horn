new18(A,B,C,C).
new16(A,B,C,D) :- E>= 1, E=C, F= 0, new18(A,B,C,D).
new16(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new18(A,B,C,D).
new15(A,B,A,B) :- C= 1, D=E, D=A, A>= 0, E=B, B>= 0, new16(A,B,C,F).
new15(A,B,A,B) :- C= 0, D>=E+ 1, D=A, A>= 0, E=B, B>= 0, new16(A,B,C,F).
new15(A,B,A,B) :- C= 0, D+ 1=<E, D=A, A>= 0, E=B, B>= 0, new16(A,B,C,F).
new14(A,B,C,D) :- E+ 1=< 1024, E=A, A>= 0, F= 1024, G=H+ 1, H=A, I= 1, J=K+ 1, 
          K=B, B>= 0, L= 1, new13(G,J,C,D).
new14(A,B,C,D) :- E>= 1024, E=A, A>= 0, F= 1024, new15(A,B,C,D).
new13(A,B,C,D) :- new14(A,B,C,D).
new12(A,B,C,D) :- new13(A,B,C,D).
new10(A,B,C,D) :- new12(A,B,C,D).
safe :- init(A,B), new10(A,B,C,D).
new9(A,B).
new7(A,B,C,C) :- new9(A,B).
new6(A,B,C,D) :- E= 0, E=C, F= 0, new7(A,B,C,D).
new5(A,B,A,B) :- C= 1, D=E, D=A, A>= 0, E=B, B>= 0, new6(A,B,C,F).
new5(A,B,A,B) :- C= 0, D>=E+ 1, D=A, A>= 0, E=B, B>= 0, new6(A,B,C,F).
new5(A,B,A,B) :- C= 0, D+ 1=<E, D=A, A>= 0, E=B, B>= 0, new6(A,B,C,F).
new4(A,B,C,D) :- E+ 1=< 1024, E=A, A>= 0, F= 1024, G=H+ 1, H=A, I= 1, J=K+ 1, 
          K=B, B>= 0, L= 1, new3(G,J,C,D).
new4(A,B,C,D) :- E>= 1024, E=A, A>= 0, F= 1024, new5(A,B,C,D).
new3(A,B,C,D) :- new4(A,B,C,D).
new2(A,B,C,D) :- new3(A,B,C,D).
new1(A,B,C,D) :- new2(A,B,C,D).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
