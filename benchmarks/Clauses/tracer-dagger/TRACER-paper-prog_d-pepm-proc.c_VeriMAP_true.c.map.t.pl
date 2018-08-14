new15(A,B,A,B) :- C>= 10000, C=D+E, D=A, E=B, F= 10000.
new14(A,B,C,D) :- E+ 1=< 10000, E=B, F= 10000, G=H+ 1, H=A, I= 1, J=K+ 1, K=B, 
          L= 1, new13(G,J,C,D).
new14(A,B,C,D) :- E>= 10000, E=B, F= 10000, new15(A,B,C,D).
new13(A,B,C,D) :- new14(A,B,C,D).
new8(A,B,C,D) :- E= 0, new13(A,E,C,D).
new6(A,B) :- new8(A,C,B,D).
safe :- init(A), new6(A,B).
new5(A,B,A,B) :- C+ 1=< 10000, C=D+E, D=A, E=B, F= 10000.
new4(A,B,C,D) :- E+ 1=< 10000, E=B, F= 10000, G=H+ 1, H=A, I= 1, J=K+ 1, K=B, 
          L= 1, new3(G,J,C,D).
new4(A,B,C,D) :- E>= 10000, E=B, F= 10000, new5(A,B,C,D).
new3(A,B,C,D) :- new4(A,B,C,D).
new2(A,B,C,D) :- E= 0, new3(A,E,C,D).
new1(A,B) :- new2(A,C,B,D).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
