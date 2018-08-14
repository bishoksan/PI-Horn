new20(A,B,C,D) :- E=F, E=A, F=B, new17(A,B,C,D).
new19(A,B,A,B) :- C= 20, C=B, D= 20.
new18(A,B,C,D) :- E= 20, E=A, F= 20, new19(A,B,C,D).
new18(A,B,C,D) :- E>= 21, E=A, F= 20, G=H+ 1, H=A, I= 1, J=K+ 1, K=B, L= 1, 
          new20(G,J,C,D).
new18(A,B,C,D) :- E+ 1=< 20, E=A, F= 20, G=H+ 1, H=A, I= 1, J=K+ 1, K=B, L= 1, 
          new20(G,J,C,D).
new17(A,B,C,D) :- new18(A,B,C,D).
new10(A,B,C,D) :- new17(A,B,C,D).
new8(A,B,C,D) :- new10(A,B,C,D).
safe :- init(A,B), new8(A,B,C,D).
new7(A,B,A,B).
new6(A,B,C,D) :- E>=F+ 1, E=A, F=B, new7(A,B,C,D).
new6(A,B,C,D) :- E+ 1=<F, E=A, F=B, new7(A,B,C,D).
new6(A,B,C,D) :- E=F, E=A, F=B, new3(A,B,C,D).
new5(A,B,C,D) :- E>= 21, E=B, F= 20, new7(A,B,C,D).
new5(A,B,C,D) :- E+ 1=< 20, E=B, F= 20, new7(A,B,C,D).
new4(A,B,C,D) :- E= 20, E=A, F= 20, new5(A,B,C,D).
new4(A,B,C,D) :- E>= 21, E=A, F= 20, G=H+ 1, H=A, I= 1, J=K+ 1, K=B, L= 1, 
          new6(G,J,C,D).
new4(A,B,C,D) :- E+ 1=< 20, E=A, F= 20, G=H+ 1, H=A, I= 1, J=K+ 1, K=B, L= 1, 
          new6(G,J,C,D).
new3(A,B,C,D) :- new4(A,B,C,D).
new2(A,B,C,D) :- new3(A,B,C,D).
new1(A,B,C,D) :- new2(A,B,C,D).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
