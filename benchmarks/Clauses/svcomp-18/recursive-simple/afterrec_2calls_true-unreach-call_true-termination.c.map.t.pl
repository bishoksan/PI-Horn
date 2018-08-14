new24(A,B) :- C=A, new10(A,C,B,D).
new22(A,B) :- new24(A,B).
safe :- init(A), new22(A,B).
new21(A,B,A,B) :- new17(A).
new20(A,B,C,B) :- D=B, new3(A,D,C,E).
new20(A,B,C,D) :- E=B, new10(A,E,F,G), new21(F,B,C,D).
new17(A).
new10(A,B,A,B) :- C+ 1=< 3, C=B, D= 3.
new7(A,B,A,B) :- new17(A).
new6(A,B,A,B) :- C+ 1=< 3, C=B, D= 3.
new5(A,B,C,D) :- E>= 3, E=B, F= 3, G=H- 1, H=B, I= 1, new20(A,G,C,D).
new4(A,B,C,B) :- D=B, new5(A,D,C,E).
new4(A,B,C,D) :- E=B, new6(A,E,F,G), new7(F,B,C,D).
new3(A,B,C,D) :- E>= 3, E=B, F= 3, G=H- 1, H=B, I= 1, new4(A,G,C,D).
new2(A,B) :- C=A, new3(A,C,B,D).
new1(A,B) :- new2(A,B).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
