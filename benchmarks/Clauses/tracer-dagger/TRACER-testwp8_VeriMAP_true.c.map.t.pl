new12(A,B,B) :- C>= 1, C=B, D= 0.
new12(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new11(A,B,C,D,A,B,C,D) :- E= 1, F=< 50, F=G+H, G=B, H=C, I= 50, new12(A,E,J).
new11(A,B,C,D,A,B,C,D) :- E= 0, F>= 51, F=G+H, G=B, H=C, I= 50, new12(A,E,J).
new10(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, K=L+ 1, L=B, M= 1, 
          new11(A,K,C,D,E,F,G,H).
new10(A,B,C,D,E,F,G,H) :- I=< 0, I=D, J= 0, K=L+ 4, L=B, M= 4, 
          new11(A,K,C,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K= 2, new10(A,B,K,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I=< 0, I=C, J= 0, K= 5, new10(A,B,K,D,E,F,G,H).
new7(A,B) :- new9(A,C,D,E,B,F,G,H).
safe :- init(A), new7(A,B).
new6(A,B,B).
new5(A,B,C) :- D= 0, D=B, E= 0, new6(A,B,C).
new4(A,B,C,D,A,B,C,D) :- E= 1, F=< 50, F=G+H, G=B, H=C, I= 50, new5(A,E,J).
new4(A,B,C,D,A,B,C,D) :- E= 0, F>= 51, F=G+H, G=B, H=C, I= 50, new5(A,E,J).
new3(A,B,C,D,E,F,G,H) :- I>= 1, I=D, J= 0, K=L+ 1, L=B, M= 1, 
          new4(A,K,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I=< 0, I=D, J= 0, K=L+ 4, L=B, M= 4, 
          new4(A,K,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K= 2, new3(A,B,K,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I=< 0, I=C, J= 0, K= 5, new3(A,B,K,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
