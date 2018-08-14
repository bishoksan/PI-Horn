new20(A,B,C,D,A,B,C,D) :- E=< 0, E=A, F= 0.
new15(A,B,C,D,E,F,G,H) :- I= 0, new20(A,B,C,I,E,F,G,H).
new13(A,B,C,D,E,F) :- new15(A,B,C,G,D,E,F,H).
safe :- init(A,B,C), new13(A,B,C,D,E,F).
new12(A,B,C).
new10(A,B,C,D,D) :- new12(A,B,C).
new9(A,B,C,D,D).
new7(A,B,C,D,E) :- F>= 1, F=D, G= 0, new9(A,B,C,D,E).
new7(A,B,C,D,E) :- F+ 1=< 0, F=D, G= 0, new9(A,B,C,D,E).
new6(A,B,C,D,E) :- F= 0, F=D, G= 0, new10(A,B,C,D,E).
new5(A,B,C,D,A,B,C,D) :- E= 1, F= 0, F=B, G= 0, new6(A,B,C,E,H).
new5(A,B,C,D,E,F,G,H) :- I= 1, J= 0, J=B, K= 0, L=M+ 1, M=D, D>= 0, N= 1, 
          new7(A,B,C,I,O), new4(A,B,C,L,E,F,G,H).
new5(A,B,C,D,A,B,C,D) :- E= 0, F>= 1, F=B, G= 0, new6(A,B,C,E,H).
new5(A,B,C,D,E,F,G,H) :- I= 0, J>= 1, J=B, K= 0, L=M+ 1, M=D, D>= 0, N= 1, 
          new7(A,B,C,I,O), new4(A,B,C,L,E,F,G,H).
new5(A,B,C,D,A,B,C,D) :- E= 0, F+ 1=< 0, F=B, G= 0, new6(A,B,C,E,H).
new5(A,B,C,D,E,F,G,H) :- I= 0, J+ 1=< 0, J=B, K= 0, L=M+ 1, M=D, D>= 0, N= 1, 
          new7(A,B,C,I,O), new4(A,B,C,L,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- new5(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I>= 1, I=A, J= 0, K= 0, new4(A,B,C,K,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I= 0, new3(A,B,C,I,E,F,G,H).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,D,E,F,H).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
