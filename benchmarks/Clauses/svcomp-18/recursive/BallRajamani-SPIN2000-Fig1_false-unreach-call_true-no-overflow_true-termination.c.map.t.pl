new22(A,B,A,B).
new20(A,B,C,D) :- E= 0, E=A, F= 0, new22(A,B,C,D).
new19(A,B,C,D) :- E=A, F=B, new5(A,B,E,F,G,H,I,J), new20(G,H,C,D).
new18(A,B,C,D) :- E=A, F=B, new5(A,B,E,F,G,H,I,J), new19(G,H,C,D).
new17(A,B,C,D) :- new18(A,B,C,D).
new15(A,B,C,D) :- new17(A,B,C,D).
safe :- init(A,B), new15(A,B,C,D).
new12(A,B,C,D,A,B,C,D).
new11(A,B,C,D,E,F,G,H) :- I=D, J=C, new5(A,B,I,J,K,L,M,N), 
          new12(K,L,C,D,E,F,G,H).
new10(A,B).
new8(A,B,A,B) :- new10(A,B).
new7(A,B,C,D) :- E>= 1, E=A, F= 0, new8(A,B,C,D).
new7(A,B,C,D) :- E+ 1=< 0, E=A, F= 0, new8(A,B,C,D).
new6(A,B,C,D) :- E=A, F=B, new5(A,B,E,F,G,H,I,J), new7(G,H,C,D).
new5(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, new11(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, new11(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I= 0, I=C, J= 0, K=D, new12(K,B,C,D,E,F,G,H).
new3(A,B,C,D) :- E=A, F=B, new5(A,B,E,F,G,H,I,J), new6(G,H,C,D).
new2(A,B,C,D) :- new3(A,B,C,D).
new1(A,B,C,D) :- new2(A,B,C,D).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
