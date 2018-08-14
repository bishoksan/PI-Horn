new18(A,B,B) :- C>= 1, C=B, D= 0.
new18(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new17(A,B,C,D,A,B,C,D) :- E= 1, F= 17, F=D, G= 17, new18(A,E,H).
new17(A,B,C,D,A,B,C,D) :- E= 0, F>= 18, F=D, G= 17, new18(A,E,H).
new17(A,B,C,D,A,B,C,D) :- E= 0, F+ 1=< 17, F=D, G= 17, new18(A,E,H).
new16(A,B,C,D,E,F,G,H) :- I=J+ 3, J=C, K= 3, L=M, new5(A,I,N,O,P,M), 
          new17(O,B,C,L,E,F,G,H).
new15(A,B,C,D,E,F,G,H) :- I=J+ 2, J=B, K= 2, L=M, new5(A,I,N,O,P,M), 
          new16(O,B,L,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I=J+ 1, J=A, K= 1, L=M, new5(A,I,N,O,P,M), 
          new15(O,L,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- new14(A,B,C,D,E,F,G,H).
new11(A,B) :- new13(A,C,D,E,B,F,G,H).
safe :- init(A), new11(A,B).
new10(A,B,B).
new9(A,B,C) :- D= 0, D=B, E= 0, new10(A,B,C).
new8(A,B,C,D,A,B,C,D) :- E= 1, F= 17, F=D, G= 17, new9(A,E,H).
new8(A,B,C,D,A,B,C,D) :- E= 0, F>= 18, F=D, G= 17, new9(A,E,H).
new8(A,B,C,D,A,B,C,D) :- E= 0, F+ 1=< 17, F=D, G= 17, new9(A,E,H).
new7(A,B,C,D,E,F,G,H) :- I=J+ 3, J=C, K= 3, L=M, new5(A,I,N,O,P,M), 
          new8(O,B,C,L,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I=J+ 2, J=B, K= 2, L=M, new5(A,I,N,O,P,M), 
          new7(O,B,L,D,E,F,G,H).
new5(A,B,C,A,B,D) :- D=E+ 1, E=B, F= 1.
new3(A,B,C,D,E,F,G,H) :- I=J+ 1, J=A, K= 1, L=M, new5(A,I,N,O,P,M), 
          new6(O,L,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,B,C,D,E,F,G,H).
new1(A,B) :- new2(A,C,D,E,B,F,G,H).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
