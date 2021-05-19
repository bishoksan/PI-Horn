new18(A,B,C,C).
new16(A,B,C,D) :- E>= 1, E=C, F= 0, new18(A,B,C,D).
new16(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new18(A,B,C,D).
new15(A,B,C,A,B,C) :- D= 1, E>=F+ 1, E=C, C>= 0, F=A, A>= 0, new16(A,B,D,G).
new15(A,B,C,A,B,C) :- D= 1, E+ 1=<F, E=C, C>= 0, F=A, A>= 0, new16(A,B,D,G).
new15(A,B,C,A,B,C) :- D= 0, E=F, E=C, C>= 0, F=A, A>= 0, new16(A,B,D,G).
new14(A,B,C,D,E,F) :- G>= 1, G=B, B>= 0, H= 0, I=J- 1, J=B, K= 1, L=M+ 1, M=C, 
          C>= 0, N= 1, new13(A,I,L,D,E,F).
new14(A,B,C,D,E,F) :- G=< 0, G=B, B>= 0, H= 0, new15(A,B,C,D,E,F).
new13(A,B,C,D,E,F) :- new14(A,B,C,D,E,F).
new12(A,B,C,D,E,F) :- G= 0, new13(A,B,G,D,E,F).
new10(A,B,C,D) :- new12(A,B,E,C,D,F).
safe :- init(A,B), new10(A,B,C,D).
new9(A,B).
new7(A,B,C,C) :- new9(A,B).
new6(A,B,C,D) :- E= 0, E=C, F= 0, new7(A,B,C,D).
new5(A,B,C,A,B,C) :- D= 1, E>=F+ 1, E=C, C>= 0, F=A, A>= 0, new6(A,B,D,G).
new5(A,B,C,A,B,C) :- D= 1, E+ 1=<F, E=C, C>= 0, F=A, A>= 0, new6(A,B,D,G).
new5(A,B,C,A,B,C) :- D= 0, E=F, E=C, C>= 0, F=A, A>= 0, new6(A,B,D,G).
new4(A,B,C,D,E,F) :- G>= 1, G=B, B>= 0, H= 0, I=J- 1, J=B, K= 1, L=M+ 1, M=C, 
          C>= 0, N= 1, new3(A,I,L,D,E,F).
new4(A,B,C,D,E,F) :- G=< 0, G=B, B>= 0, H= 0, new5(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G= 0, new3(A,B,G,D,E,F).
new1(A,B,C,D) :- new2(A,B,E,C,D,F).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
