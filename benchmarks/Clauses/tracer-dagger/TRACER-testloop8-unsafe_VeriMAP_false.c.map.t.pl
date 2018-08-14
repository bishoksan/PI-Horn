new20(A,B,C,C) :- D>= 1, D=C, E= 0.
new20(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new19(A,B,C,A,B,C) :- D= 1, E>= 1, E=B, F= 0, new20(A,B,D,G).
new19(A,B,C,A,B,C) :- D= 1, E+ 1=< 0, E=B, F= 0, new20(A,B,D,G).
new19(A,B,C,A,B,C) :- D= 0, E= 0, E=B, F= 0, new20(A,B,D,G).
new18(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, I=J- 1, J=A, K= 1, L=M- 1, M=B, N= 1, 
          new17(I,L,C,D,E,F).
new18(A,B,C,D,E,F) :- G=< 0, G=A, H= 0, new19(A,B,C,D,E,F).
new17(A,B,C,D,E,F) :- new18(A,B,C,D,E,F).
new16(A,B,C,D,E,F) :- G>= 1, G=C, H= 0, I=J+ 1, J=A, K= 1, L=M+ 1, M=B, N= 1, 
          new14(I,L,C,D,E,F).
new16(A,B,C,D,E,F) :- G+ 1=< 0, G=C, H= 0, I=J+ 1, J=A, K= 1, L=M+ 1, M=B, 
          N= 1, new14(I,L,C,D,E,F).
new16(A,B,C,D,E,F) :- G= 0, G=C, H= 0, new17(A,B,C,D,E,F).
new15(A,B,C,D,E,F) :- new16(A,B,G,D,E,F).
new14(A,B,C,D,E,F) :- new15(A,B,C,D,E,F).
new13(A,B,C,D,E,F) :- new14(A,B,C,D,E,F).
new11(A,B,C,D) :- new13(A,B,E,C,D,F).
safe :- init(A,B), new11(A,B,C,D).
new10(A,B,C,C).
new9(A,B,C,D) :- E= 0, E=C, F= 0, new10(A,B,C,D).
new8(A,B,C,A,B,C) :- D= 1, E>= 1, E=B, F= 0, new9(A,B,D,G).
new8(A,B,C,A,B,C) :- D= 1, E+ 1=< 0, E=B, F= 0, new9(A,B,D,G).
new8(A,B,C,A,B,C) :- D= 0, E= 0, E=B, F= 0, new9(A,B,D,G).
new7(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, I=J- 1, J=A, K= 1, L=M- 1, M=B, N= 1, 
          new6(I,L,C,D,E,F).
new7(A,B,C,D,E,F) :- G=< 0, G=A, H= 0, new8(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- new7(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- G>= 1, G=C, H= 0, I=J+ 1, J=A, K= 1, L=M+ 1, M=B, N= 1, 
          new3(I,L,C,D,E,F).
new5(A,B,C,D,E,F) :- G+ 1=< 0, G=C, H= 0, I=J+ 1, J=A, K= 1, L=M+ 1, M=B, N= 1, 
          new3(I,L,C,D,E,F).
new5(A,B,C,D,E,F) :- G= 0, G=C, H= 0, new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- new5(A,B,G,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B,C,D) :- new2(A,B,E,C,D,F).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
