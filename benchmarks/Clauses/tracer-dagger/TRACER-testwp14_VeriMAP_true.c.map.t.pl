new16(A,B,B) :- C>= 1, C=B, D= 0.
new16(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new15(A,B,C,A,B,C) :- D= 1, E>= 5, E=B, F= 4, new16(A,D,G).
new15(A,B,C,A,B,C) :- D= 0, E=< 4, E=B, F= 4, new16(A,D,G).
new14(A,B,C,D,E,F) :- G= 1, H>= 1, H=A, I= 0, J=A, new5(A,G,K), 
          new15(A,J,C,D,E,F).
new14(A,B,C,D,E,F) :- G= 0, H=< 0, H=A, I= 0, J=A, new5(A,G,K), 
          new15(A,J,C,D,E,F).
new13(A,B,C,D,E,F) :- new14(A,B,C,D,E,F).
new12(A,B,C,D,E,F) :- G>= 5, G=A, H= 4, I= 4, new13(A,B,I,D,E,F).
new12(A,B,C,D,E,F) :- G=< 4, G=A, H= 4, I= 4, new13(I,B,C,D,E,F).
new10(A,B) :- new12(A,C,D,B,E,F).
safe :- init(A), new10(A,B).
new8(A,B,B).
new7(A,B,C) :- D= 0, D=B, E= 0, new8(A,B,C).
new6(A,B,C,A,B,C) :- D= 1, E>= 5, E=B, F= 4, new7(A,D,G).
new6(A,B,C,A,B,C) :- D= 0, E=< 4, E=B, F= 4, new7(A,D,G).
new5(A,B,B) :- C>= 1, C=B, D= 0.
new5(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new4(A,B,C,D,E,F) :- G= 1, H>= 1, H=A, I= 0, J=A, new5(A,G,K), 
          new6(A,J,C,D,E,F).
new4(A,B,C,D,E,F) :- G= 0, H=< 0, H=A, I= 0, J=A, new5(A,G,K), 
          new6(A,J,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G>= 5, G=A, H= 4, I= 4, new3(A,B,I,D,E,F).
new2(A,B,C,D,E,F) :- G=< 4, G=A, H= 4, I= 4, new3(I,B,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
