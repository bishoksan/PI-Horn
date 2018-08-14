new21(A,B,C,D,E,F) :- G=C, H=I+ 1, I=B, J= 1, new7(A,G,K,L,M,N), 
          new19(L,H,C,D,E,F).
new20(A,B,C,D,E,F) :- G+ 1=< 10, G=B, H= 10, new21(A,B,C,D,E,F).
new20(A,B,C,A,B,C) :- D>= 10, D=B, E= 10.
new19(A,B,C,D,E,F) :- new20(A,B,C,D,E,F).
new18(A,B,C,D,E,F) :- G= 0, new19(A,G,C,D,E,F).
new16(A,B) :- new18(A,C,D,B,E,F).
safe :- init(A), new16(A,B).
new15(A,B,B).
new14(A,B,C) :- D= 0, D=B, E= 0, new15(A,B,C).
new12(A,B,C,A,B,C) :- D= 1, E>= 0, E=A, F= 0, new14(A,D,G).
new12(A,B,C,A,B,C) :- D= 0, E+ 1=< 0, E=A, F= 0, new14(A,D,G).
new10(A,B,B) :- C>= 1, C=B, D= 0.
new10(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new9(A,B,C,A,B,C).
new8(A,B,C,D,E,F) :- G= 1, H>= 0, H=A, I= 0, new10(A,G,J), new9(A,B,C,D,E,F).
new8(A,B,C,D,E,F) :- G= 0, H+ 1=< 0, H=A, I= 0, new10(A,G,J), new9(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G>= 0, G=B, H= 0, I= 1, new8(A,B,I,D,E,F).
new7(A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0, I= 2, J= 1, new9(J,B,I,D,E,F).
new6(A,B,C,D,E,F) :- G>= 0, G=B, H= 0, I= 1, new12(A,B,I,D,E,F).
new5(A,B,C,D,B,C) :- E=C, new6(A,E,F,D,G,H).
new5(A,B,C,D,E,F) :- G=C, H=I+ 1, I=B, J= 1, new7(A,G,K,L,M,N), 
          new3(L,H,C,D,E,F).
new4(A,B,C,D,E,F) :- G+ 1=< 10, G=B, H= 10, new5(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G= 0, new3(A,G,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
