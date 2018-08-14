new23(A,B,C,D,E,F) :- G= 1, H>= 0, H=B, I= -1, new11(A,G,J), new18(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- G= 0, H=< -1, H=B, I= -1, new11(A,G,J), 
          new18(A,B,C,D,E,F).
new22(A,B,C,D,E,F) :- G= 1, H=< 2, H=C, I= 2, J=K+ 1, K=A, L= 1, new11(A,G,M), 
          new20(J,B,C,D,E,F).
new22(A,B,C,D,E,F) :- G= 0, H>= 3, H=C, I= 2, J=K+ 1, K=A, L= 1, new11(A,G,M), 
          new20(J,B,C,D,E,F).
new21(A,B,C,D,E,F) :- G+ 1=< 10, G=A, H= 10, new22(A,B,C,D,E,F).
new21(A,B,C,D,E,F) :- G>= 10, G=A, H= 10, new23(A,B,C,D,E,F).
new20(A,B,C,D,E,F) :- new21(A,B,C,D,E,F).
new19(A,B,C,D,E,F) :- new20(A,B,C,D,E,F).
new18(A,B,C,A,B,C).
new17(A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0, I= 0, new19(A,I,C,D,E,F).
new17(A,B,C,D,E,F) :- G>= 0, G=B, H= 0, new19(A,B,C,D,E,F).
new16(A,B,C,D,E,F) :- G=< 2, G=C, H= 2, new17(A,B,C,D,E,F).
new16(A,B,C,D,E,F) :- G>= 3, G=C, H= 2, new18(A,B,C,D,E,F).
new14(A,B) :- new16(A,C,D,B,E,F).
safe :- init(A), new14(A,B).
new13(A,B,B).
new11(A,B,B) :- C>= 1, C=B, D= 0.
new11(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new10(A,B,C) :- D= 0, D=B, E= 0, new13(A,B,C).
new9(A,B,C,A,B,C) :- D= 1, E>= 0, E=B, F= -1, new10(A,D,G).
new9(A,B,C,A,B,C) :- D= 0, E=< -1, E=B, F= -1, new10(A,D,G).
new8(A,B,C,A,B,C) :- D= 1, E=< 2, E=C, F= 2, new10(A,D,G).
new8(A,B,C,D,E,F) :- G= 1, H=< 2, H=C, I= 2, J=K+ 1, K=A, L= 1, new11(A,G,M), 
          new6(J,B,C,D,E,F).
new8(A,B,C,A,B,C) :- D= 0, E>= 3, E=C, F= 2, new10(A,D,G).
new8(A,B,C,D,E,F) :- G= 0, H>= 3, H=C, I= 2, J=K+ 1, K=A, L= 1, new11(A,G,M), 
          new6(J,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G+ 1=< 10, G=A, H= 10, new8(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G>= 10, G=A, H= 10, new9(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- new7(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0, I= 0, new5(A,I,C,D,E,F).
new3(A,B,C,D,E,F) :- G>= 0, G=B, H= 0, new5(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G=< 2, G=C, H= 2, new3(A,B,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
