new31(A,B,C,D,E,F) :- G= 3, new21(A,G,C,D,E,F).
new30(A,B,C,D,E,F) :- G= 4, new21(A,G,C,D,E,F).
new29(A,B,C,D,E,F) :- G= 1, H>= 4, H=A, I= 3, J= 5, new13(A,G,K), 
          new21(A,J,C,D,E,F).
new29(A,B,C,D,E,F) :- G= 1, H+ 1=< 3, H=A, I= 3, J= 5, new13(A,G,K), 
          new21(A,J,C,D,E,F).
new29(A,B,C,D,E,F) :- G= 0, H= 3, H=A, I= 3, J= 5, new13(A,G,K), 
          new21(A,J,C,D,E,F).
new28(A,B,C,D,E,F) :- G= 4, G=B, H= 4, new29(A,B,C,D,E,F).
new28(A,B,C,D,E,F) :- G>= 5, G=B, H= 4, new21(A,B,C,D,E,F).
new28(A,B,C,D,E,F) :- G+ 1=< 4, G=B, H= 4, new21(A,B,C,D,E,F).
new27(A,B,C,D,E,F) :- G= 1, G=A, H= 1, I= 2, new30(I,B,C,D,E,F).
new27(A,B,C,D,E,F) :- G>= 2, G=A, H= 1, new30(A,B,C,D,E,F).
new27(A,B,C,D,E,F) :- G+ 1=< 1, G=A, H= 1, new30(A,B,C,D,E,F).
new26(A,B,C,D,E,F) :- G= 3, G=B, H= 3, new27(A,B,C,D,E,F).
new26(A,B,C,D,E,F) :- G>= 4, G=B, H= 3, new28(A,B,C,D,E,F).
new26(A,B,C,D,E,F) :- G+ 1=< 3, G=B, H= 3, new28(A,B,C,D,E,F).
new25(A,B,C,D,E,F) :- G= 0, G=A, H= 0, I= 1, new31(I,B,C,D,E,F).
new25(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, new31(A,B,C,D,E,F).
new25(A,B,C,D,E,F) :- G+ 1=< 0, G=A, H= 0, new31(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- G= 2, G=B, H= 2, new25(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- G>= 3, G=B, H= 2, new26(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- G+ 1=< 2, G=B, H= 2, new26(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- G>= 1, G=C, H= 0, new24(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- G+ 1=< 0, G=C, H= 0, new24(A,B,C,D,E,F).
new23(A,B,C,A,B,C) :- D= 0, D=C, E= 0.
new22(A,B,C,D,E,F) :- new23(A,B,G,D,E,F).
new21(A,B,C,D,E,F) :- new22(A,B,C,D,E,F).
new20(A,B,C,D,E,F) :- G= 2, new21(A,G,C,D,E,F).
new18(A,B) :- new20(A,C,D,B,E,F).
safe :- init(A), new18(A,B).
new17(A,B,C,D,E,F) :- G= 3, new3(A,G,C,D,E,F).
new16(A,B,C,D,E,F) :- G= 4, new3(A,G,C,D,E,F).
new15(A,B,B).
new13(A,B,B) :- C>= 1, C=B, D= 0.
new13(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new12(A,B,C) :- D= 0, D=B, E= 0, new15(A,B,C).
new11(A,B,C,A,B,C) :- D= 1, E>= 4, E=A, F= 3, new12(A,D,G).
new11(A,B,C,D,E,F) :- G= 1, H>= 4, H=A, I= 3, J= 5, new13(A,G,K), 
          new3(A,J,C,D,E,F).
new11(A,B,C,A,B,C) :- D= 1, E+ 1=< 3, E=A, F= 3, new12(A,D,G).
new11(A,B,C,D,E,F) :- G= 1, H+ 1=< 3, H=A, I= 3, J= 5, new13(A,G,K), 
          new3(A,J,C,D,E,F).
new11(A,B,C,A,B,C) :- D= 0, E= 3, E=A, F= 3, new12(A,D,G).
new11(A,B,C,D,E,F) :- G= 0, H= 3, H=A, I= 3, J= 5, new13(A,G,K), 
          new3(A,J,C,D,E,F).
new10(A,B,C,D,E,F) :- G= 4, G=B, H= 4, new11(A,B,C,D,E,F).
new10(A,B,C,D,E,F) :- G>= 5, G=B, H= 4, new3(A,B,C,D,E,F).
new10(A,B,C,D,E,F) :- G+ 1=< 4, G=B, H= 4, new3(A,B,C,D,E,F).
new9(A,B,C,D,E,F) :- G= 1, G=A, H= 1, I= 2, new16(I,B,C,D,E,F).
new9(A,B,C,D,E,F) :- G>= 2, G=A, H= 1, new16(A,B,C,D,E,F).
new9(A,B,C,D,E,F) :- G+ 1=< 1, G=A, H= 1, new16(A,B,C,D,E,F).
new8(A,B,C,D,E,F) :- G= 3, G=B, H= 3, new9(A,B,C,D,E,F).
new8(A,B,C,D,E,F) :- G>= 4, G=B, H= 3, new10(A,B,C,D,E,F).
new8(A,B,C,D,E,F) :- G+ 1=< 3, G=B, H= 3, new10(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G= 0, G=A, H= 0, I= 1, new17(I,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, new17(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G+ 1=< 0, G=A, H= 0, new17(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G= 2, G=B, H= 2, new7(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G>= 3, G=B, H= 2, new8(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G+ 1=< 2, G=B, H= 2, new8(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- G>= 1, G=C, H= 0, new6(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- G+ 1=< 0, G=C, H= 0, new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- new5(A,B,G,D,E,F).
new3(A,B,C,D,E,F) :- new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G= 2, new3(A,G,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
