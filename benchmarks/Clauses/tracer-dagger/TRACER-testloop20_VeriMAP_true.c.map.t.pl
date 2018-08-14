new29(A,B,C,D,E,F) :- G= 3, new27(A,B,G,D,E,F).
new28(A,B,C,D,E,F) :- G= 4, new27(A,B,G,D,E,F).
new27(A,B,C,D,E,F) :- new20(A,B,C,D,E,F).
new26(A,B,C,D,E,F) :- G= 1, H>= 4, H=B, I= 3, J= 5, new12(A,G,K), 
          new27(A,B,J,D,E,F).
new26(A,B,C,D,E,F) :- G= 1, H+ 1=< 3, H=B, I= 3, J= 5, new12(A,G,K), 
          new27(A,B,J,D,E,F).
new26(A,B,C,D,E,F) :- G= 0, H= 3, H=B, I= 3, J= 5, new12(A,G,K), 
          new27(A,B,J,D,E,F).
new25(A,B,C,D,E,F) :- G= 4, G=C, H= 4, new26(A,B,C,D,E,F).
new25(A,B,C,D,E,F) :- G>= 5, G=C, H= 4, new27(A,B,C,D,E,F).
new25(A,B,C,D,E,F) :- G+ 1=< 4, G=C, H= 4, new27(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- G= 1, G=B, H= 1, I= 2, new28(A,I,C,D,E,F).
new24(A,B,C,D,E,F) :- G>= 2, G=B, H= 1, new28(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- G+ 1=< 1, G=B, H= 1, new28(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- G= 3, G=C, H= 3, new24(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- G>= 4, G=C, H= 3, new25(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- G+ 1=< 3, G=C, H= 3, new25(A,B,C,D,E,F).
new22(A,B,C,D,E,F) :- G= 0, G=B, H= 0, I= 1, new29(A,I,C,D,E,F).
new22(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new29(A,B,C,D,E,F).
new22(A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0, new29(A,B,C,D,E,F).
new21(A,B,C,D,E,F) :- G= 2, G=C, H= 2, new22(A,B,C,D,E,F).
new21(A,B,C,D,E,F) :- G>= 3, G=C, H= 2, new23(A,B,C,D,E,F).
new21(A,B,C,D,E,F) :- G+ 1=< 2, G=C, H= 2, new23(A,B,C,D,E,F).
new20(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, new21(A,B,C,D,E,F).
new20(A,B,C,D,E,F) :- G+ 1=< 0, G=A, H= 0, new21(A,B,C,D,E,F).
new20(A,B,C,A,B,C) :- D= 0, D=A, E= 0.
new19(A,B,C,D,E,F) :- G= 0, H= 2, new20(A,G,H,D,E,F).
new17(A,B) :- new19(A,C,D,B,E,F).
safe :- init(A), new17(A,B).
new16(A,B,C,D,E,F) :- G= 3, new10(A,B,G,D,E,F).
new15(A,B,C,D,E,F) :- G= 4, new10(A,B,G,D,E,F).
new14(A,B,B).
new12(A,B,B) :- C>= 1, C=B, D= 0.
new12(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new11(A,B,C) :- D= 0, D=B, E= 0, new14(A,B,C).
new10(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new9(A,B,C,A,B,C) :- D= 1, E>= 4, E=B, F= 3, new11(A,D,G).
new9(A,B,C,D,E,F) :- G= 1, H>= 4, H=B, I= 3, J= 5, new12(A,G,K), 
          new10(A,B,J,D,E,F).
new9(A,B,C,A,B,C) :- D= 1, E+ 1=< 3, E=B, F= 3, new11(A,D,G).
new9(A,B,C,D,E,F) :- G= 1, H+ 1=< 3, H=B, I= 3, J= 5, new12(A,G,K), 
          new10(A,B,J,D,E,F).
new9(A,B,C,A,B,C) :- D= 0, E= 3, E=B, F= 3, new11(A,D,G).
new9(A,B,C,D,E,F) :- G= 0, H= 3, H=B, I= 3, J= 5, new12(A,G,K), 
          new10(A,B,J,D,E,F).
new8(A,B,C,D,E,F) :- G= 4, G=C, H= 4, new9(A,B,C,D,E,F).
new8(A,B,C,D,E,F) :- G>= 5, G=C, H= 4, new10(A,B,C,D,E,F).
new8(A,B,C,D,E,F) :- G+ 1=< 4, G=C, H= 4, new10(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G= 1, G=B, H= 1, I= 2, new15(A,I,C,D,E,F).
new7(A,B,C,D,E,F) :- G>= 2, G=B, H= 1, new15(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G+ 1=< 1, G=B, H= 1, new15(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G= 3, G=C, H= 3, new7(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G>= 4, G=C, H= 3, new8(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G+ 1=< 3, G=C, H= 3, new8(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- G= 0, G=B, H= 0, I= 1, new16(A,I,C,D,E,F).
new5(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new16(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0, new16(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G= 2, G=C, H= 2, new5(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G>= 3, G=C, H= 2, new6(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- G+ 1=< 2, G=C, H= 2, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G>= 1, G=A, H= 0, new4(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G+ 1=< 0, G=A, H= 0, new4(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G= 0, H= 2, new3(A,G,H,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
