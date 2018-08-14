new26(A,B,C,A,B,C) :- D>=E, D=B, E=A.
new25(A,B,C,D,E,F) :- G>= 1, G=C, H= 0, I=J- 1, J=C, K= 1, L=M- 1, M=B, N= 1, 
          new24(A,L,I,D,E,F).
new25(A,B,C,D,E,F) :- G=< 0, G=C, H= 0, new26(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- new25(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- G+ 1=<H, G=C, H=A, I=J+ 1, J=C, K= 1, L=M+ 2, M=B, N= 2, 
          new22(A,L,I,D,E,F).
new23(A,B,C,D,E,F) :- G>=H, G=C, H=A, new24(A,B,C,D,E,F).
new22(A,B,C,D,E,F) :- new23(A,B,C,D,E,F).
new21(A,B,C,D,E,F) :- G= 1, H>= 0, H=A, I= 0, new4(A,B,G,J), new22(A,B,C,D,E,F).
new21(A,B,C,D,E,F) :- G= 0, H+ 1=< 0, H=A, I= 0, new4(A,B,G,J), 
          new22(A,B,C,D,E,F).
new13(A,B,C,D,E,F) :- G= 0, new21(A,B,G,D,E,F).
new11(A,B,C,D) :- new13(A,B,E,C,D,F).
safe :- init(A,B), new11(A,B,C,D).
new9(A,B,C,A,B,C) :- D+ 1=<E, D=B, E=A.
new8(A,B,C,D,E,F) :- G>= 1, G=C, H= 0, I=J- 1, J=C, K= 1, L=M- 1, M=B, N= 1, 
          new7(A,L,I,D,E,F).
new8(A,B,C,D,E,F) :- G=< 0, G=C, H= 0, new9(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- new8(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G+ 1=<H, G=C, H=A, I=J+ 1, J=C, K= 1, L=M+ 2, M=B, N= 2, 
          new5(A,L,I,D,E,F).
new6(A,B,C,D,E,F) :- G>=H, G=C, H=A, new7(A,B,C,D,E,F).
new5(A,B,C,D,E,F) :- new6(A,B,C,D,E,F).
new4(A,B,C,C) :- D>= 1, D=C, E= 0.
new4(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new3(A,B,C,D,E,F) :- G= 1, H>= 0, H=A, I= 0, new4(A,B,G,J), new5(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G= 0, H+ 1=< 0, H=A, I= 0, new4(A,B,G,J), 
          new5(A,B,C,D,E,F).
new2(A,B,C,D,E,F) :- G= 0, new3(A,B,G,D,E,F).
new1(A,B,C,D) :- new2(A,B,E,C,D,F).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
