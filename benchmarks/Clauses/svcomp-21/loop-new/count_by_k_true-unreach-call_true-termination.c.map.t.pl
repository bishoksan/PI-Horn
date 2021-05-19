new27(A,B,A,B) :- C>= 11, C=A, D= 10.
new22(A,B,B).
new20(A,B,C) :- D>= 1, D=B, E= 0, new22(A,B,C).
new20(A,B,C) :- D+ 1=< 0, D=B, E= 0, new22(A,B,C).
new19(A,B,A,B) :- C= 1, D=E, D=B, E= 1000000*F, G= 1000000, F=A, new20(A,C,H).
new19(A,B,A,B) :- C= 0, D>=E+ 1, D=B, E= 1000000*F, G= 1000000, F=A, 
          new20(A,C,H).
new19(A,B,A,B) :- C= 0, D+ 1=<E, D=B, E= 1000000*F, G= 1000000, F=A, 
          new20(A,C,H).
new18(A,B,C,D) :- E+ 1=<F, E=B, F= 1000000*G, H= 1000000, G=A, I=J+K, J=B, K=A, 
          new17(A,I,C,D).
new18(A,B,C,D) :- E>=F, E=B, F= 1000000*G, H= 1000000, G=A, new19(A,B,C,D).
new17(A,B,C,D) :- new18(A,B,C,D).
new16(A,B,C,D) :- E=< 10, E=A, F= 10, G= 0, new17(A,G,C,D).
new15(A,B,C,D) :-  0=<E, F= 0, E=A, new16(A,B,C,D).
new14(A,B,A,B) :-  0>=C+ 1, D= 0, C=A.
new13(A,B,C,D) :-  0=<E, F= 0, E=A, new27(A,B,C,D).
new11(A,B) :- new13(A,C,B,D).
new11(A,B) :- new14(A,C,B,D).
new11(A,B) :- new15(A,C,B,D).
safe :- init(A), new11(A,B).
new10(A).
new8(A,B,B) :- new10(A).
new7(A,B,C) :- D= 0, D=B, E= 0, new8(A,B,C).
new6(A,B,A,B) :- C= 1, D=E, D=B, E= 1000000*F, G= 1000000, F=A, new7(A,C,H).
new6(A,B,A,B) :- C= 0, D>=E+ 1, D=B, E= 1000000*F, G= 1000000, F=A, new7(A,C,H).
new6(A,B,A,B) :- C= 0, D+ 1=<E, D=B, E= 1000000*F, G= 1000000, F=A, new7(A,C,H).
new5(A,B,C,D) :- E+ 1=<F, E=B, F= 1000000*G, H= 1000000, G=A, I=J+K, J=B, K=A, 
          new4(A,I,C,D).
new5(A,B,C,D) :- E>=F, E=B, F= 1000000*G, H= 1000000, G=A, new6(A,B,C,D).
new4(A,B,C,D) :- new5(A,B,C,D).
new3(A,B,C,D) :- E=< 10, E=A, F= 10, G= 0, new4(A,G,C,D).
new2(A,B,C,D) :-  0=<E, F= 0, E=A, new3(A,B,C,D).
new1(A,B) :- new2(A,C,B,D).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
