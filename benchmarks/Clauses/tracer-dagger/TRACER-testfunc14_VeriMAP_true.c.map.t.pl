new24(A,B,B) :- C>= 1, C=B, D= 0.
new24(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new23(A,B,A,B) :- C= 1, D>= 2, D=B, E= 1, new24(A,C,F).
new23(A,B,A,B) :- C= 1, D+ 1=< 1, D=B, E= 1, new24(A,C,F).
new23(A,B,A,B) :- C= 0, D= 1, D=B, E= 1, new24(A,C,F).
new22(A,B,C,D) :- new6(A,E,F,G,H,I,J,K,L,M), new23(I,B,C,D).
new21(A,B,C,D) :- new22(A,B,C,D).
new20(A,B,C,D) :- E>= 1, E=A, F= 0, G= 1, new21(A,G,C,D).
new20(A,B,C,D) :- E=< 0, E=A, F= 0, G= 3, new21(A,G,C,D).
new18(A,B) :- new20(A,C,B,D).
safe :- init(A), new18(A,B).
new13(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=C, L= 0, M=N+ 1, N=B, O= 1, 
          new12(A,M,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=C, L= 0, M=N+ 1, N=B, O= 1, 
          new12(A,M,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,A,B,C,D,E) :- F= 0, F=C, G= 0.
new12(A,B,C,D,E,F,G,H,I,J) :- new13(A,B,C,D,E,F,G,H,I,J).
new11(A,B,C,D,E,F,G,H,I,J) :- new12(A,B,C,D,E,F,G,H,I,J).
new10(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=D, L= 0, M= 4, 
          new11(A,B,C,D,M,F,G,H,I,J).
new10(A,B,C,D,E,F,G,H,I,J) :- K=< 0, K=D, L= 0, M= 5, 
          new11(A,B,C,D,M,F,G,H,I,J).
new9(A,B,B).
new8(A,B,C) :- D= 0, D=B, E= 0, new9(A,B,C).
new7(A,B,A,B) :- C= 1, D>= 2, D=B, E= 1, new8(A,C,F).
new7(A,B,A,B) :- C= 1, D+ 1=< 1, D=B, E= 1, new8(A,C,F).
new7(A,B,A,B) :- C= 0, D= 1, D=B, E= 1, new8(A,C,F).
new6(A,B,C,D,E,F,G,H,I,J) :- K= 0, new10(A,K,C,D,E,F,G,H,I,J).
new4(A,B,C,D) :- new6(A,E,F,G,H,I,J,K,L,M), new7(I,B,C,D).
new3(A,B,C,D) :- new4(A,B,C,D).
new2(A,B,C,D) :- E>= 1, E=A, F= 0, G= 1, new3(A,G,C,D).
new2(A,B,C,D) :- E=< 0, E=A, F= 0, G= 3, new3(A,G,C,D).
new1(A,B) :- new2(A,C,B,D).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
