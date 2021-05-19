new26(A,B,C,A,B,C).
new24(A,B,C,D,E,F) :- G>= 6, G=B, H= 5, new26(A,B,C,D,E,F).
new24(A,B,C,D,E,F) :- G+ 1=< 5, G=B, H= 5, new26(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- G=A, H= 0, I= 0, new5(A,G,J,K,L,M), new24(K,I,H,D,E,F).
new23(A,B,C,D,E,F) :- G=A, H=I+ 1, I=J, K= 1, L=H, new7(A,G,M,N,O,J), 
          new24(N,L,H,D,E,F).
new22(A,B,C,D,E,F) :- new23(A,B,C,D,E,F).
new20(A,B) :- new22(A,C,D,B,E,F).
safe :- init(A), new20(A,B).
new15(A).
new13(A,B,C,A,B,C) :- new15(A).
new11(A,B,C,D,B,E) :- F=G- 1, G=B, H= 1, E= 0, new5(A,F,I,D,J,K).
new11(A,B,C,D,B,E) :- F=G- 1, G=B, H= 1, E=I+ 1, I=J, K= 1, new7(A,F,L,D,M,J).
new10(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new11(A,B,C,D,E,F).
new10(A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0, new11(A,B,C,D,E,F).
new9(A,B,C,A,B,C) :- D= 0, D=B, E= 0.
new8(A,B,C,D,B,E) :- F=G- 1, G=B, H= 1, E= 0, new9(A,F,I,D,J,K).
new8(A,B,C,D,B,E) :- F=G- 1, G=B, H= 1, E=I+ 1, I=J, K= 1, new10(A,F,L,D,M,J).
new7(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new8(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0, new8(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G= 5, G=B, H= 5, new13(A,B,C,D,E,F).
new5(A,B,C,A,B,C) :- D= 0, D=B, E= 0.
new3(A,B,C,D,E,F) :- G=A, H= 0, I= 0, new5(A,G,J,K,L,M), new6(K,I,H,D,E,F).
new3(A,B,C,D,E,F) :- G=A, H=I+ 1, I=J, K= 1, L=H, new7(A,G,M,N,O,J), 
          new6(N,L,H,D,E,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
