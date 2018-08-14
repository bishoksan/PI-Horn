new20(A,B,C,A,B,C).
new18(A,B,C,D,E,F) :- G>= 21, G=B, H= 20, new20(A,B,C,D,E,F).
new18(A,B,C,D,E,F) :- G+ 1=< 20, G=B, H= 20, new20(A,B,C,D,E,F).
new17(A,B,C,D,E,F) :- G=A, H= 0, I= 0, new5(A,G,J,K,L,M), new18(K,I,H,D,E,F).
new17(A,B,C,D,E,F) :- G=A, H=I+ 1, I=J, K= 1, L=H, new7(A,G,M,N,O,J), 
          new18(N,L,H,D,E,F).
new16(A,B,C,D,E,F) :- new17(A,B,C,D,E,F).
new14(A,B) :- new16(A,C,D,B,E,F).
safe :- init(A), new14(A,B).
new11(A).
new9(A,B,C,A,B,C) :- new11(A).
new8(A,B,C,D,B,E) :- F=G- 1, G=B, H= 1, E= 0, new5(A,F,I,D,J,K).
new8(A,B,C,D,B,E) :- F=G- 1, G=B, H= 1, E=I+ 1, I=J, K= 1, new7(A,F,L,D,M,J).
new7(A,B,C,D,E,F) :- G>= 1, G=B, H= 0, new8(A,B,C,D,E,F).
new7(A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0, new8(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G= 20, G=B, H= 20, new9(A,B,C,D,E,F).
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
