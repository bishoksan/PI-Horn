new26(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=D, L= 0, M=N+ 1, N=A, O= 1, 
          new17(M,B,C,D,E,F,G,H,I,J).
new26(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=D, L= 0, M=N+ 1, N=A, O= 1, 
          new17(M,B,C,D,E,F,G,H,I,J).
new26(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=D, L= 0, M=N- 1, N=A, O= 1, P=Q- 1, Q=C, 
          R= 1, new17(M,B,P,D,E,F,G,H,I,J).
new25(A,B,C,D,E,F,G,H,I,J) :- K=L, new26(A,B,C,K,L,F,G,H,I,J).
new24(A,B,C,D,D).
new22(A,B,C,D,E) :- F>= 1, F=D, G= 0, new24(A,B,C,D,E).
new22(A,B,C,D,E) :- F+ 1=< 0, F=D, G= 0, new24(A,B,C,D,E).
new21(A,B,C,D,E,A,B,C,D,E) :- F= 0, new22(A,B,C,F,G).
new20(A,B,C,D,E,F,G,H,I,J) :- new21(A,B,C,D,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :-  101=<K, L= 100, K=C, new25(A,B,C,D,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :-  100>=K, L= 100, K=C, new20(A,B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 100, K=A, L= 100, 
          new19(A,B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- K>= 100, K=A, L= 100, new20(A,B,C,D,E,F,G,H,I,J).
new17(A,B,C,D,E,F,G,H,I,J) :- new18(A,B,C,D,E,F,G,H,I,J).
new16(A,B,C,D,E,F,G,H,I,J) :- new17(A,B,C,D,E,F,G,H,I,J).
new14(A,B,C,D,E,F) :- new16(A,B,C,G,H,D,E,F,I,J).
safe :- init(A,B,C), new14(A,B,C,D,E,F).
new13(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=D, L= 0, M=N+ 1, N=A, O= 1, 
          new3(M,B,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=D, L= 0, M=N+ 1, N=A, O= 1, 
          new3(M,B,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=D, L= 0, M=N- 1, N=A, O= 1, P=Q- 1, Q=C, 
          R= 1, new3(M,B,P,D,E,F,G,H,I,J).
new12(A,B,C,D,E,F,G,H,I,J) :- K=L, new13(A,B,C,K,L,F,G,H,I,J).
new11(A,B,C).
new9(A,B,C,D,D) :- new11(A,B,C).
new8(A,B,C,D,E) :- F= 0, F=D, G= 0, new9(A,B,C,D,E).
new7(A,B,C,D,E,A,B,C,D,E) :- F= 0, new8(A,B,C,F,G).
new6(A,B,C,D,E,F,G,H,I,J) :- new7(A,B,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :-  101=<K, L= 100, K=C, new12(A,B,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :-  100>=K, L= 100, K=C, new6(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 100, K=A, L= 100, new5(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K>= 100, K=A, L= 100, new6(A,B,C,D,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- new4(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- new3(A,B,C,D,E,F,G,H,I,J).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,H,D,E,F,I,J).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
