new57(A,B,C,D,E,F,G,H,I,J) :-  0=<K, L= -1, K=D, new54(A,B,C,D,E,F,G,H,I,J).
new56(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=A, new57(A,B,C,D,E,F,G,H,I,J).
new55(A,B,C,D,E,F,G,H,I,J) :-  0=<K, L= -1, K=C, new56(A,B,C,D,E,F,G,H,I,J).
new54(A,B,C,D,E,F,G,H,I,J) :- K=L+ 1, L=B, M= 1, new43(A,K,C,D,E,F,G,H,I,J).
new53(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=A, new55(A,B,C,D,E,F,G,H,I,J).
new52(A,B,C,D,E,F,G,H,I,J) :- K>=L+ 1, K=C, L=D, new53(A,B,C,D,E,F,G,H,I,J).
new52(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=D, new53(A,B,C,D,E,F,G,H,I,J).
new52(A,B,C,D,E,F,G,H,I,J) :- K=L, K=C, L=D, new54(A,B,C,D,E,F,G,H,I,J).
new51(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, M=B, new52(A,B,C,M,E,F,G,H,I,J).
new51(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=E, L= 0, M=B, 
          new52(A,B,C,M,E,F,G,H,I,J).
new51(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, new52(A,B,C,D,E,F,G,H,I,J).
new50(A,B,C,D,E,F,G,H,I,J) :- new51(A,B,C,D,K,F,G,H,I,J).
new49(A,B,C,D,E,F,G,H,I,J) :-  0=<K, L= -1, K=D, new50(A,B,C,D,E,F,G,H,I,J).
new48(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=A, new49(A,B,C,D,E,F,G,H,I,J).
new47(A,B,C,D,E,F,G,H,I,J) :-  0=<K, L= -1, K=B, new48(A,B,C,D,E,F,G,H,I,J).
new45(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=B, L=A, new47(A,B,C,D,E,F,G,H,I,J).
new44(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=B, L=A, new45(A,B,C,D,E,F,G,H,I,J).
new44(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=B, L=A, M=N+ 1, N=C, O= 1, 
          new41(A,B,M,D,E,F,G,H,I,J).
new43(A,B,C,D,E,F,G,H,I,J) :- new44(A,B,C,D,E,F,G,H,I,J).
new42(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=M- 1, M=A, N= 1, O=C, P=Q+ 1, 
          Q=C, R= 1, new43(A,P,C,O,E,F,G,H,I,J).
new42(A,B,C,D,E,A,B,C,D,E) :- F>=G, F=C, G=H- 1, H=A, I= 1.
new41(A,B,C,D,E,F,G,H,I,J) :- new42(A,B,C,D,E,F,G,H,I,J).
new22(A,B,C,D,E,F,G,H,I,J) :- K= 0, new41(A,B,C,K,E,F,G,H,I,J).
new20(A,B,C,D,E,F) :- new22(A,B,C,G,H,D,E,F,I,J).
safe :- init(A,B,C), new20(A,B,C,D,E,F).
new19(A,B,C,D,E,F,G,H,I,J) :-  -1>=K, L= -1, K=D, new8(A,B,C,D,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :-  0=<K, L= -1, K=D, new16(A,B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=D, L=A, new8(A,B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=A, new19(A,B,C,D,E,F,G,H,I,J).
new17(A,B,C,D,E,F,G,H,I,J) :-  -1>=K, L= -1, K=C, new8(A,B,C,D,E,F,G,H,I,J).
new17(A,B,C,D,E,F,G,H,I,J) :-  0=<K, L= -1, K=C, new18(A,B,C,D,E,F,G,H,I,J).
new16(A,B,C,D,E,F,G,H,I,J) :- K=L+ 1, L=B, M= 1, new5(A,K,C,D,E,F,G,H,I,J).
new15(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=C, L=A, new8(A,B,C,D,E,F,G,H,I,J).
new15(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=A, new17(A,B,C,D,E,F,G,H,I,J).
new14(A,B,C,D,E,F,G,H,I,J) :- K>=L+ 1, K=C, L=D, new15(A,B,C,D,E,F,G,H,I,J).
new14(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=D, new15(A,B,C,D,E,F,G,H,I,J).
new14(A,B,C,D,E,F,G,H,I,J) :- K=L, K=C, L=D, new16(A,B,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, M=B, new14(A,B,C,M,E,F,G,H,I,J).
new13(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=E, L= 0, M=B, 
          new14(A,B,C,M,E,F,G,H,I,J).
new13(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, new14(A,B,C,D,E,F,G,H,I,J).
new12(A,B,C,D,E,F,G,H,I,J) :- new13(A,B,C,D,K,F,G,H,I,J).
new11(A,B,C,D,E,F,G,H,I,J) :-  -1>=K, L= -1, K=D, new8(A,B,C,D,E,F,G,H,I,J).
new11(A,B,C,D,E,F,G,H,I,J) :-  0=<K, L= -1, K=D, new12(A,B,C,D,E,F,G,H,I,J).
new10(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=D, L=A, new8(A,B,C,D,E,F,G,H,I,J).
new10(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=A, new11(A,B,C,D,E,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :-  -1>=K, L= -1, K=B, new8(A,B,C,D,E,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :-  0=<K, L= -1, K=B, new10(A,B,C,D,E,F,G,H,I,J).
new8(A,B,C,D,E,A,B,C,D,E).
new7(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=B, L=A, new8(A,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=B, L=A, new9(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=B, L=A, new7(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=B, L=A, M=N+ 1, N=C, O= 1, 
          new3(A,B,M,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- new6(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=M- 1, M=A, N= 1, O=C, P=Q+ 1, Q=C, 
          R= 1, new5(A,P,C,O,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- new4(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K= 0, new3(A,B,C,K,E,F,G,H,I,J).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,H,D,E,F,I,J).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
