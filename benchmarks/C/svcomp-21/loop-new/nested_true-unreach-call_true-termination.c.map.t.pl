new57(A,B,C,D,E,A,B,C,D,E) :- F>= 10001, F=B, G= 10000.
new43(A,B,C,D,E,A,B,C,D,E) :- F>= 10001, F=C, G= 10000.
new42(A,B,C,D,E,F,G,H,I,J) :-  10=<K, L= 10, K=C, new43(A,B,C,D,E,F,G,H,I,J).
new41(A,B,C,D,E,F,G,H,I,J) :- K=< 10000, K=B, L= 10000, 
          new42(A,B,C,D,E,F,G,H,I,J).
new34(A,B,C,D,E,A,B,C,D,E) :-  10>=F+ 1, G= 10, F=C.
new33(A,B,C,D,E,F,G,H,I,J) :- K=< 10000, K=B, L= 10000, 
          new34(A,B,C,D,E,F,G,H,I,J).
new32(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=E, L=C, M=N+ 1, N=A, O= 1, P=Q+ 1, 
          Q=E, R= 1, new27(M,B,C,D,P,F,G,H,I,J).
new32(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=E, L=C, M=N+ 1, N=D, O= 1, 
          new25(A,B,C,M,E,F,G,H,I,J).
new31(A,B,C,D,D).
new29(A,B,C,D,E) :- F>= 1, F=D, G= 0, new31(A,B,C,D,E).
new29(A,B,C,D,E) :- F+ 1=< 0, F=D, G= 0, new31(A,B,C,D,E).
new28(A,B,C,D,E,A,B,C,D,E) :- F= 1, G>= 100, G=A, H= 100, new29(A,B,C,F,I).
new28(A,B,C,D,E,A,B,C,D,E) :- F= 0, G+ 1=< 100, G=A, H= 100, new29(A,B,C,F,I).
new27(A,B,C,D,E,F,G,H,I,J) :- new32(A,B,C,D,E,F,G,H,I,J).
new26(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=B, M= 0, 
          new27(A,B,C,D,M,F,G,H,I,J).
new26(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=D, L=B, new28(A,B,C,D,E,F,G,H,I,J).
new25(A,B,C,D,E,F,G,H,I,J) :- new26(A,B,C,D,E,F,G,H,I,J).
new24(A,B,C,D,E,F,G,H,I,J) :- K=< 10000, K=C, L= 10000, M= 0, 
          new25(A,B,C,M,E,F,G,H,I,J).
new23(A,B,C,D,E,F,G,H,I,J) :-  10=<K, L= 10, K=C, new24(A,B,C,D,E,F,G,H,I,J).
new22(A,B,C,D,E,F,G,H,I,J) :- K=< 10000, K=B, L= 10000, 
          new23(A,B,C,D,E,F,G,H,I,J).
new21(A,B,C,D,E,F,G,H,I,J) :-  10=<K, L= 10, K=B, new22(A,B,C,D,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J) :-  10=<K, L= 10, K=B, new33(A,B,C,D,E,F,G,H,I,J).
new19(A,B,C,D,E,F,G,H,I,J) :-  10=<K, L= 10, K=B, new41(A,B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,A,B,C,D,E) :-  10>=F+ 1, G= 10, F=B.
new17(A,B,C,D,E,F,G,H,I,J) :-  10=<K, L= 10, K=B, new57(A,B,C,D,E,F,G,H,I,J).
new15(A,B,C,D,E,F) :- new17(A,B,C,G,H,D,E,F,I,J).
new15(A,B,C,D,E,F) :- new18(A,B,C,G,H,D,E,F,I,J).
new15(A,B,C,D,E,F) :- new19(A,B,C,G,H,D,E,F,I,J).
new15(A,B,C,D,E,F) :- new20(A,B,C,G,H,D,E,F,I,J).
new15(A,B,C,D,E,F) :- new21(A,B,C,G,H,D,E,F,I,J).
safe :- init(A,B,C), new15(A,B,C,D,E,F).
new14(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=E, L=C, M=N+ 1, N=A, O= 1, P=Q+ 1, 
          Q=E, R= 1, new8(M,B,C,D,P,F,G,H,I,J).
new14(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=E, L=C, M=N+ 1, N=D, O= 1, 
          new6(A,B,C,M,E,F,G,H,I,J).
new13(A,B,C).
new11(A,B,C,D,D) :- new13(A,B,C).
new10(A,B,C,D,E) :- F= 0, F=D, G= 0, new11(A,B,C,D,E).
new9(A,B,C,D,E,A,B,C,D,E) :- F= 1, G>= 100, G=A, H= 100, new10(A,B,C,F,I).
new9(A,B,C,D,E,A,B,C,D,E) :- F= 0, G+ 1=< 100, G=A, H= 100, new10(A,B,C,F,I).
new8(A,B,C,D,E,F,G,H,I,J) :- new14(A,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=B, M= 0, new8(A,B,C,D,M,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=D, L=B, new9(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- new7(A,B,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- K=< 10000, K=C, L= 10000, M= 0, 
          new6(A,B,C,M,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :-  10=<K, L= 10, K=C, new5(A,B,C,D,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- K=< 10000, K=B, L= 10000, 
          new4(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :-  10=<K, L= 10, K=B, new3(A,B,C,D,E,F,G,H,I,J).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,H,D,E,F,I,J).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
