new65(A,B,C,D,E,A,B,C,D,E) :- F>= 200, F=A, G= 200.
new54(A,B,C,D,E,A,B,C,D,E) :- F=< 100, F=B, G= 100.
new53(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 200, K=A, L= 200, 
          new54(A,B,C,D,E,F,G,H,I,J).
new43(A,B,C,D,E,A,B,C,D,E) :- F>= 200, F=B, G= 200.
new42(A,B,C,D,E,F,G,H,I,J) :- K>= 101, K=B, L= 100, new43(A,B,C,D,E,F,G,H,I,J).
new41(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 200, K=A, L= 200, 
          new42(A,B,C,D,E,F,G,H,I,J).
new40(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=C, L= 0, M=N+ 1, N=A, O= 1, 
          new29(M,B,C,D,E,F,G,H,I,J).
new40(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=C, L= 0, M=N+ 1, N=A, O= 1, 
          new29(M,B,C,D,E,F,G,H,I,J).
new40(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=C, L= 0, M=N- 1, N=A, O= 1, P=Q- 1, Q=B, 
          R= 1, new29(M,P,C,D,E,F,G,H,I,J).
new39(A,B,C,D,E,F,G,H,I,J) :- K=L, new40(A,B,K,L,E,F,G,H,I,J).
new38(A,B,C,C).
new36(A,B,C,D) :- E>= 1, E=C, F= 0, new38(A,B,C,D).
new36(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new38(A,B,C,D).
new35(A,B,C,D,E,F,G,H,I,J) :- K=< 100, K=B, L= 100, M= 1, 
          new34(A,B,C,D,M,F,G,H,I,J).
new35(A,B,C,D,E,F,G,H,I,J) :- K>= 101, K=B, L= 100, M= 0, 
          new34(A,B,C,D,M,F,G,H,I,J).
new34(A,B,C,D,E,A,B,C,D,E) :- F=E, new36(A,B,F,G).
new33(A,B,C,D,E,F,G,H,I,J) :- K>= 100, K=A, L= 100, M= 1, 
          new34(A,B,C,D,M,F,G,H,I,J).
new33(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 100, K=A, L= 100, 
          new35(A,B,C,D,E,F,G,H,I,J).
new32(A,B,C,D,E,F,G,H,I,J) :- new33(A,B,C,D,E,F,G,H,I,J).
new31(A,B,C,D,E,F,G,H,I,J) :- K>= 101, K=B, L= 100, new39(A,B,C,D,E,F,G,H,I,J).
new31(A,B,C,D,E,F,G,H,I,J) :- K=< 100, K=B, L= 100, new32(A,B,C,D,E,F,G,H,I,J).
new30(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 100, K=A, L= 100, 
          new31(A,B,C,D,E,F,G,H,I,J).
new30(A,B,C,D,E,F,G,H,I,J) :- K>= 100, K=A, L= 100, new32(A,B,C,D,E,F,G,H,I,J).
new29(A,B,C,D,E,F,G,H,I,J) :- new30(A,B,C,D,E,F,G,H,I,J).
new28(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 200, K=B, L= 200, 
          new29(A,B,C,D,E,F,G,H,I,J).
new27(A,B,C,D,E,F,G,H,I,J) :- K>= 101, K=B, L= 100, new28(A,B,C,D,E,F,G,H,I,J).
new26(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 200, K=A, L= 200, 
          new27(A,B,C,D,E,F,G,H,I,J).
new25(A,B,C,D,E,F,G,H,I,J) :- K>= -99, K=A, L= -100, new26(A,B,C,D,E,F,G,H,I,J).
new24(A,B,C,D,E,F,G,H,I,J) :- K>= -99, K=A, L= -100, new41(A,B,C,D,E,F,G,H,I,J).
new23(A,B,C,D,E,F,G,H,I,J) :- K>= -99, K=A, L= -100, new53(A,B,C,D,E,F,G,H,I,J).
new22(A,B,C,D,E,F,G,H,I,J) :- K>= -99, K=A, L= -100, new65(A,B,C,D,E,F,G,H,I,J).
new21(A,B,C,D,E,A,B,C,D,E) :- F=< -100, F=A, G= -100.
new19(A,B,C,D) :- new21(A,B,E,F,G,C,D,H,I,J).
new19(A,B,C,D) :- new22(A,B,E,F,G,C,D,H,I,J).
new19(A,B,C,D) :- new23(A,B,E,F,G,C,D,H,I,J).
new19(A,B,C,D) :- new24(A,B,E,F,G,C,D,H,I,J).
new19(A,B,C,D) :- new25(A,B,E,F,G,C,D,H,I,J).
safe :- init(A,B), new19(A,B,C,D).
new18(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=C, L= 0, M=N+ 1, N=A, O= 1, 
          new6(M,B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=C, L= 0, M=N+ 1, N=A, O= 1, 
          new6(M,B,C,D,E,F,G,H,I,J).
new18(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=C, L= 0, M=N- 1, N=A, O= 1, P=Q- 1, Q=B, 
          R= 1, new6(M,P,C,D,E,F,G,H,I,J).
new17(A,B,C,D,E,F,G,H,I,J) :- K=L, new18(A,B,K,L,E,F,G,H,I,J).
new16(A,B).
new14(A,B,C,C) :- new16(A,B).
new13(A,B,C,D) :- E= 0, E=C, F= 0, new14(A,B,C,D).
new12(A,B,C,D,E,F,G,H,I,J) :- K=< 100, K=B, L= 100, M= 1, 
          new11(A,B,C,D,M,F,G,H,I,J).
new12(A,B,C,D,E,F,G,H,I,J) :- K>= 101, K=B, L= 100, M= 0, 
          new11(A,B,C,D,M,F,G,H,I,J).
new11(A,B,C,D,E,A,B,C,D,E) :- F=E, new13(A,B,F,G).
new10(A,B,C,D,E,F,G,H,I,J) :- K>= 100, K=A, L= 100, M= 1, 
          new11(A,B,C,D,M,F,G,H,I,J).
new10(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 100, K=A, L= 100, 
          new12(A,B,C,D,E,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :- new10(A,B,C,D,E,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- K>= 101, K=B, L= 100, new17(A,B,C,D,E,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- K=< 100, K=B, L= 100, new9(A,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 100, K=A, L= 100, new8(A,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J) :- K>= 100, K=A, L= 100, new9(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- new7(A,B,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 200, K=B, L= 200, new6(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K>= 101, K=B, L= 100, new5(A,B,C,D,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 200, K=A, L= 200, new4(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K>= -99, K=A, L= -100, new3(A,B,C,D,E,F,G,H,I,J).
new1(A,B,C,D) :- new2(A,B,E,F,G,C,D,H,I,J).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
