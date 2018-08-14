new86(A,B,C,D,E,F,A,B,C,D,E,F) :- G>= 1000000, G=D, H= 1000000.
new85(A,B,C,D,E,F,G,H,I,J,K,L) :- new86(A,B,C,D,E,M,G,H,I,J,K,L).
new84(A,B,C,D,E,F,G,H,I,J,K,L) :- new85(A,B,C,M,E,F,G,H,I,J,K,L).
new70(A,B,C,D,E,F,A,B,C,D,E,F) :- G>= 1000000, G=E, H= 1000000.
new69(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 1000000, M=D, N= 1000000, 
          new70(A,B,C,D,E,F,G,H,I,J,K,L).
new68(A,B,C,D,E,F,G,H,I,J,K,L) :- new69(A,B,C,D,E,M,G,H,I,J,K,L).
new67(A,B,C,D,E,F,G,H,I,J,K,L) :- new68(A,B,C,M,E,F,G,H,I,J,K,L).
new54(A,B,C,D,E,F,A,B,C,D,E,F) :- G>= 1000000, G=F, H= 1000000.
new53(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 1000000, M=E, N= 1000000, 
          new54(A,B,C,D,E,F,G,H,I,J,K,L).
new52(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 1000000, M=D, N= 1000000, 
          new53(A,B,C,D,E,F,G,H,I,J,K,L).
new51(A,B,C,D,E,F,G,H,I,J,K,L) :- new52(A,B,C,D,E,M,G,H,I,J,K,L).
new50(A,B,C,D,E,F,G,H,I,J,K,L) :- new51(A,B,C,M,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=P+ 1, P=B, Q= 1, O=A, 
          R=S+ 2, S=B, T= 2, U=V+ 1, V=C, W= 1, new20(A,B,M,X), 
          new44(A,R,U,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=P+ 1, P=B, Q= 1, O=A, R=S+ 2, 
          S=B, T= 2, U=V+ 1, V=C, W= 1, new20(A,B,M,X), 
          new44(A,R,U,D,E,F,G,H,I,J,K,L).
new48(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new20(A,B,M,P), 
          new49(A,B,C,D,E,F,G,H,I,J,K,L).
new48(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new20(A,B,M,P), 
          new49(A,B,C,D,E,F,G,H,I,J,K,L).
new47(A,B,C,D,E,F,G,H,I,J,K,L) :- new40(A,B,C,D,E,F,G,H,I,J,K,L).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 3, M=E, N= 2, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 2, M=E, N= 2, 
          new47(A,B,C,D,E,F,G,H,I,J,K,L).
new45(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=C, N=F, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L).
new45(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=C, N=F, 
          new47(A,B,C,D,E,F,G,H,I,J,K,L).
new44(A,B,C,D,E,F,G,H,I,J,K,L) :- new45(A,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=E, N= 2*O, P= 2, O=F, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=E, N= 2*O, P= 2, O=F, Q= 0, 
          new44(A,B,Q,D,E,F,G,H,I,J,K,L).
new42(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=E, N=D, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L).
new42(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=E, N=D, O=E, P=Q-R, Q=E, R=D, S=T+U, 
          T=B, U=D, new43(O,S,C,D,P,F,G,H,I,J,K,L).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new42(A,B,C,D,E,F,G,H,I,J,K,L).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=F, N= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L).
new40(A,B,C,D,E,F,A,B,C,D,E,F).
new39(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new41(A,B,C,D,E,F,G,H,I,J,K,L).
new39(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=E, N= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L).
new38(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new39(A,B,C,D,E,F,G,H,I,J,K,L).
new38(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=D, N= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L).
new37(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 1000000, M=F, N= 1000000, 
          new38(A,B,C,D,E,F,G,H,I,J,K,L).
new36(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 1000000, M=E, N= 1000000, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L).
new35(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 1000000, M=D, N= 1000000, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L).
new34(A,B,C,D,E,F,G,H,I,J,K,L) :- new35(A,B,C,D,E,M,G,H,I,J,K,L).
new33(A,B,C,D,E,F,G,H,I,J,K,L) :- new34(A,B,C,M,E,F,G,H,I,J,K,L).
new32(A,B,C,D,E,F,G,H,I,J,K,L) :- new33(A,B,C,D,E,F,G,H,I,J,K,L).
new31(A,B,C,D,E,F,G,H,I,J,K,L) :- new50(A,B,C,D,E,F,G,H,I,J,K,L).
new30(A,B,C,D,E,F,G,H,I,J,K,L) :- new67(A,B,C,D,E,F,G,H,I,J,K,L).
new29(A,B,C,D,E,F,G,H,I,J,K,L) :- new84(A,B,C,D,E,F,G,H,I,J,K,L).
new27(A,B,C,D) :- new29(A,B,E,F,G,H,C,D,I,J,K,L).
new27(A,B,C,D) :- new30(A,B,E,F,G,H,C,D,I,J,K,L).
new27(A,B,C,D) :- new31(A,B,E,F,G,H,C,D,I,J,K,L).
new27(A,B,C,D) :- new32(A,B,E,F,G,H,C,D,I,J,K,L).
safe :- init(A,B), new27(A,B,C,D).
new26(A,B).
new24(A,B,C,C) :- new26(A,B).
new23(A,B,C,C).
new21(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=J+ 1, J=B, K= 1, I=A, 
          new19(A,B,G,L).
new21(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=P+ 1, P=B, Q= 1, O=A, 
          R=S+ 2, S=B, T= 2, U=V+ 1, V=C, W= 1, new20(A,B,M,X), 
          new14(A,R,U,D,E,F,G,H,I,J,K,L).
new21(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=J+ 1, J=B, K= 1, I=A, 
          new19(A,B,G,L).
new21(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=P+ 1, P=B, Q= 1, O=A, R=S+ 2, 
          S=B, T= 2, U=V+ 1, V=C, W= 1, new20(A,B,M,X), 
          new14(A,R,U,D,E,F,G,H,I,J,K,L).
new20(A,B,C,D) :- E>= 1, E=C, F= 0, new23(A,B,C,D).
new20(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new23(A,B,C,D).
new19(A,B,C,D) :- E= 0, E=C, F= 0, new24(A,B,C,D).
new18(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=B, new19(A,B,G,J).
new18(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new20(A,B,M,P), 
          new21(A,B,C,D,E,F,G,H,I,J,K,L).
new18(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=B, new19(A,B,G,J).
new18(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new20(A,B,M,P), 
          new21(A,B,C,D,E,F,G,H,I,J,K,L).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 3, M=E, N= 2, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L).
new15(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=C, N=F, 
          new16(A,B,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- new15(A,B,C,D,E,F,G,H,I,J,K,L).
new13(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=E, N= 2*O, P= 2, O=F, Q= 0, 
          new14(A,B,Q,D,E,F,G,H,I,J,K,L).
new12(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=E, N=D, O=E, P=Q-R, Q=E, R=D, S=T+U, 
          T=B, U=D, new13(O,S,C,D,P,F,G,H,I,J,K,L).
new11(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new12(A,B,C,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 1000000, M=F, N= 1000000, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 1000000, M=E, N= 1000000, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 1000000, M=D, N= 1000000, 
          new6(A,B,C,D,E,F,G,H,I,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- new5(A,B,C,D,E,M,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- new4(A,B,C,M,E,F,G,H,I,J,K,L).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- new3(A,B,C,D,E,F,G,H,I,J,K,L).
new1(A,B,C,D) :- new2(A,B,E,F,G,H,C,D,I,J,K,L).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
