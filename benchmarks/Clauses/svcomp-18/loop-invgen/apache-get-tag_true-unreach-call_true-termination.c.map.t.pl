new74(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=<O, N=B, O=A, new12(A,B,M,P), 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new74(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O+ 1, N=B, O=A, new12(A,B,M,P), 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new73(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=<O, N=B, O=A, new12(A,B,M,P), 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new73(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O+ 1, N=B, O=A, new12(A,B,M,P), 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new72(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=<O, N=B, O=A, new12(A,B,M,P), 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new72(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O+ 1, N=B, O=A, new12(A,B,M,P), 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new71(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new72(A,B,C,D,E,F,G,H,I,J,K,L).
new71(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new72(A,B,C,D,E,F,G,H,I,J,K,L).
new70(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N, M=B, N=A, new71(A,B,C,D,E,F,G,H,I,J,K,L).
new70(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=B, N=A, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L).
new70(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=B, N=A, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L).
new69(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=<O, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new70(A,P,C,D,E,F,G,H,I,J,K,L).
new69(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O+ 1, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new70(A,P,C,D,E,F,G,H,I,J,K,L).
new68(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new69(A,B,C,D,E,F,G,H,I,J,K,L).
new68(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new69(A,B,C,D,E,F,G,H,I,J,K,L).
new67(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L).
new67(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=D, N= 0, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L).
new67(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=D, N= 0, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L).
new66(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=<O, N=B, O=A, new12(A,B,M,P), 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new66(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O+ 1, N=B, O=A, new12(A,B,M,P), 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new65(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=<O, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new54(A,P,C,D,E,F,G,H,I,J,K,L).
new65(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O+ 1, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new54(A,P,C,D,E,F,G,H,I,J,K,L).
new64(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new65(A,B,C,D,E,F,G,H,I,J,K,L).
new64(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new65(A,B,C,D,E,F,G,H,I,J,K,L).
new63(A,B,C,D,E,F,G,H,I,J,K,L) :- new64(A,B,C,D,E,F,G,H,I,J,K,L).
new62(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new66(A,B,C,D,E,F,G,H,I,J,K,L).
new62(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new66(A,B,C,D,E,F,G,H,I,J,K,L).
new61(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new62(A,B,C,D,E,F,G,H,I,J,K,L).
new61(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, 
          new62(A,B,C,D,E,F,G,H,I,J,K,L).
new61(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L).
new60(A,B,C,D,E,F,G,H,I,J,K,L) :- new61(A,B,C,D,M,F,G,H,I,J,K,L).
new59(A,B,C,D,E,F,G,H,I,J,K,L) :- new67(A,B,C,M,E,F,G,H,I,J,K,L).
new58(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L).
new58(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L).
new58(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=F, N= 0, 
          new60(A,B,C,D,E,F,G,H,I,J,K,L).
new57(A,B,C,D,E,F,G,H,I,J,K,L) :- new58(A,B,C,D,E,M,G,H,I,J,K,L).
new56(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new73(A,B,C,D,E,F,G,H,I,J,K,L).
new56(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new73(A,B,C,D,E,F,G,H,I,J,K,L).
new55(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N, M=B, N=A, new56(A,B,C,D,E,F,G,H,I,J,K,L).
new55(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=B, N=A, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L).
new55(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=B, N=A, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L).
new54(A,B,C,D,E,F,G,H,I,J,K,L) :- new55(A,B,C,D,E,F,G,H,I,J,K,L).
new53(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=<O, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new54(A,P,C,D,E,F,G,H,I,J,K,L).
new53(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O+ 1, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new54(A,P,C,D,E,F,G,H,I,J,K,L).
new52(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=<O, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new44(A,P,C,D,E,F,G,H,I,J,K,L).
new52(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O+ 1, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new44(A,P,C,D,E,F,G,H,I,J,K,L).
new51(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new52(A,B,C,D,E,F,G,H,I,J,K,L).
new51(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new52(A,B,C,D,E,F,G,H,I,J,K,L).
new50(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new53(A,B,C,D,E,F,G,H,I,J,K,L).
new50(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new53(A,B,C,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new51(A,B,C,D,E,F,G,H,I,J,K,L).
new48(A,B,C,D,E,F,G,H,I,J,K,L) :- new49(A,B,M,D,E,F,G,H,I,J,K,L).
new47(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new74(A,B,C,D,E,F,G,H,I,J,K,L).
new47(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new74(A,B,C,D,E,F,G,H,I,J,K,L).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N, M=B, N=A, new47(A,B,C,D,E,F,G,H,I,J,K,L).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=B, N=A, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=B, N=A, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L).
new45(A,B,C,D,E,F,A,B,C,D,E,F).
new44(A,B,C,D,E,F,G,H,I,J,K,L) :- new46(A,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=A, N= 1, O=P- 1, P=A, Q= 1, 
          new44(O,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 1, M=A, N= 1, 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new41(A,B,C,D) :- new43(A,B,E,F,G,H,C,D,I,J,K,L).
safe :- init(A,B), new41(A,B,C,D).
new40(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H=<I, H=B, I=A, new11(A,B,G,J).
new40(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I+ 1, H=B, I=A, new11(A,B,G,J).
new39(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H=<I, H=B, I=A, new11(A,B,G,J).
new39(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I+ 1, H=B, I=A, new11(A,B,G,J).
new38(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H=<I, H=B, I=A, new11(A,B,G,J).
new38(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I+ 1, H=B, I=A, new11(A,B,G,J).
new37(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=B, new11(A,B,G,J).
new37(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new38(A,B,C,D,E,F,G,H,I,J,K,L).
new37(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=B, new11(A,B,G,J).
new37(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new38(A,B,C,D,E,F,G,H,I,J,K,L).
new36(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N, M=B, N=A, new37(A,B,C,D,E,F,G,H,I,J,K,L).
new36(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=B, N=A, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L).
new36(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=B, N=A, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L).
new35(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H=<I, H=B, I=A, new11(A,B,G,J).
new35(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=<O, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new36(A,P,C,D,E,F,G,H,I,J,K,L).
new35(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I+ 1, H=B, I=A, new11(A,B,G,J).
new35(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O+ 1, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new36(A,P,C,D,E,F,G,H,I,J,K,L).
new34(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=B, new11(A,B,G,J).
new34(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new35(A,B,C,D,E,F,G,H,I,J,K,L).
new34(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=B, new11(A,B,G,J).
new34(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new35(A,B,C,D,E,F,G,H,I,J,K,L).
new33(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new34(A,B,C,D,E,F,G,H,I,J,K,L).
new33(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=D, N= 0, 
          new34(A,B,C,D,E,F,G,H,I,J,K,L).
new33(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=D, N= 0, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L).
new32(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H=<I, H=B, I=A, new11(A,B,G,J).
new32(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I+ 1, H=B, I=A, new11(A,B,G,J).
new31(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H=<I, H=B, I=A, new11(A,B,G,J).
new31(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=<O, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new20(A,P,C,D,E,F,G,H,I,J,K,L).
new31(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I+ 1, H=B, I=A, new11(A,B,G,J).
new31(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O+ 1, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new20(A,P,C,D,E,F,G,H,I,J,K,L).
new30(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=B, new11(A,B,G,J).
new30(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new31(A,B,C,D,E,F,G,H,I,J,K,L).
new30(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=B, new11(A,B,G,J).
new30(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new31(A,B,C,D,E,F,G,H,I,J,K,L).
new29(A,B,C,D,E,F,G,H,I,J,K,L) :- new30(A,B,C,D,E,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=B, new11(A,B,G,J).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new32(A,B,C,D,E,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=B, new11(A,B,G,J).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new32(A,B,C,D,E,F,G,H,I,J,K,L).
new27(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L).
new27(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L).
new27(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L).
new26(A,B,C,D,E,F,G,H,I,J,K,L) :- new27(A,B,C,D,M,F,G,H,I,J,K,L).
new25(A,B,C,D,E,F,G,H,I,J,K,L) :- new33(A,B,C,M,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new25(A,B,C,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new25(A,B,C,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=F, N= 0, 
          new26(A,B,C,D,E,F,G,H,I,J,K,L).
new23(A,B,C,D,E,F,G,H,I,J,K,L) :- new24(A,B,C,D,E,M,G,H,I,J,K,L).
new22(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=B, new11(A,B,G,J).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new39(A,B,C,D,E,F,G,H,I,J,K,L).
new22(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=B, new11(A,B,G,J).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new39(A,B,C,D,E,F,G,H,I,J,K,L).
new21(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N, M=B, N=A, new22(A,B,C,D,E,F,G,H,I,J,K,L).
new21(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=B, N=A, 
          new23(A,B,C,D,E,F,G,H,I,J,K,L).
new21(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=B, N=A, 
          new23(A,B,C,D,E,F,G,H,I,J,K,L).
new20(A,B,C,D,E,F,G,H,I,J,K,L) :- new21(A,B,C,D,E,F,G,H,I,J,K,L).
new19(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H=<I, H=B, I=A, new11(A,B,G,J).
new19(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=<O, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new20(A,P,C,D,E,F,G,H,I,J,K,L).
new19(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I+ 1, H=B, I=A, new11(A,B,G,J).
new19(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O+ 1, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new20(A,P,C,D,E,F,G,H,I,J,K,L).
new18(A,B).
new16(A,B,C,C) :- new18(A,B).
new15(A,B,C,C).
new13(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H=<I, H=B, I=A, new11(A,B,G,J).
new13(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=<O, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new3(A,P,C,D,E,F,G,H,I,J,K,L).
new13(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I+ 1, H=B, I=A, new11(A,B,G,J).
new13(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O+ 1, N=B, O=A, P=Q+ 1, Q=B, R= 1, 
          new12(A,B,M,S), new3(A,P,C,D,E,F,G,H,I,J,K,L).
new12(A,B,C,D) :- E>= 1, E=C, F= 0, new15(A,B,C,D).
new12(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new15(A,B,C,D).
new11(A,B,C,D) :- E= 0, E=C, F= 0, new16(A,B,C,D).
new10(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=B, new11(A,B,G,J).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new13(A,B,C,D,E,F,G,H,I,J,K,L).
new10(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=B, new11(A,B,G,J).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new13(A,B,C,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=B, new11(A,B,G,J).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new19(A,B,C,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=B, new11(A,B,G,J).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new19(A,B,C,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new10(A,B,C,D,E,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- new8(A,B,M,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=B, new11(A,B,G,J).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=B, new12(A,B,M,P), 
          new40(A,B,C,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=B, new11(A,B,G,J).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=B, new12(A,B,M,P), 
          new40(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N, M=B, N=A, new6(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=B, N=A, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=B, N=A, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- new5(A,B,C,D,E,F,G,H,I,J,K,L).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=A, N= 1, O=P- 1, P=A, Q= 1, 
          new3(O,B,C,D,E,F,G,H,I,J,K,L).
new1(A,B,C,D) :- new2(A,B,E,F,G,H,C,D,I,J,K,L).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
