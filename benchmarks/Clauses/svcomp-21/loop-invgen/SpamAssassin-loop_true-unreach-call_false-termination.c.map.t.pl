new63(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          new13(A,B,M,S), new41(A,B,C,P,E,F,G,H,I,J,K,L).
new63(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          new13(A,B,M,S), new41(A,B,C,P,E,F,G,H,I,J,K,L).
new62(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=D, O=A, new13(A,B,M,P), 
          new63(A,B,C,D,E,F,G,H,I,J,K,L).
new62(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=D, O=A, new13(A,B,M,P), 
          new63(A,B,C,D,E,F,G,H,I,J,K,L).
new61(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          S=T+ 1, T=C, U= 1, new13(A,B,M,V), new62(A,B,S,P,E,F,G,H,I,J,K,L).
new61(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          S=T+ 1, T=C, U= 1, new13(A,B,M,V), new62(A,B,S,P,E,F,G,H,I,J,K,L).
new60(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=D, O=A, new13(A,B,M,P), 
          new61(A,B,C,D,E,F,G,H,I,J,K,L).
new60(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=D, O=A, new13(A,B,M,P), 
          new61(A,B,C,D,E,F,G,H,I,J,K,L).
new59(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,M,P), 
          new60(A,B,C,D,E,F,G,H,I,J,K,L).
new59(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,M,P), 
          new60(A,B,C,D,E,F,G,H,I,J,K,L).
new58(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=B, new13(A,B,M,P), 
          new59(A,B,C,D,E,F,G,H,I,J,K,L).
new58(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=B, new13(A,B,M,P), 
          new59(A,B,C,D,E,F,G,H,I,J,K,L).
new57(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          S=T+ 1, T=C, U= 1, new13(A,B,M,V), new58(A,B,S,P,E,F,G,H,I,J,K,L).
new57(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          S=T+ 1, T=C, U= 1, new13(A,B,M,V), new58(A,B,S,P,E,F,G,H,I,J,K,L).
new56(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=D, O=A, new13(A,B,M,P), 
          new57(A,B,C,D,E,F,G,H,I,J,K,L).
new56(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=D, O=A, new13(A,B,M,P), 
          new57(A,B,C,D,E,F,G,H,I,J,K,L).
new55(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,M,P), 
          new56(A,B,C,D,E,F,G,H,I,J,K,L).
new55(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,M,P), 
          new56(A,B,C,D,E,F,G,H,I,J,K,L).
new54(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=B, new13(A,B,M,P), 
          new55(A,B,C,D,E,F,G,H,I,J,K,L).
new54(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=B, new13(A,B,M,P), 
          new55(A,B,C,D,E,F,G,H,I,J,K,L).
new53(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new47(A,B,C,D,E,F,G,H,I,J,K,L).
new53(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new47(A,B,C,D,E,F,G,H,I,J,K,L).
new53(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=F, N= 0, 
          new54(A,B,C,D,E,F,G,H,I,J,K,L).
new52(A,B,C,D,E,F,G,H,I,J,K,L) :- new53(A,B,C,D,E,M,G,H,I,J,K,L).
new51(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,M,P), 
          new52(A,B,C,D,E,F,G,H,I,J,K,L).
new51(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,M,P), 
          new52(A,B,C,D,E,F,G,H,I,J,K,L).
new50(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          S=T+ 1, T=C, U= 1, new13(A,B,M,V), new41(A,B,S,P,E,F,G,H,I,J,K,L).
new50(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          S=T+ 1, T=C, U= 1, new13(A,B,M,V), new41(A,B,S,P,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=D, O=A, new13(A,B,M,P), 
          new50(A,B,C,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=D, O=A, new13(A,B,M,P), 
          new50(A,B,C,D,E,F,G,H,I,J,K,L).
new48(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,M,P), 
          new49(A,B,C,D,E,F,G,H,I,J,K,L).
new48(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,M,P), 
          new49(A,B,C,D,E,F,G,H,I,J,K,L).
new47(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=B, new13(A,B,M,P), 
          new48(A,B,C,D,E,F,G,H,I,J,K,L).
new47(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=B, new13(A,B,M,P), 
          new48(A,B,C,D,E,F,G,H,I,J,K,L).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=P+ 1, P=C, Q= 1, O=B, 
          new13(A,B,M,R), new51(A,B,C,D,E,F,G,H,I,J,K,L).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=P+ 1, P=C, Q= 1, O=B, 
          new13(A,B,M,R), new51(A,B,C,D,E,F,G,H,I,J,K,L).
new45(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=O+ 1, O=C, P= 1, N=B, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L).
new45(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=O+ 1, O=C, P= 1, N=B, 
          new47(A,B,C,D,E,F,G,H,I,J,K,L).
new44(A,B,C,D,E,F,G,H,I,J,K,L) :- new39(A,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=D, N=E, 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=D, N=E, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L).
new42(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=C, N=B, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L).
new42(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=C, N=B, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- new42(A,B,C,D,E,F,G,H,I,J,K,L).
new40(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=C, N=B, O= 0, 
          new41(A,B,C,O,E,F,G,H,I,J,K,L).
new40(A,B,C,D,E,F,A,B,C,D,E,F) :- G>=H, G=C, H=B.
new39(A,B,C,D,E,F,G,H,I,J,K,L) :- new40(A,B,C,D,E,F,G,H,I,J,K,L).
new38(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 0, M=A, N= 0, O=P- 4, P=A, Q= 4, R= 0, 
          new39(A,B,R,D,O,F,G,H,I,J,K,L).
new37(A,B,C,D,E,F,A,B,C,D,E,F) :- G+ 1=< 0, G=A, H= 0.
new35(A,B,C,D) :- new37(A,B,E,F,G,H,C,D,I,J,K,L).
new35(A,B,C,D) :- new38(A,B,E,F,G,H,C,D,I,J,K,L).
safe :- init(A,B), new35(A,B,C,D).
new34(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=D, new12(A,B,G,J).
new34(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          new13(A,B,M,S), new5(A,B,C,P,E,F,G,H,I,J,K,L).
new34(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=D, new12(A,B,G,J).
new34(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          new13(A,B,M,S), new5(A,B,C,P,E,F,G,H,I,J,K,L).
new33(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=D, I=A, new12(A,B,G,J).
new33(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=D, O=A, new13(A,B,M,P), 
          new34(A,B,C,D,E,F,G,H,I,J,K,L).
new33(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=D, I=A, new12(A,B,G,J).
new33(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=D, O=A, new13(A,B,M,P), 
          new34(A,B,C,D,E,F,G,H,I,J,K,L).
new32(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=D, new12(A,B,G,J).
new32(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          S=T+ 1, T=C, U= 1, new13(A,B,M,V), new33(A,B,S,P,E,F,G,H,I,J,K,L).
new32(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=D, new12(A,B,G,J).
new32(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          S=T+ 1, T=C, U= 1, new13(A,B,M,V), new33(A,B,S,P,E,F,G,H,I,J,K,L).
new31(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=D, I=A, new12(A,B,G,J).
new31(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=D, O=A, new13(A,B,M,P), 
          new32(A,B,C,D,E,F,G,H,I,J,K,L).
new31(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=D, I=A, new12(A,B,G,J).
new31(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=D, O=A, new13(A,B,M,P), 
          new32(A,B,C,D,E,F,G,H,I,J,K,L).
new30(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=C, new12(A,B,G,J).
new30(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,M,P), 
          new31(A,B,C,D,E,F,G,H,I,J,K,L).
new30(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=C, new12(A,B,G,J).
new30(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,M,P), 
          new31(A,B,C,D,E,F,G,H,I,J,K,L).
new29(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=C, I=B, new12(A,B,G,J).
new29(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=B, new13(A,B,M,P), 
          new30(A,B,C,D,E,F,G,H,I,J,K,L).
new29(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=C, I=B, new12(A,B,G,J).
new29(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=B, new13(A,B,M,P), 
          new30(A,B,C,D,E,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=D, new12(A,B,G,J).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          S=T+ 1, T=C, U= 1, new13(A,B,M,V), new29(A,B,S,P,E,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=D, new12(A,B,G,J).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          S=T+ 1, T=C, U= 1, new13(A,B,M,V), new29(A,B,S,P,E,F,G,H,I,J,K,L).
new27(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=D, I=A, new12(A,B,G,J).
new27(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=D, O=A, new13(A,B,M,P), 
          new28(A,B,C,D,E,F,G,H,I,J,K,L).
new27(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=D, I=A, new12(A,B,G,J).
new27(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=D, O=A, new13(A,B,M,P), 
          new28(A,B,C,D,E,F,G,H,I,J,K,L).
new26(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=C, new12(A,B,G,J).
new26(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,M,P), 
          new27(A,B,C,D,E,F,G,H,I,J,K,L).
new26(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=C, new12(A,B,G,J).
new26(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,M,P), 
          new27(A,B,C,D,E,F,G,H,I,J,K,L).
new25(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=C, I=B, new12(A,B,G,J).
new25(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=B, new13(A,B,M,P), 
          new26(A,B,C,D,E,F,G,H,I,J,K,L).
new25(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=C, I=B, new12(A,B,G,J).
new25(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=B, new13(A,B,M,P), 
          new26(A,B,C,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=F, N= 0, 
          new25(A,B,C,D,E,F,G,H,I,J,K,L).
new23(A,B,C,D,E,F,G,H,I,J,K,L) :- new24(A,B,C,D,E,M,G,H,I,J,K,L).
new22(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=C, new12(A,B,G,J).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,M,P), 
          new23(A,B,C,D,E,F,G,H,I,J,K,L).
new22(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=C, new12(A,B,G,J).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,M,P), 
          new23(A,B,C,D,E,F,G,H,I,J,K,L).
new21(A,B).
new19(A,B,C,C) :- new21(A,B).
new18(A,B,C,C).
new16(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=D, new12(A,B,G,J).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          S=T+ 1, T=C, U= 1, new13(A,B,M,V), new5(A,B,S,P,E,F,G,H,I,J,K,L).
new16(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=D, new12(A,B,G,J).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=D, P=Q+ 1, Q=D, R= 1, 
          S=T+ 1, T=C, U= 1, new13(A,B,M,V), new5(A,B,S,P,E,F,G,H,I,J,K,L).
new15(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=D, I=A, new12(A,B,G,J).
new15(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=D, O=A, new13(A,B,M,P), 
          new16(A,B,C,D,E,F,G,H,I,J,K,L).
new15(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=D, I=A, new12(A,B,G,J).
new15(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=D, O=A, new13(A,B,M,P), 
          new16(A,B,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=C, new12(A,B,G,J).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,M,P), 
          new15(A,B,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=C, new12(A,B,G,J).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,M,P), 
          new15(A,B,C,D,E,F,G,H,I,J,K,L).
new13(A,B,C,D) :- E>= 1, E=C, F= 0, new18(A,B,C,D).
new13(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new18(A,B,C,D).
new12(A,B,C,D) :- E= 0, E=C, F= 0, new19(A,B,C,D).
new11(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=C, I=B, new12(A,B,G,J).
new11(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=B, new13(A,B,M,P), 
          new14(A,B,C,D,E,F,G,H,I,J,K,L).
new11(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=C, I=B, new12(A,B,G,J).
new11(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=B, new13(A,B,M,P), 
          new14(A,B,C,D,E,F,G,H,I,J,K,L).
new10(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=J+ 1, J=C, K= 1, I=B, 
          new12(A,B,G,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=P+ 1, P=C, Q= 1, O=B, 
          new13(A,B,M,R), new22(A,B,C,D,E,F,G,H,I,J,K,L).
new10(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=J+ 1, J=C, K= 1, I=B, 
          new12(A,B,G,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=P+ 1, P=C, Q= 1, O=B, 
          new13(A,B,M,R), new22(A,B,C,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=O+ 1, O=C, P= 1, N=B, 
          new10(A,B,C,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=O+ 1, O=C, P= 1, N=B, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- new3(A,B,C,D,E,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=D, N=E, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=D, N=E, new8(A,B,C,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=C, N=B, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=C, N=B, new8(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- new6(A,B,C,D,E,F,G,H,I,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=C, N=B, O= 0, 
          new5(A,B,C,O,E,F,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- new4(A,B,C,D,E,F,G,H,I,J,K,L).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 0, M=A, N= 0, O=P- 4, P=A, Q= 4, R= 0, 
          new3(A,B,R,D,O,F,G,H,I,J,K,L).
new1(A,B,C,D) :- new2(A,B,E,F,G,H,C,D,I,J,K,L).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
