new44(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=D, O=A, new13(A,B,C,M,P), 
          new32(A,B,C,D,E,F,G,H,I,J,K,L).
new44(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=D, O=A, new13(A,B,C,M,P), 
          new32(A,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=D, new13(A,B,C,M,P), 
          new44(A,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=D, new13(A,B,C,M,P), 
          new44(A,B,C,D,E,F,G,H,I,J,K,L).
new42(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=B, P=Q+ 1, Q=C, R= 1, 
          S=T+ 1, T=D, U= 1, new13(A,B,C,M,V), new43(A,B,P,S,E,F,G,H,I,J,K,L).
new42(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=B, P=Q+ 1, Q=C, R= 1, 
          S=T+ 1, T=D, U= 1, new13(A,B,C,M,V), new43(A,B,P,S,E,F,G,H,I,J,K,L).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,C,M,P), 
          new42(A,B,C,D,E,F,G,H,I,J,K,L).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,C,M,P), 
          new42(A,B,C,D,E,F,G,H,I,J,K,L).
new40(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=B, P=Q+ 1, Q=C, R= 1, 
          new13(A,B,C,M,S), new30(A,B,P,D,E,F,G,H,I,J,K,L).
new40(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=B, P=Q+ 1, Q=C, R= 1, 
          new13(A,B,C,M,S), new30(A,B,P,D,E,F,G,H,I,J,K,L).
new39(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,C,M,P), 
          new40(A,B,C,D,E,F,G,H,I,J,K,L).
new39(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,C,M,P), 
          new40(A,B,C,D,E,F,G,H,I,J,K,L).
new38(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=B, P=Q+ 1, Q=C, R= 1, 
          new13(A,B,C,M,S), new39(A,B,P,D,E,F,G,H,I,J,K,L).
new38(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=B, P=Q+ 1, Q=C, R= 1, 
          new13(A,B,C,M,S), new39(A,B,P,D,E,F,G,H,I,J,K,L).
new37(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,C,M,P), 
          new38(A,B,C,D,E,F,G,H,I,J,K,L).
new37(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,C,M,P), 
          new38(A,B,C,D,E,F,G,H,I,J,K,L).
new36(A,B,C,D,E,F,G,H,I,J,K,L) :- new37(A,B,C,D,E,F,G,H,I,J,K,L).
new35(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N, M=C, N=E, new36(A,B,C,D,E,F,G,H,I,J,K,L).
new35(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=C, N=E, 
          new41(A,B,C,D,E,F,G,H,I,J,K,L).
new35(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=C, N=E, 
          new41(A,B,C,D,E,F,G,H,I,J,K,L).
new34(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new35(A,B,C,D,E,F,G,H,I,J,K,L).
new34(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new35(A,B,C,D,E,F,G,H,I,J,K,L).
new34(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=F, N= 0, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L).
new33(A,B,C,D,E,F,G,H,I,J,K,L) :- new34(A,B,C,D,E,M,G,H,I,J,K,L).
new32(A,B,C,D,E,F,G,H,I,J,K,L) :- new33(A,B,C,D,E,F,G,H,I,J,K,L).
new31(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=B, N=A, O= 0, P=Q- 2, Q=B, R= 2, 
          new32(A,B,C,O,P,F,G,H,I,J,K,L).
new31(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=B, N=A, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L).
new30(A,B,C,D,E,F,A,B,C,D,E,F).
new29(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=A, N= 0, 
          new31(A,B,C,D,E,F,G,H,I,J,K,L).
new29(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=A, N= 0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 2, M=B, N= 1, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 1, M=B, N= 1, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L).
new26(A,B,C,D,E,F) :- new28(A,B,C,G,H,I,D,E,F,J,K,L).
safe :- init(A,B,C), new26(A,B,C,D,E,F).
new25(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=D, I=A, new12(A,B,C,G,J).
new25(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=D, O=A, new13(A,B,C,M,P), 
          new6(A,B,C,D,E,F,G,H,I,J,K,L).
new25(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=D, I=A, new12(A,B,C,G,J).
new25(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=D, O=A, new13(A,B,C,M,P), 
          new6(A,B,C,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=D, new12(A,B,C,G,J).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=D, new13(A,B,C,M,P), 
          new25(A,B,C,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=D, new12(A,B,C,G,J).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=D, new13(A,B,C,M,P), 
          new25(A,B,C,D,E,F,G,H,I,J,K,L).
new23(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=C, I=B, new12(A,B,C,G,J).
new23(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=B, P=Q+ 1, Q=C, R= 1, 
          S=T+ 1, T=D, U= 1, new13(A,B,C,M,V), new24(A,B,P,S,E,F,G,H,I,J,K,L).
new23(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=C, I=B, new12(A,B,C,G,J).
new23(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=B, P=Q+ 1, Q=C, R= 1, 
          S=T+ 1, T=D, U= 1, new13(A,B,C,M,V), new24(A,B,P,S,E,F,G,H,I,J,K,L).
new22(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=C, new12(A,B,C,G,J).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,C,M,P), 
          new23(A,B,C,D,E,F,G,H,I,J,K,L).
new22(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=C, new12(A,B,C,G,J).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,C,M,P), 
          new23(A,B,C,D,E,F,G,H,I,J,K,L).
new21(A,B,C).
new19(A,B,C,D,D) :- new21(A,B,C).
new18(A,B,C,D,D).
new16(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=C, I=B, new12(A,B,C,G,J).
new16(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=C, I=B, new12(A,B,C,G,J).
new15(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=C, new12(A,B,C,G,J).
new15(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,C,M,P), 
          new16(A,B,C,D,E,F,G,H,I,J,K,L).
new15(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=C, new12(A,B,C,G,J).
new15(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,C,M,P), 
          new16(A,B,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=C, I=B, new12(A,B,C,G,J).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=B, P=Q+ 1, Q=C, R= 1, 
          new13(A,B,C,M,S), new15(A,B,P,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=C, I=B, new12(A,B,C,G,J).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=B, P=Q+ 1, Q=C, R= 1, 
          new13(A,B,C,M,S), new15(A,B,P,D,E,F,G,H,I,J,K,L).
new13(A,B,C,D,E) :- F>= 1, F=D, G= 0, new18(A,B,C,D,E).
new13(A,B,C,D,E) :- F+ 1=< 0, F=D, G= 0, new18(A,B,C,D,E).
new12(A,B,C,D,E) :- F= 0, F=D, G= 0, new19(A,B,C,D,E).
new11(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=C, new12(A,B,C,G,J).
new11(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new13(A,B,C,M,P), 
          new14(A,B,C,D,E,F,G,H,I,J,K,L).
new11(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=C, new12(A,B,C,G,J).
new11(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new13(A,B,C,M,P), 
          new14(A,B,C,D,E,F,G,H,I,J,K,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- new11(A,B,C,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N, M=C, N=E, new10(A,B,C,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=C, N=E, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=C, N=E, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=F, N= 0, 
          new10(A,B,C,D,E,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- new8(A,B,C,D,E,M,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- new7(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=B, N=A, O= 0, P=Q- 2, Q=B, R= 2, 
          new6(A,B,C,O,P,F,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=A, N= 0, 
          new5(A,B,C,D,E,F,G,H,I,J,K,L).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 2, M=B, N= 1, 
          new3(A,B,C,D,E,F,G,H,I,J,K,L).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,H,I,D,E,F,J,K,L).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
