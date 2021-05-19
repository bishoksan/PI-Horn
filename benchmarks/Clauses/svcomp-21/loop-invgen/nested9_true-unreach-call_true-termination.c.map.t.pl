new117(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H>= 1000000, H=B, I= 1000000.
new91(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H>= 1000000, H=D, I= 1000000.
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=D, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 1000000, O=B, P= 1000000, 
          new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new76(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :-  -1000000>=H, I= -1000000, H=D.
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 1000000, O=B, P= 1000000, 
          new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new65(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H>= 1000000, H=C, I= 1000000.
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=C, 
          new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 1000000, O=D, P= 1000000, 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=D, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 1000000, O=B, P= 1000000, 
          new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new50(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :-  -1000000>=H, I= -1000000, H=C.
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 1000000, O=D, P= 1000000, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=D, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 1000000, O=B, P= 1000000, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 1, P=<Q, P=R-S, R=G, S=E, Q= 2*T, 
          U= 2, T=B, V=W+ 1, W=G, X= 1, new18(A,B,C,D,O,Y), 
          new44(A,B,C,D,E,F,V,H,I,J,K,L,M,N).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, P>=Q+ 1, P=R-S, R=G, S=E, Q= 2*T, 
          U= 2, T=B, V=W+ 1, W=G, X= 1, new18(A,B,C,D,O,Y), 
          new44(A,B,C,D,E,F,V,H,I,J,K,L,M,N).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=<P, O=G, P=F, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P, O=G, P=F, Q=R+ 1, R=F, S= 1, 
          new42(A,B,C,D,E,Q,G,H,I,J,K,L,M,N).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=<P, O=F, P= 3*Q, R= 3, Q=E, S=E, 
          new44(A,B,C,D,E,F,S,H,I,J,K,L,M,N).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P, O=F, P= 3*Q, R= 3, Q=E, S=T+ 1, 
          T=E, U= 1, new39(A,B,C,D,S,F,G,H,I,J,K,L,M,N).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=<P, O=E, P=B, Q= 2*R, S= 2, R=E, 
          new42(A,B,C,D,E,Q,G,H,I,J,K,L,M,N).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P, O=E, P=B, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new40(A,B,C,D,E,F,G,A,B,C,D,E,F,G).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=<P, O= 3*Q, R= 3, Q=B, P=S+T, S=D, T=C, 
          U= 0, new39(A,B,C,D,U,F,G,H,I,J,K,L,M,N).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P+ 1, O= 3*Q, R= 3, Q=B, P=S+T, S=D, 
          T=C, new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 1000000, O=C, P= 1000000, 
          new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=C, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 1000000, O=D, P= 1000000, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=D, 
          new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 1000000, O=B, P= 1000000, 
          new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=B, 
          new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=B, 
          new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=B, 
          new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=B, 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=B, 
          new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new27(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :-  -1000000>=H, I= -1000000, H=B.
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=B, 
          new117(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new24(A,B,C,D,E,F,G,H) :- new26(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new24(A,B,C,D,E,F,G,H) :- new27(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new24(A,B,C,D,E,F,G,H) :- new28(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new24(A,B,C,D,E,F,G,H) :- new29(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new24(A,B,C,D,E,F,G,H) :- new30(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new24(A,B,C,D,E,F,G,H) :- new31(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new24(A,B,C,D,E,F,G,H) :- new32(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
safe :- init(A,B,C,D), new24(A,B,C,D,E,F,G,H).
new23(A,B,C,D).
new21(A,B,C,D,E,E) :- new23(A,B,C,D).
new20(A,B,C,D,E,E).
new18(A,B,C,D,E,F) :- G>= 1, G=E, H= 0, new20(A,B,C,D,E,F).
new18(A,B,C,D,E,F) :- G+ 1=< 0, G=E, H= 0, new20(A,B,C,D,E,F).
new17(A,B,C,D,E,F) :- G= 0, G=E, H= 0, new21(A,B,C,D,E,F).
new16(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 1, I=<J, I=K-L, K=G, L=E, J= 2*M, 
          N= 2, M=B, new17(A,B,C,D,H,O).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 1, P=<Q, P=R-S, R=G, S=E, Q= 2*T, 
          U= 2, T=B, V=W+ 1, W=G, X= 1, new18(A,B,C,D,O,Y), 
          new14(A,B,C,D,E,F,V,H,I,J,K,L,M,N).
new16(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 0, I>=J+ 1, I=K-L, K=G, L=E, J= 2*M, 
          N= 2, M=B, new17(A,B,C,D,H,O).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, P>=Q+ 1, P=R-S, R=G, S=E, Q= 2*T, 
          U= 2, T=B, V=W+ 1, W=G, X= 1, new18(A,B,C,D,O,Y), 
          new14(A,B,C,D,E,F,V,H,I,J,K,L,M,N).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=<P, O=G, P=F, 
          new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P, O=G, P=F, Q=R+ 1, R=F, S= 1, 
          new12(A,B,C,D,E,Q,G,H,I,J,K,L,M,N).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=<P, O=F, P= 3*Q, R= 3, Q=E, S=E, 
          new14(A,B,C,D,E,F,S,H,I,J,K,L,M,N).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P, O=F, P= 3*Q, R= 3, Q=E, S=T+ 1, 
          T=E, U= 1, new9(A,B,C,D,S,F,G,H,I,J,K,L,M,N).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=<P, O=E, P=B, Q= 2*R, S= 2, R=E, 
          new12(A,B,C,D,E,Q,G,H,I,J,K,L,M,N).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=<P, O= 3*Q, R= 3, Q=B, P=S+T, S=D, T=C, 
          U= 0, new9(A,B,C,D,U,F,G,H,I,J,K,L,M,N).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 1000000, O=C, P= 1000000, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=C, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 1000000, O=D, P= 1000000, 
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=D, 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 1000000, O=B, P= 1000000, 
          new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-  -999999=<O, P= -1000000, O=B, 
          new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new1(A,B,C,D,E,F,G,H) :- new2(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
init(A,B,C,D).
false :- init(A,B,C,D), new1(A,B,C,D,E,F,G,H).
spec :- false.
spec :- safe.
