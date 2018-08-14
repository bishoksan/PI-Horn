new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new34(A,B,C,D,E,F,G,H,I,J,K,C1,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new50(A,B,C,D,E,F,G,H,I,C1,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=L, D1= 0, E1=F1-G1, F1=F, F>= 0, G1=B, B>= 0, 
          new47(A,B,C,D,E,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1+ 1=< 0, 
          C1=L, D1= 0, E1=F1-G1, F1=F, F>= 0, G1=B, B>= 0, 
          new47(A,B,C,D,E,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1= 0, 
          C1=L, D1= 0, E1=F1-G1, F1=H, H>= 0, G1=C, C>= 0, 
          new47(A,B,C,D,E,F,G,E1,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=J, D1= 0, E1=F1-G1, F1=D, D>= 0, G1=A, A>= 0, 
          new47(A,B,C,E1,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1+ 1=< 0, 
          C1=J, D1= 0, E1=F1-G1, F1=D, D>= 0, G1=A, A>= 0, 
          new47(A,B,C,E1,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1= 0, 
          C1=J, D1= 0, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=H, H>= 0, D1= 0, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=< 0, 
          C1=H, H>= 0, D1= 0, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1= 0, 
          C1=H, H>= 0, D1= 0, E1= 1, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=H, H>= 0, D1= 0, E1= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1+ 1=< 0, 
          C1=H, H>= 0, D1= 0, E1= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new43(A,B,C,D,D).
new41(A,B,C,D,E) :- F>= 1, F=D, G= 0, new43(A,B,C,D,E).
new41(A,B,C,D,E) :- F+ 1=< 0, F=D, G= 0, new43(A,B,C,D,E).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=N, 
          new41(A,B,C,O,P).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1= 0, 
          C1=F, F>= 0, D1= 0, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=F, F>= 0, D1= 0, E1= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1+ 1=< 0, 
          C1=F, F>= 0, D1= 0, E1= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1= 0, 
          C1=D, D>= 0, D1= 0, 
          new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=D, D>= 0, D1= 0, E1= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1+ 1=< 0, 
          C1=D, D>= 0, D1= 0, E1= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=F, F>= 0, D1= 0, 
          new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=< 0, 
          C1=F, F>= 0, D1= 0, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=D, D>= 0, D1= 0, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=< 0, 
          C1=D, D>= 0, D1= 0, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=D1, 
          new34(A,B,C,D,E,F,G,H,I,J,K,C1,D1,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=D1, 
          new33(A,B,C,D,E,F,G,H,I,C1,D1,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=D1, 
          D1>= 0, E1=C1, C1>= 0, 
          new32(A,B,C,D,E,F,G,E1,C1,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=D1, 
          D1>= 0, E1=C1, C1>= 0, 
          new31(A,B,C,D,E,E1,C1,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=D1, 
          D1>= 0, E1=C1, C1>= 0, 
          new30(A,B,C,E1,C1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new26(A,B,C,D,E,F) :- 
          new28(A,B,C,G,H,I,J,K,L,M,N,O,P,Q,D,E,F,R,S,T,U,V,W,X,Y,Z,A1,B1).
safe :- init(A,B,C), new26(A,B,C,D,E,F).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new8(A,B,C,D,E,F,G,H,I,J,K,C1,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new25(A,B,C,D,E,F,G,H,I,C1,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=L, D1= 0, E1=F1-G1, F1=F, F>= 0, G1=B, B>= 0, 
          new22(A,B,C,D,E,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1+ 1=< 0, 
          C1=L, D1= 0, E1=F1-G1, F1=F, F>= 0, G1=B, B>= 0, 
          new22(A,B,C,D,E,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1= 0, 
          C1=L, D1= 0, E1=F1-G1, F1=H, H>= 0, G1=C, C>= 0, 
          new22(A,B,C,D,E,F,G,E1,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=J, D1= 0, E1=F1-G1, F1=D, D>= 0, G1=A, A>= 0, 
          new22(A,B,C,E1,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1+ 1=< 0, 
          C1=J, D1= 0, E1=F1-G1, F1=D, D>= 0, G1=A, A>= 0, 
          new22(A,B,C,E1,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1= 0, 
          C1=J, D1= 0, 
          new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=H, H>= 0, D1= 0, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=< 0, 
          C1=H, H>= 0, D1= 0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1= 0, 
          C1=H, H>= 0, D1= 0, E1= 1, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=H, H>= 0, D1= 0, E1= 0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1+ 1=< 0, 
          C1=H, H>= 0, D1= 0, E1= 0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new18(A,B,C).
new16(A,B,C,D,D) :- new18(A,B,C).
new15(A,B,C,D,E) :- F= 0, F=D, G= 0, new16(A,B,C,D,E).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=N, 
          new15(A,B,C,O,P).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1= 0, 
          C1=F, F>= 0, D1= 0, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=F, F>= 0, D1= 0, E1= 0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1+ 1=< 0, 
          C1=F, F>= 0, D1= 0, E1= 0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1= 0, 
          C1=D, D>= 0, D1= 0, 
          new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=D, D>= 0, D1= 0, E1= 0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1+ 1=< 0, 
          C1=D, D>= 0, D1= 0, E1= 0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,E1,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=F, F>= 0, D1= 0, 
          new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=< 0, 
          C1=F, F>= 0, D1= 0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1>= 1, 
          C1=D, D>= 0, D1= 0, 
          new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=< 0, 
          C1=D, D>= 0, D1= 0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=D1, 
          new8(A,B,C,D,E,F,G,H,I,J,K,C1,D1,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=D1, 
          new7(A,B,C,D,E,F,G,H,I,C1,D1,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=D1, 
          D1>= 0, E1=C1, C1>= 0, 
          new6(A,B,C,D,E,F,G,E1,C1,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=D1, 
          D1>= 0, E1=C1, C1>= 0, 
          new5(A,B,C,D,E,E1,C1,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- C1=D1, 
          D1>= 0, E1=C1, C1>= 0, 
          new4(A,B,C,E1,C1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :- 
          new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1).
new1(A,B,C,D,E,F) :- 
          new2(A,B,C,G,H,I,J,K,L,M,N,O,P,Q,D,E,F,R,S,T,U,V,W,X,Y,Z,A1,B1).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
