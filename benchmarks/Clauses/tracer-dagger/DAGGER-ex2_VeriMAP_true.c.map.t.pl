new52(A,B,B) :- C>= 1, C=B, D= 0.
new52(A,B,B) :- C+ 1=< 0, C=B, D= 0.
new51(A,B,C,D,E,F,G,H,I,J,K,L,A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=< 132, N=A, 
          O= 132, new52(A,M,P).
new51(A,B,C,D,E,F,G,H,I,J,K,L,A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>= 133, N=A, 
          O= 132, new52(A,M,P).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=L, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new51(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=L, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new51(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=L, Z= 0, 
          A1=B1+ 2, B1=A, C1= 2, 
          new51(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new50(A,B,C,D,E,F,G,H,I,J,K,Y,M,N,O,P,Q,R,S,T,U,V,W,X).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=K, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new49(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=K, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new49(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=K, Z= 0, 
          A1=B1+ 4, B1=A, C1= 4, 
          new49(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new48(A,B,C,D,E,F,G,H,I,J,Y,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=J, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new47(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=J, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new47(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=J, Z= 0, 
          A1=B1+ 6, B1=A, C1= 6, 
          new47(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new46(A,B,C,D,E,F,G,H,I,Y,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=I, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new45(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=I, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new45(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=I, Z= 0, 
          A1=B1+ 8, B1=A, C1= 8, 
          new45(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new44(A,B,C,D,E,F,G,H,Y,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=H, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new43(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=H, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new43(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=H, Z= 0, 
          A1=B1+ 10, B1=A, C1= 10, 
          new43(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new42(A,B,C,D,E,F,G,Y,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=G, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new41(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=G, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new41(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=G, Z= 0, 
          A1=B1+ 12, B1=A, C1= 12, 
          new41(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new40(A,B,C,D,E,F,Y,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=F, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new39(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=F, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new39(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=F, Z= 0, 
          A1=B1+ 14, B1=A, C1= 14, 
          new39(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new38(A,B,C,D,E,Y,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=E, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new37(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=E, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new37(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=E, Z= 0, 
          A1=B1+ 16, B1=A, C1= 16, 
          new37(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new36(A,B,C,D,Y,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=D, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new35(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=D, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new35(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=D, Z= 0, 
          A1=B1+ 18, B1=A, C1= 18, 
          new35(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new34(A,B,C,Y,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=C, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new33(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=C, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new33(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=C, Z= 0, 
          A1=B1+ 20, B1=A, C1= 20, 
          new33(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new32(A,B,Y,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=B, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new31(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=B, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new31(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=B, Z= 0, 
          A1=B1+ 22, B1=A, C1= 22, 
          new31(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new30(A,Y,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new27(A,B) :- new29(A,C,D,E,F,G,H,I,J,K,L,M,B,N,O,P,Q,R,S,T,U,V,W,X).
safe :- init(A), new27(A,B).
new26(A,B,B).
new25(A,B,C) :- D= 0, D=B, E= 0, new26(A,B,C).
new24(A,B,C,D,E,F,G,H,I,J,K,L,A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N=< 132, N=A, 
          O= 132, new25(A,M,P).
new24(A,B,C,D,E,F,G,H,I,J,K,L,A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>= 133, N=A, 
          O= 132, new25(A,M,P).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=L, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new24(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=L, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new24(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=L, Z= 0, 
          A1=B1+ 2, B1=A, C1= 2, 
          new24(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new23(A,B,C,D,E,F,G,H,I,J,K,Y,M,N,O,P,Q,R,S,T,U,V,W,X).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=K, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new22(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=K, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new22(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=K, Z= 0, 
          A1=B1+ 4, B1=A, C1= 4, 
          new22(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new21(A,B,C,D,E,F,G,H,I,J,Y,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=J, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new20(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=J, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new20(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=J, Z= 0, 
          A1=B1+ 6, B1=A, C1= 6, 
          new20(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new19(A,B,C,D,E,F,G,H,I,Y,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=I, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new18(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=I, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new18(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=I, Z= 0, 
          A1=B1+ 8, B1=A, C1= 8, 
          new18(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new17(A,B,C,D,E,F,G,H,Y,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=H, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new16(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=H, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new16(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=H, Z= 0, 
          A1=B1+ 10, B1=A, C1= 10, 
          new16(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new15(A,B,C,D,E,F,G,Y,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=G, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new14(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=G, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new14(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=G, Z= 0, 
          A1=B1+ 12, B1=A, C1= 12, 
          new14(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new13(A,B,C,D,E,F,Y,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=F, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new12(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=F, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new12(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=F, Z= 0, 
          A1=B1+ 14, B1=A, C1= 14, 
          new12(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new11(A,B,C,D,E,Y,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=E, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new10(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=E, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new10(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=E, Z= 0, 
          A1=B1+ 16, B1=A, C1= 16, 
          new10(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new9(A,B,C,D,Y,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=D, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new8(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=D, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new8(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=D, Z= 0, 
          A1=B1+ 18, B1=A, C1= 18, 
          new8(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new7(A,B,C,Y,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=C, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new6(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=C, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new6(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=C, Z= 0, 
          A1=B1+ 20, B1=A, C1= 20, 
          new6(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new5(A,B,Y,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y>= 1, Y=B, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new4(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y+ 1=< 0, Y=B, Z= 0, 
          A1=B1+ 1, B1=A, C1= 1, 
          new4(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- Y= 0, Y=B, Z= 0, 
          A1=B1+ 22, B1=A, C1= 22, 
          new4(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- 
          new3(A,Y,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X).
new1(A,B) :- new2(A,C,D,E,F,G,H,I,J,K,L,M,B,N,O,P,Q,R,S,T,U,V,W,X).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
