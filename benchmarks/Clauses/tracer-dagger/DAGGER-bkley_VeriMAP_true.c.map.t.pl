new129(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H>= 1, H=C, I= 0.
new129(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H+ 1=< 0, H=C, I= 0.
new114(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H>= 1, H=B, I= 0.
new114(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H+ 1=< 0, H=B, I= 0.
new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=C, P= 0, 
          new114(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new99(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H+ 1=< 1, H=A, I= 1.
new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=B, P= 0, 
          new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=C, P= 0, 
          new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new96(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, Q= 0, R= 0, S= 1, 
          T=U- 1, U=V+ 0, V=W+ 1, W=X+ 0, X=A, Y= 0, Z= 1, A1= 0, B1= 1, 
          new84(T,Q,R,S,E,F,G,H,I,J,K,L,M,N).
new95(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=P+Q, P=C, Q=B, R= 1, S=T- 1, 
          T=U+V, U=W+X, W=A, X=B, V=C, Y= 1, Z=A1+ 1, A1=D, B1= 1, C1= 0, 
          D1= 0, new84(S,C1,D1,Z,E,F,G,H,I,J,K,L,M,N).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=E, P= 0, 
          new95(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=E, P= 0, 
          new95(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=E, P= 0, 
          new96(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new94(A,B,C,D,O,F,G,H,I,J,K,L,M,N).
new92(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H+ 1=< 1, H=A, I= 1.
new92(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, Q=R+S, R=C, S=D, T= 0, 
          U=V- 1, V=A, W= 1, X=Y+ 1, Y=B, Z= 1, 
          new84(U,X,Q,T,E,F,G,H,I,J,K,L,M,N).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=F, P= 0, 
          new92(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=F, P= 0, 
          new92(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=F, P= 0, 
          new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new91(A,B,C,D,E,O,G,H,I,J,K,L,M,N).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=G, P= 0, 
          new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=G, P= 0, 
          new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new86(A,B,C,D,E,F,O,H,I,J,K,L,M,N).
new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=B, P= 0, 
          new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=C, P= 0, 
          new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, Q= 0, R= 0, S= 1, 
          T=U- 1, U=V+ 0, V=W+ 1, W=X+ 0, X=A, Y= 0, Z= 1, A1= 0, B1= 1, 
          new68(T,Q,R,S,E,F,G,H,I,J,K,L,M,N).
new79(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H+ 1=< 1, H=I+J, I=C, J=B, K= 1.
new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=P+Q, P=C, Q=B, R= 1, S=T- 1, 
          T=U+V, U=W+X, W=A, X=B, V=C, Y= 1, Z=A1+ 1, A1=D, B1= 1, C1= 0, 
          D1= 0, new68(S,C1,D1,Z,E,F,G,H,I,J,K,L,M,N).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=E, P= 0, 
          new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=E, P= 0, 
          new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=E, P= 0, 
          new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new78(A,B,C,D,O,F,G,H,I,J,K,L,M,N).
new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, Q=R+S, R=C, S=D, T= 0, 
          U=V- 1, V=A, W= 1, X=Y+ 1, Y=B, Z= 1, 
          new68(U,X,Q,T,E,F,G,H,I,J,K,L,M,N).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=F, P= 0, 
          new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=F, P= 0, 
          new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=F, P= 0, 
          new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new75(A,B,C,D,E,O,G,H,I,J,K,L,M,N).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=G, P= 0, 
          new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=G, P= 0, 
          new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new70(A,B,C,D,E,F,O,H,I,J,K,L,M,N).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=B, P= 0, 
          new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=C, P= 0, 
          new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new64(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H+ 1=< 1, H=A, I= 1.
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, Q= 0, R= 0, S= 1, 
          T=U- 1, U=V+ 0, V=W+ 1, W=X+ 0, X=A, Y= 0, Z= 1, A1= 0, B1= 1, 
          new52(T,Q,R,S,E,F,G,H,I,J,K,L,M,N).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=P+Q, P=C, Q=B, R= 1, S=T- 1, 
          T=U+V, U=W+X, W=A, X=B, V=C, Y= 1, Z=A1+ 1, A1=D, B1= 1, C1= 0, 
          D1= 0, new52(S,C1,D1,Z,E,F,G,H,I,J,K,L,M,N).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=E, P= 0, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=E, P= 0, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=E, P= 0, 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new62(A,B,C,D,O,F,G,H,I,J,K,L,M,N).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, Q=R+S, R=C, S=D, T= 0, 
          U=V- 1, V=A, W= 1, X=Y+ 1, Y=B, Z= 1, 
          new52(U,X,Q,T,E,F,G,H,I,J,K,L,M,N).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=F, P= 0, 
          new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=F, P= 0, 
          new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=F, P= 0, 
          new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new59(A,B,C,D,E,O,G,H,I,J,K,L,M,N).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=G, P= 0, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=G, P= 0, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new54(A,B,C,D,E,F,O,H,I,J,K,L,M,N).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=B, P= 0, 
          new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=C, P= 0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, Q= 0, R= 0, S= 1, 
          T=U- 1, U=V+ 0, V=W+ 1, W=X+ 0, X=A, Y= 0, Z= 1, A1= 0, B1= 1, 
          new36(T,Q,R,S,E,F,G,H,I,J,K,L,M,N).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=P+Q, P=C, Q=B, R= 1, S=T- 1, 
          T=U+V, U=W+X, W=A, X=B, V=C, Y= 1, Z=A1+ 1, A1=D, B1= 1, C1= 0, 
          D1= 0, new36(S,C1,D1,Z,E,F,G,H,I,J,K,L,M,N).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=E, P= 0, 
          new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=E, P= 0, 
          new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=E, P= 0, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new46(A,B,C,D,O,F,G,H,I,J,K,L,M,N).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, Q=R+S, R=C, S=D, T= 0, 
          U=V- 1, V=A, W= 1, X=Y+ 1, Y=B, Z= 1, 
          new36(U,X,Q,T,E,F,G,H,I,J,K,L,M,N).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=F, P= 0, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=F, P= 0, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=F, P= 0, 
          new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new42(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 1, I>= 1, I=J+K, J=L+M, L=N+O, N=A, 
          O=B, M=D, K=C, P= 1, new12(A,B,C,D,H,Q).
new42(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 0, I+ 1=< 1, I=J+K, J=L+M, L=N+O, N=A, 
          O=B, M=D, K=C, P= 1, new12(A,B,C,D,H,Q).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 1, P>= 0, P=B, Q= 0, 
          new12(A,B,C,D,O,R), new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, P+ 1=< 0, P=B, Q= 0, 
          new12(A,B,C,D,O,R), new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 1, P>= 0, P=D, Q= 0, 
          new12(A,B,C,D,O,R), new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, P+ 1=< 0, P=D, Q= 0, 
          new12(A,B,C,D,O,R), new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new43(A,B,C,D,E,O,G,H,I,J,K,L,M,N).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=G, P= 0, 
          new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=G, P= 0, 
          new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=G, P= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new38(A,B,C,D,E,F,O,H,I,J,K,L,M,N).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=B, P= 0, 
          new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=C, P= 0, 
          new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=D, P= 0, 
          new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=D, P= 0, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=D, P= 0, 
          new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=D, P= 0, 
          new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=D, P= 0, 
          new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=D, P= 0, 
          new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=D, P= 0, 
          new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new25(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H>= 1, H=D, I= 0.
new25(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H+ 1=< 0, H=D, I= 0.
new23(A,B,C,D,E,F,G,H) :- new25(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new23(A,B,C,D,E,F,G,H) :- new26(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new23(A,B,C,D,E,F,G,H) :- new27(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new23(A,B,C,D,E,F,G,H) :- new28(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new23(A,B,C,D,E,F,G,H) :- new29(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new23(A,B,C,D,E,F,G,H) :- new30(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new23(A,B,C,D,E,F,G,H) :- new31(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
new23(A,B,C,D,E,F,G,H) :- new32(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
safe :- init(A,B,C,D), new23(A,B,C,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, Q= 0, R= 0, S= 1, 
          T=U- 1, U=V+ 0, V=W+ 1, W=X+ 0, X=A, Y= 0, Z= 1, A1= 0, B1= 1, 
          new6(T,Q,R,S,E,F,G,H,I,J,K,L,M,N).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=P+Q, P=C, Q=B, R= 1, S=T- 1, 
          T=U+V, U=W+X, W=A, X=B, V=C, Y= 1, Z=A1+ 1, A1=D, B1= 1, C1= 0, 
          D1= 0, new6(S,C1,D1,Z,E,F,G,H,I,J,K,L,M,N).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=E, P= 0, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=E, P= 0, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=E, P= 0, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new20(A,B,C,D,O,F,G,H,I,J,K,L,M,N).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, Q=R+S, R=C, S=D, T= 0, 
          U=V- 1, V=A, W= 1, X=Y+ 1, Y=B, Z= 1, 
          new6(U,X,Q,T,E,F,G,H,I,J,K,L,M,N).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=F, P= 0, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=F, P= 0, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=F, P= 0, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new16(A,B,C,D,E,E).
new14(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 1, I>= 1, I=J+K, J=L+M, L=N+O, N=A, 
          O=B, M=D, K=C, P= 1, new11(A,B,C,D,H,Q).
new14(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 0, I+ 1=< 1, I=J+K, J=L+M, L=N+O, N=A, 
          O=B, M=D, K=C, P= 1, new11(A,B,C,D,H,Q).
new13(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 1, I>= 0, I=B, J= 0, 
          new11(A,B,C,D,H,K).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 1, P>= 0, P=B, Q= 0, 
          new12(A,B,C,D,O,R), new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new13(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 0, I+ 1=< 0, I=B, J= 0, 
          new11(A,B,C,D,H,K).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, P+ 1=< 0, P=B, Q= 0, 
          new12(A,B,C,D,O,R), new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new12(A,B,C,D,E,E) :- F>= 1, F=E, G= 0.
new12(A,B,C,D,E,E) :- F+ 1=< 0, F=E, G= 0.
new11(A,B,C,D,E,F) :- G= 0, G=E, H= 0, new16(A,B,C,D,E,F).
new10(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 1, I>= 0, I=D, J= 0, 
          new11(A,B,C,D,H,K).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 1, P>= 0, P=D, Q= 0, 
          new12(A,B,C,D,O,R), new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new10(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 0, I+ 1=< 0, I=D, J= 0, 
          new11(A,B,C,D,H,K).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, P+ 1=< 0, P=D, Q= 0, 
          new12(A,B,C,D,O,R), new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new17(A,B,C,D,E,O,G,H,I,J,K,L,M,N).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=G, P= 0, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=G, P= 0, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=G, P= 0, 
          new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new8(A,B,C,D,E,F,O,H,I,J,K,L,M,N).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=A, P= 1, 
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=B, P= 0, 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=C, P= 0, 
          new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=D, P= 0, 
          new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new1(A,B,C,D,E,F,G,H) :- new2(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
init(A,B,C,D).
false :- init(A,B,C,D), new1(A,B,C,D,E,F,G,H).
spec :- false.
spec :- safe.
