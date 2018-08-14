new274(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J>= 1, J=K-L, K=C, L=A, M= 0.
new274(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J+ 1=< 0, J=K-L, K=C, L=A, M= 0.
new254(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J>= 1, J=K-L, K=D, L=B, M= 0.
new254(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J+ 1=< 0, J=K-L, K=D, L=B, M= 0.
new253(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=C, U=A, V= 0, 
          new254(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new234(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J>= 1, J=K-L, K=E, L= 2*M, N= 2, 
          M=B, O= 0.
new234(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J+ 1=< 0, J=K-L, K=E, L= 2*M, 
          N= 2, M=B, O= 0.
new233(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=D, U=B, V= 0, 
          new234(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new232(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=C, U=A, V= 0, 
          new233(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new214(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J+ 1=< 0, J=K-L, K= 2*M, N= 2, 
          M=B, L=A, O= 0.
new213(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=E, U= 2*V, W= 2, 
          V=B, X= 0, new214(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new212(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=D, U=B, V= 0, 
          new213(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new211(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=C, U=A, V= 0, 
          new212(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new194(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J>= 1, J=K- 1, K=L-M, L= 2*N, 
          O= 2, N=B, M=A, P= 1, Q= 0.
new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T-U, T= 2*V, W= 2, V=B, 
          U=A, X= 0, new194(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new192(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=E, U= 2*V, W= 2, 
          V=B, X= 0, new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new191(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=D, U=B, V= 0, 
          new192(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new190(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=C, U=A, V= 0, 
          new191(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new189(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=C, T= 2, U=V- 1, V=B, 
          W= 1, X=Y- 2, Y= 2*Z, A1= 2, Z=B, B1= 2, C1=D1- 1, D1=B, E1= 1, 
          new174(A,C1,C,U,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new188(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 3, S=C, T= 3, U=B, V= 2*W, 
          X= 2, W=B, new174(A,B,C,U,V,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 1, S=B, T= 1, U=V- 1, V=C, 
          W= 1, new188(A,B,U,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=B, T= 2, 
          new189(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new185(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=F, T= 0, 
          new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new185(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=F, T= 0, 
          new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new185(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=F, T= 0, 
          new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new184(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new185(A,B,C,D,E,S,G,H,I,J,K,L,M,N,O,P,Q,R).
new183(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T-U, T=E, U=C, V= 0, 
          W=E, X= 2*Y, Z= 2, Y=E, new174(A,B,C,W,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=G, T= 0, 
          new183(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=G, T= 0, 
          new183(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=G, T= 0, 
          new184(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new181(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new182(A,B,C,D,E,F,S,H,I,J,K,L,M,N,O,P,Q,R).
new180(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J+ 1=< 0, J=K- 1, K=L-M, L=C, 
          M=E, N= 1, O= 0.
new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T- 1, T=U-V, U=C, V=E, 
          W= 1, X= 0, Y=Z+ 1, Z=E, A1= 1, B1=C1+ 2, C1= 2*D1, E1= 2, D1=E, 
          F1= 2, new174(A,B,C,Y,B1,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=H, T= 0, 
          new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=H, T= 0, 
          new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=H, T= 0, 
          new181(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new177(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new179(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P,Q,R).
new176(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=I, T= 0, 
          new177(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new176(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=I, T= 0, 
          new177(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new175(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new176(A,B,C,D,E,F,G,H,S,J,K,L,M,N,O,P,Q,R).
new174(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new175(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T- 1, T=U-V, U= 2*W, 
          X= 2, W=B, V=A, Y= 1, Z= 0, 
          new174(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new172(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T-U, T= 2*V, W= 2, V=B, 
          U=A, X= 0, new173(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new171(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=E, U= 2*V, W= 2, 
          V=B, X= 0, new172(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new170(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=D, U=B, V= 0, 
          new171(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new169(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=C, U=A, V= 0, 
          new170(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new168(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=C, T= 2, U=V- 1, V=B, 
          W= 1, X=Y- 2, Y= 2*Z, A1= 2, Z=B, B1= 2, C1=D1- 1, D1=B, E1= 1, 
          new153(A,C1,C,U,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new167(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 3, S=C, T= 3, U=B, V= 2*W, 
          X= 2, W=B, new153(A,B,C,U,V,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new166(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 1, S=B, T= 1, U=V- 1, V=C, 
          W= 1, new167(A,B,U,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new165(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=B, T= 2, 
          new168(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new164(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=F, T= 0, 
          new165(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new164(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=F, T= 0, 
          new165(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new164(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=F, T= 0, 
          new166(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new163(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new164(A,B,C,D,E,S,G,H,I,J,K,L,M,N,O,P,Q,R).
new162(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J>= 1, J=K-L, K=E, L=C, M= 0.
new162(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T-U, T=E, U=C, V= 0, 
          W=E, X= 2*Y, Z= 2, Y=E, new153(A,B,C,W,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new161(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=G, T= 0, 
          new162(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new161(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=G, T= 0, 
          new162(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new161(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=G, T= 0, 
          new163(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new160(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new161(A,B,C,D,E,F,S,H,I,J,K,L,M,N,O,P,Q,R).
new159(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T- 1, T=U-V, U=C, V=E, 
          W= 1, X= 0, Y=Z+ 1, Z=E, A1= 1, B1=C1+ 2, C1= 2*D1, E1= 2, D1=E, 
          F1= 2, new153(A,B,C,Y,B1,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new158(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=H, T= 0, 
          new159(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new158(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=H, T= 0, 
          new159(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new158(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=H, T= 0, 
          new160(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new158(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P,Q,R).
new155(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=I, T= 0, 
          new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new155(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=I, T= 0, 
          new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new154(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new155(A,B,C,D,E,F,G,H,S,J,K,L,M,N,O,P,Q,R).
new153(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new154(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new152(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T- 1, T=U-V, U= 2*W, 
          X= 2, W=B, V=A, Y= 1, Z= 0, 
          new153(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new151(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T-U, T= 2*V, W= 2, V=B, 
          U=A, X= 0, new152(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new150(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=E, U= 2*V, W= 2, 
          V=B, X= 0, new151(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new149(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=D, U=B, V= 0, 
          new150(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=C, U=A, V= 0, 
          new149(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new147(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=C, T= 2, U=V- 1, V=B, 
          W= 1, X=Y- 2, Y= 2*Z, A1= 2, Z=B, B1= 2, C1=D1- 1, D1=B, E1= 1, 
          new132(A,C1,C,U,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new146(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 3, S=C, T= 3, U=B, V= 2*W, 
          X= 2, W=B, new132(A,B,C,U,V,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new145(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 1, S=B, T= 1, U=V- 1, V=C, 
          W= 1, new146(A,B,U,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new144(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J+ 1=< 2, J=B, K= 2.
new144(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=B, T= 2, 
          new147(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new143(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=F, T= 0, 
          new144(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new143(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=F, T= 0, 
          new144(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new143(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=F, T= 0, 
          new145(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new143(A,B,C,D,E,S,G,H,I,J,K,L,M,N,O,P,Q,R).
new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T-U, T=E, U=C, V= 0, 
          W=E, X= 2*Y, Z= 2, Y=E, new132(A,B,C,W,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new140(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=G, T= 0, 
          new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new140(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=G, T= 0, 
          new141(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new140(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=G, T= 0, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new139(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new140(A,B,C,D,E,F,S,H,I,J,K,L,M,N,O,P,Q,R).
new138(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T- 1, T=U-V, U=C, V=E, 
          W= 1, X= 0, Y=Z+ 1, Z=E, A1= 1, B1=C1+ 2, C1= 2*D1, E1= 2, D1=E, 
          F1= 2, new132(A,B,C,Y,B1,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new137(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=H, T= 0, 
          new138(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new137(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=H, T= 0, 
          new138(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new137(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=H, T= 0, 
          new139(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new137(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P,Q,R).
new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=I, T= 0, 
          new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=I, T= 0, 
          new135(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new133(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new134(A,B,C,D,E,F,G,H,S,J,K,L,M,N,O,P,Q,R).
new132(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new133(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new131(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T- 1, T=U-V, U= 2*W, 
          X= 2, W=B, V=A, Y= 1, Z= 0, 
          new132(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new130(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T-U, T= 2*V, W= 2, V=B, 
          U=A, X= 0, new131(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=E, U= 2*V, W= 2, 
          V=B, X= 0, new130(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new128(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=D, U=B, V= 0, 
          new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new127(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=C, U=A, V= 0, 
          new128(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new126(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J+ 1=< 2, J=C, K= 2.
new126(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=C, T= 2, U=V- 1, V=B, 
          W= 1, X=Y- 2, Y= 2*Z, A1= 2, Z=B, B1= 2, C1=D1- 1, D1=B, E1= 1, 
          new111(A,C1,C,U,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new125(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 3, S=C, T= 3, U=B, V= 2*W, 
          X= 2, W=B, new111(A,B,C,U,V,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new124(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 1, S=B, T= 1, U=V- 1, V=C, 
          W= 1, new125(A,B,U,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new123(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=B, T= 2, 
          new126(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=F, T= 0, 
          new123(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=F, T= 0, 
          new123(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=F, T= 0, 
          new124(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new121(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new122(A,B,C,D,E,S,G,H,I,J,K,L,M,N,O,P,Q,R).
new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T-U, T=E, U=C, V= 0, 
          W=E, X= 2*Y, Z= 2, Y=E, new111(A,B,C,W,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new119(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=G, T= 0, 
          new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new119(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=G, T= 0, 
          new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new119(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=G, T= 0, 
          new121(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new119(A,B,C,D,E,F,S,H,I,J,K,L,M,N,O,P,Q,R).
new117(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T- 1, T=U-V, U=C, V=E, 
          W= 1, X= 0, Y=Z+ 1, Z=E, A1= 1, B1=C1+ 2, C1= 2*D1, E1= 2, D1=E, 
          F1= 2, new111(A,B,C,Y,B1,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=H, T= 0, 
          new117(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=H, T= 0, 
          new117(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=H, T= 0, 
          new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new114(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new116(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P,Q,R).
new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=I, T= 0, 
          new114(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=I, T= 0, 
          new114(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new112(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new113(A,B,C,D,E,F,G,H,S,J,K,L,M,N,O,P,Q,R).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new112(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new110(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T- 1, T=U-V, U= 2*W, 
          X= 2, W=B, V=A, Y= 1, Z= 0, 
          new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new109(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T-U, T= 2*V, W= 2, V=B, 
          U=A, X= 0, new110(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new108(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=E, U= 2*V, W= 2, 
          V=B, X= 0, new109(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new107(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=D, U=B, V= 0, 
          new108(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=C, U=A, V= 0, 
          new107(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=C, T= 2, U=V- 1, V=B, 
          W= 1, X=Y- 2, Y= 2*Z, A1= 2, Z=B, B1= 2, C1=D1- 1, D1=B, E1= 1, 
          new90(A,C1,C,U,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new104(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 3, S=C, T= 3, U=B, V= 2*W, 
          X= 2, W=B, new90(A,B,C,U,V,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new103(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J>= 2, J=B, K= 1.
new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 1, S=B, T= 1, U=V- 1, V=C, 
          W= 1, new104(A,B,U,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new102(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=B, T= 2, 
          new105(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new101(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=F, T= 0, 
          new102(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new101(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=F, T= 0, 
          new102(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new101(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=F, T= 0, 
          new103(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new101(A,B,C,D,E,S,G,H,I,J,K,L,M,N,O,P,Q,R).
new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T-U, T=E, U=C, V= 0, 
          W=E, X= 2*Y, Z= 2, Y=E, new90(A,B,C,W,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=G, T= 0, 
          new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=G, T= 0, 
          new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=G, T= 0, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new98(A,B,C,D,E,F,S,H,I,J,K,L,M,N,O,P,Q,R).
new96(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T- 1, T=U-V, U=C, V=E, 
          W= 1, X= 0, Y=Z+ 1, Z=E, A1= 1, B1=C1+ 2, C1= 2*D1, E1= 2, D1=E, 
          F1= 2, new90(A,B,C,Y,B1,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new95(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=H, T= 0, 
          new96(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new95(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=H, T= 0, 
          new96(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new95(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=H, T= 0, 
          new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new95(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P,Q,R).
new92(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=I, T= 0, 
          new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new92(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=I, T= 0, 
          new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new92(A,B,C,D,E,F,G,H,S,J,K,L,M,N,O,P,Q,R).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T- 1, T=U-V, U= 2*W, 
          X= 2, W=B, V=A, Y= 1, Z= 0, 
          new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T-U, T= 2*V, W= 2, V=B, 
          U=A, X= 0, new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=E, U= 2*V, W= 2, 
          V=B, X= 0, new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=D, U=B, V= 0, 
          new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=C, U=A, V= 0, 
          new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=C, T= 2, U=V- 1, V=B, 
          W= 1, X=Y- 2, Y= 2*Z, A1= 2, Z=B, B1= 2, C1=D1- 1, D1=B, E1= 1, 
          new69(A,C1,C,U,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new83(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J+ 1=< 3, J=C, K= 3.
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 3, S=C, T= 3, U=B, V= 2*W, 
          X= 2, W=B, new69(A,B,C,U,V,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 1, S=B, T= 1, U=V- 1, V=C, 
          W= 1, new83(A,B,U,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=B, T= 2, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=F, T= 0, 
          new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=F, T= 0, 
          new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=F, T= 0, 
          new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new80(A,B,C,D,E,S,G,H,I,J,K,L,M,N,O,P,Q,R).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T-U, T=E, U=C, V= 0, 
          W=E, X= 2*Y, Z= 2, Y=E, new69(A,B,C,W,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=G, T= 0, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=G, T= 0, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=G, T= 0, 
          new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new77(A,B,C,D,E,F,S,H,I,J,K,L,M,N,O,P,Q,R).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T- 1, T=U-V, U=C, V=E, 
          W= 1, X= 0, Y=Z+ 1, Z=E, A1= 1, B1=C1+ 2, C1= 2*D1, E1= 2, D1=E, 
          F1= 2, new69(A,B,C,Y,B1,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=H, T= 0, 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=H, T= 0, 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=H, T= 0, 
          new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new74(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P,Q,R).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=I, T= 0, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=I, T= 0, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new71(A,B,C,D,E,F,G,H,S,J,K,L,M,N,O,P,Q,R).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T- 1, T=U-V, U= 2*W, 
          X= 2, W=B, V=A, Y= 1, Z= 0, 
          new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T-U, T= 2*V, W= 2, V=B, 
          U=A, X= 0, new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=E, U= 2*V, W= 2, 
          V=B, X= 0, new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=D, U=B, V= 0, 
          new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=C, U=A, V= 0, 
          new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=C, T= 2, U=V- 1, V=B, 
          W= 1, X=Y- 2, Y= 2*Z, A1= 2, Z=B, B1= 2, C1=D1- 1, D1=B, E1= 1, 
          new46(A,C1,C,U,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 3, S=C, T= 3, U=B, V= 2*W, 
          X= 2, W=B, new46(A,B,C,U,V,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 1, S=B, T= 1, U=V- 1, V=C, 
          W= 1, new62(A,B,U,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=B, T= 2, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=F, T= 0, 
          new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=F, T= 0, 
          new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=F, T= 0, 
          new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new59(A,B,C,D,E,S,G,H,I,J,K,L,M,N,O,P,Q,R).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T-U, T=E, U=C, V= 0, 
          W=E, X= 2*Y, Z= 2, Y=E, new46(A,B,C,W,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=G, T= 0, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=G, T= 0, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=G, T= 0, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new56(A,B,C,D,E,F,S,H,I,J,K,L,M,N,O,P,Q,R).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T- 1, T=U-V, U=C, V=E, 
          W= 1, X= 0, Y=Z+ 1, Z=E, A1= 1, B1=C1+ 2, C1= 2*D1, E1= 2, D1=E, 
          F1= 2, new46(A,B,C,Y,B1,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=H, T= 0, 
          new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=H, T= 0, 
          new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=H, T= 0, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new51(A,B,C,D,E,F,F) :- G>= 1, G=F, H= 0.
new51(A,B,C,D,E,F,F) :- G+ 1=< 0, G=F, H= 0.
new50(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1, K>= 0, K=L+ 1, L=M+N, 
          M= -2*O, P= -2, O=B, N=C, Q= 1, R= 0, new51(A,B,C,D,E,J,S).
new50(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0, K+ 1=< 0, K=L+ 1, L=M+N, 
          M= -2*O, P= -2, O=B, N=C, Q= 1, R= 0, new51(A,B,C,D,E,J,S).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new53(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P,Q,R).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=I, T= 0, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=I, T= 0, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=I, T= 0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new48(A,B,C,D,E,F,G,H,S,J,K,L,M,N,O,P,Q,R).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T- 1, T=U-V, U= 2*W, 
          X= 2, W=B, V=A, Y= 1, Z= 0, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T-U, T= 2*V, W= 2, V=B, 
          U=A, X= 0, new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=E, U= 2*V, W= 2, 
          V=B, X= 0, new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=D, U=B, V= 0, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=C, U=A, V= 0, 
          new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new106(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new127(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new169(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new190(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new211(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new232(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new253(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new274(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new28(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J+ 1=< 2, J=A, K= 2.
new26(A,B,C,D,E,F,G,H,I,J) :- new28(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J) :- new29(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J) :- new30(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J) :- new31(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J) :- new32(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J) :- new33(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J) :- new34(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J) :- new35(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J) :- new36(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J) :- new37(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J) :- new38(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J) :- new39(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J) :- new40(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
safe :- init(A,B,C,D,E), new26(A,B,C,D,E,F,G,H,I,J).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=C, T= 2, U=V- 1, V=B, 
          W= 1, X=Y- 2, Y= 2*Z, A1= 2, Z=B, B1= 2, C1=D1- 1, D1=B, E1= 1, 
          new8(A,C1,C,U,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 3, S=C, T= 3, U=B, V= 2*W, 
          X= 2, W=B, new8(A,B,C,U,V,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 1, S=B, T= 1, U=V- 1, V=C, 
          W= 1, new24(A,B,U,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=B, T= 2, 
          new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=F, T= 0, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=F, T= 0, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=F, T= 0, 
          new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new21(A,B,C,D,E,S,G,H,I,J,K,L,M,N,O,P,Q,R).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T-U, T=E, U=C, V= 0, 
          W=E, X= 2*Y, Z= 2, Y=E, new8(A,B,C,W,X,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=G, T= 0, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=G, T= 0, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=G, T= 0, 
          new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new18(A,B,C,D,E,F,S,H,I,J,K,L,M,N,O,P,Q,R).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T- 1, T=U-V, U=C, V=E, 
          W= 1, X= 0, Y=Z+ 1, Z=E, A1= 1, B1=C1+ 2, C1= 2*D1, E1= 2, D1=E, 
          F1= 2, new8(A,B,C,Y,B1,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=H, T= 0, 
          new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=H, T= 0, 
          new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=H, T= 0, 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new14(A,B,C,D,E,F,F).
new13(A,B,C,D,E,F,G) :- H= 0, H=F, I= 0, new14(A,B,C,D,E,F,G).
new12(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1, K>= 0, K=L+ 1, L=M+N, 
          M= -2*O, P= -2, O=B, N=C, Q= 1, R= 0, new13(A,B,C,D,E,J,S).
new12(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0, K+ 1=< 0, K=L+ 1, L=M+N, 
          M= -2*O, P= -2, O=B, N=C, Q= 1, R= 0, new13(A,B,C,D,E,J,S).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new15(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P,Q,R).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=I, T= 0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=I, T= 0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=I, T= 0, 
          new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new10(A,B,C,D,E,F,G,H,S,J,K,L,M,N,O,P,Q,R).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 0, S=T- 1, T=U-V, U= 2*W, 
          X= 2, W=B, V=A, Y= 1, Z= 0, new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 0, S=T-U, T= 2*V, W= 2, V=B, 
          U=A, X= 0, new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=E, U= 2*V, W= 2, 
          V=B, X= 0, new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=D, U=B, V= 0, 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=T-U, T=C, U=A, V= 0, 
          new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=A, T= 2, 
          new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new1(A,B,C,D,E,F,G,H,I,J) :- new2(A,B,C,D,E,K,L,M,N,F,G,H,I,J,O,P,Q,R).
init(A,B,C,D,E).
false :- init(A,B,C,D,E), new1(A,B,C,D,E,F,G,H,I,J).
spec :- false.
spec :- safe.
