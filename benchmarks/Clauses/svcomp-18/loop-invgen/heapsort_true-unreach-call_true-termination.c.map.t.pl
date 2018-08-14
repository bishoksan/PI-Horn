new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=H, T= 0, U=V+ 1, V=G, 
          W= 1, new53(A,B,C,D,E,F,U,H,I,J,K,L,M,N,O,P,Q,R).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=H, T= 0, U=V+ 1, V=G, 
          W= 1, new53(A,B,C,D,E,F,U,H,I,J,K,L,M,N,O,P,Q,R).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=H, T= 0, 
          new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new66(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P,Q,R).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=V+ 1, V=G, W= 1, 
          U=A, new14(A,B,C,S,X), new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=V+ 1, V=G, W= 1, 
          U=A, new14(A,B,C,S,X), new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=V+ 1, V=G, 
          W= 1, new14(A,B,C,S,X), new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=V+ 1, 
          V=G, W= 1, new14(A,B,C,S,X), 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=G, U=A, 
          new14(A,B,C,S,V), new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=G, U=A, 
          new14(A,B,C,S,V), new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=G, U=A, V=G, 
          W= 2*X, Y= 2, X=G, new14(A,B,C,S,Z), 
          new43(A,B,C,D,E,V,W,H,I,J,K,L,M,N,O,P,Q,R).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=G, U=A, V=G, 
          W= 2*X, Y= 2, X=G, new14(A,B,C,S,Z), 
          new43(A,B,C,D,E,V,W,H,I,J,K,L,M,N,O,P,Q,R).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=G, 
          new14(A,B,C,S,V), new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=G, 
          new14(A,B,C,S,V), new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=F, U=A, 
          new14(A,B,C,S,V), new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=F, U=A, 
          new14(A,B,C,S,V), new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=F, 
          new14(A,B,C,S,V), new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=F, 
          new14(A,B,C,S,V), new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=I, T= 0, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=I, T= 0, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=I, T= 0, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new57(A,B,C,D,E,F,G,H,S,J,K,L,M,N,O,P,Q,R).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=G, U=A, 
          new14(A,B,C,S,V), new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=G, U=A, 
          new14(A,B,C,S,V), new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=G, 
          new14(A,B,C,S,V), new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=G, 
          new14(A,B,C,S,V), new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=G, 
          new14(A,B,C,S,V), new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=G, 
          new14(A,B,C,S,V), new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=B, U=A, V=W- 1, 
          W=B, X= 1, new14(A,B,C,S,Y), 
          new41(A,V,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=B, U=A, V=W- 1, 
          W=B, X= 1, new14(A,B,C,S,Y), 
          new41(A,V,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=C, U=A, V=W- 1, 
          W=C, X= 1, new14(A,B,C,S,Y), 
          new41(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=C, U=A, V=W- 1, 
          W=C, X= 1, new14(A,B,C,S,Y), 
          new41(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=C, 
          new14(A,B,C,S,V), new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=C, 
          new14(A,B,C,S,V), new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=B, 
          new14(A,B,C,S,V), new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=B, 
          new14(A,B,C,S,V), new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=B, T= 1, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 1, S=B, T= 1, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=<T, S=G, T=C, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>=T, S=G, T=C, 
          new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=<T, S=G, T=C, 
          new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>=T+ 1, S=G, T=C, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=C, T= 1, U=B, V= 2*W, 
          X= 2, W=B, new43(A,B,C,D,E,U,V,H,I,J,K,L,M,N,O,P,Q,R).
new42(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J=< 1, J=C, K= 1.
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=B, T= 1, U=V- 1, V=B, 
          W= 1, new40(A,U,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 1, S=B, T= 1, U=V- 1, V=C, 
          W= 1, new40(A,B,U,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new37(A,B,C,D,E,F) :- new39(A,B,C,G,H,I,J,K,L,D,E,F,M,N,O,P,Q,R).
safe :- init(A,B,C), new37(A,B,C,D,E,F).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=H, T= 0, U=V+ 1, V=G, 
          W= 1, new23(A,B,C,D,E,F,U,H,I,J,K,L,M,N,O,P,Q,R).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=H, T= 0, U=V+ 1, V=G, 
          W= 1, new23(A,B,C,D,E,F,U,H,I,J,K,L,M,N,O,P,Q,R).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=H, T= 0, 
          new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new36(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P,Q,R).
new34(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1, K=<L, K=M+ 1, M=G, N= 1, 
          L=A, new13(A,B,C,J,O).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=V+ 1, V=G, W= 1, 
          U=A, new14(A,B,C,S,X), new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new34(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0, K>=L+ 1, K=M+ 1, M=G, N= 1, 
          L=A, new13(A,B,C,J,O).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=V+ 1, V=G, W= 1, 
          U=A, new14(A,B,C,S,X), new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new33(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1,  1=<K, L= 1, K=M+ 1, M=G, 
          N= 1, new13(A,B,C,J,O).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=V+ 1, V=G, 
          W= 1, new14(A,B,C,S,X), new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new33(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0,  1>=K+ 1, L= 1, K=M+ 1, 
          M=G, N= 1, new13(A,B,C,J,O).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=V+ 1, 
          V=G, W= 1, new14(A,B,C,S,X), 
          new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new32(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1, K=<L, K=G, L=A, 
          new13(A,B,C,J,M).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=G, U=A, 
          new14(A,B,C,S,V), new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new32(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0, K>=L+ 1, K=G, L=A, 
          new13(A,B,C,J,M).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=G, U=A, 
          new14(A,B,C,S,V), new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new31(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1, K=<L, K=G, L=A, 
          new13(A,B,C,J,M).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=G, U=A, V=G, 
          W= 2*X, Y= 2, X=G, new14(A,B,C,S,Z), 
          new6(A,B,C,D,E,V,W,H,I,J,K,L,M,N,O,P,Q,R).
new31(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0, K>=L+ 1, K=G, L=A, 
          new13(A,B,C,J,M).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=G, U=A, V=G, 
          W= 2*X, Y= 2, X=G, new14(A,B,C,S,Z), 
          new6(A,B,C,D,E,V,W,H,I,J,K,L,M,N,O,P,Q,R).
new30(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1,  1=<K, L= 1, K=G, 
          new13(A,B,C,J,M).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=G, 
          new14(A,B,C,S,V), new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new30(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0,  1>=K+ 1, L= 1, K=G, 
          new13(A,B,C,J,M).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=G, 
          new14(A,B,C,S,V), new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new29(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1, K=<L, K=F, L=A, 
          new13(A,B,C,J,M).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=F, U=A, 
          new14(A,B,C,S,V), new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new29(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0, K>=L+ 1, K=F, L=A, 
          new13(A,B,C,J,M).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=F, U=A, 
          new14(A,B,C,S,V), new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new28(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1,  1=<K, L= 1, K=F, 
          new13(A,B,C,J,M).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=F, 
          new14(A,B,C,S,V), new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new28(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0,  1>=K+ 1, L= 1, K=F, 
          new13(A,B,C,J,M).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=F, 
          new14(A,B,C,S,V), new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 1, S=I, T= 0, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=< 0, S=I, T= 0, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, S=I, T= 0, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new27(A,B,C,D,E,F,G,H,S,J,K,L,M,N,O,P,Q,R).
new25(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1, K=<L, K=G, L=A, 
          new13(A,B,C,J,M).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=G, U=A, 
          new14(A,B,C,S,V), new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new25(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0, K>=L+ 1, K=G, L=A, 
          new13(A,B,C,J,M).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=G, U=A, 
          new14(A,B,C,S,V), new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new24(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1,  1=<K, L= 1, K=G, 
          new13(A,B,C,J,M).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=G, 
          new14(A,B,C,S,V), new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new24(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0,  1>=K+ 1, L= 1, K=G, 
          new13(A,B,C,J,M).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=G, 
          new14(A,B,C,S,V), new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new22(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1,  1=<K, L= 1, K=G, 
          new13(A,B,C,J,M).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=G, 
          new14(A,B,C,S,V), new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new22(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0,  1>=K+ 1, L= 1, K=G, 
          new13(A,B,C,J,M).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=G, 
          new14(A,B,C,S,V), new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new21(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1, K=<L, K=B, L=A, 
          new13(A,B,C,J,M).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=B, U=A, V=W- 1, 
          W=B, X= 1, new14(A,B,C,S,Y), 
          new4(A,V,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new21(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0, K>=L+ 1, K=B, L=A, 
          new13(A,B,C,J,M).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=B, U=A, V=W- 1, 
          W=B, X= 1, new14(A,B,C,S,Y), 
          new4(A,V,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new20(A,B,C).
new18(A,B,C,D,D) :- new20(A,B,C).
new17(A,B,C,D,D).
new15(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1, K=<L, K=C, L=A, 
          new13(A,B,C,J,M).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1, T=<U, T=C, U=A, V=W- 1, 
          W=C, X= 1, new14(A,B,C,S,Y), 
          new4(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new15(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0, K>=L+ 1, K=C, L=A, 
          new13(A,B,C,J,M).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0, T>=U+ 1, T=C, U=A, V=W- 1, 
          W=C, X= 1, new14(A,B,C,S,Y), 
          new4(A,B,V,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new14(A,B,C,D,E) :- F>= 1, F=D, G= 0, new17(A,B,C,D,E).
new14(A,B,C,D,E) :- F+ 1=< 0, F=D, G= 0, new17(A,B,C,D,E).
new13(A,B,C,D,E) :- F= 0, F=D, G= 0, new18(A,B,C,D,E).
new12(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1,  1=<K, L= 1, K=C, 
          new13(A,B,C,J,M).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=C, 
          new14(A,B,C,S,V), new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new12(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0,  1>=K+ 1, L= 1, K=C, 
          new13(A,B,C,J,M).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=C, 
          new14(A,B,C,S,V), new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new11(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 1,  1=<K, L= 1, K=B, 
          new13(A,B,C,J,M).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 1,  1=<T, U= 1, T=B, 
          new14(A,B,C,S,V), new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new11(A,B,C,D,E,F,G,H,I,A,B,C,D,E,F,G,H,I) :- J= 0,  1>=K+ 1, L= 1, K=B, 
          new13(A,B,C,J,M).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S= 0,  1>=T+ 1, U= 1, T=B, 
          new14(A,B,C,S,V), new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=B, T= 1, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 1, S=B, T= 1, 
          new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S+ 1=<T, S=G, T=C, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>=T, S=G, T=C, 
          new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=<T, S=G, T=C, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>=T+ 1, S=G, T=C, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=C, T= 1, U=B, V= 2*W, 
          X= 2, W=B, new6(A,B,C,D,E,U,V,H,I,J,K,L,M,N,O,P,Q,R).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- 
          new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S>= 2, S=B, T= 1, U=V- 1, V=B, 
          W= 1, new3(A,U,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :- S=< 1, S=B, T= 1, U=V- 1, V=C, 
          W= 1, new3(A,B,U,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,H,I,J,K,L,D,E,F,M,N,O,P,Q,R).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
