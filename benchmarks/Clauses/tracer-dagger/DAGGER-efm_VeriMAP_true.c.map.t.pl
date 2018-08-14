new500(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L>= 1, L=B, M= 0.
new500(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 0, L=B, M= 0.
new467(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L>= 1, L=C, M= 0.
new467(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 0, L=C, M= 0.
new466(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new467(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new434(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L>= 2, L=D, M= 1.
new434(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 1, L=D, M= 1.
new433(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new434(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new432(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new433(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new401(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L>= 1, L=E, M= 0.
new401(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 0, L=E, M= 0.
new400(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, W=D, X= 1, 
          new401(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new399(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new400(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new398(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new399(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new368(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L>= 1, L=F, M= 0.
new368(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 0, L=F, M= 0.
new367(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=E, X= 0, 
          new368(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new366(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, W=D, X= 1, 
          new367(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new365(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new366(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new364(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new365(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new363(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          Y=Z- 1, Z=A, A1= 1, B1=C1- 1, C1=D, D1= 1, E1=F1+ 1, F1=B, G1= 1, 
          H1=I1+ 1, I1=E, J1= 1, 
          new335(Y,E1,C,B1,H1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new362(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=F, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=C, D1= 1, 
          new335(A,Y,B1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new361(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=B, D1= 1, 
          new335(A,B1,Y,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new360(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=D, G1=F, 
          H1= 0, new335(B1,Y,C,E1,E,H1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new359(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=F, G1=E, 
          H1= 0, new335(B1,B,Y,D,H1,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new358(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=G, X= 0, 
          new359(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new358(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=G, X= 0, 
          new359(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new358(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=G, X= 0, 
          new360(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new357(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new358(A,B,C,D,E,F,W,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new356(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          new361(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new355(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=H, X= 0, 
          new356(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new355(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=H, X= 0, 
          new356(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new355(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=H, X= 0, 
          new357(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new354(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new355(A,B,C,D,E,F,G,W,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new353(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          new362(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new352(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=I, X= 0, 
          new353(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new352(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=I, X= 0, 
          new353(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new352(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=I, X= 0, 
          new354(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new351(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new352(A,B,C,D,E,F,G,H,W,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new350(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 1, L=A, M= 1.
new350(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new363(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new349(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=J, X= 0, 
          new350(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new349(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=J, X= 0, 
          new350(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new349(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=J, X= 0, 
          new351(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new338(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new349(A,B,C,D,E,F,G,H,I,W,K,L,M,N,O,P,Q,R,S,T,U,V).
new337(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=K, X= 0, 
          new338(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new337(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=K, X= 0, 
          new338(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new336(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new337(A,B,C,D,E,F,G,H,I,J,W,L,M,N,O,P,Q,R,S,T,U,V).
new335(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new336(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new334(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=F, X= 0, 
          new335(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new333(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=E, X= 0, 
          new334(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new332(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, W=D, X= 1, 
          new333(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new331(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new332(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new330(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new331(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new329(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 1, L=D, M= 1.
new329(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          Y=Z- 1, Z=A, A1= 1, B1=C1- 1, C1=D, D1= 1, E1=F1+ 1, F1=B, G1= 1, 
          H1=I1+ 1, I1=E, J1= 1, 
          new301(Y,E1,C,B1,H1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new328(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=F, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=C, D1= 1, 
          new301(A,Y,B1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new327(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=B, D1= 1, 
          new301(A,B1,Y,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new326(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=D, G1=F, 
          H1= 0, new301(B1,Y,C,E1,E,H1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new325(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=F, G1=E, 
          H1= 0, new301(B1,B,Y,D,H1,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new324(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=G, X= 0, 
          new325(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new324(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=G, X= 0, 
          new325(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new324(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=G, X= 0, 
          new326(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new323(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new324(A,B,C,D,E,F,W,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new322(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          new327(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new321(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=H, X= 0, 
          new322(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new321(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=H, X= 0, 
          new322(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new321(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=H, X= 0, 
          new323(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new320(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new321(A,B,C,D,E,F,G,W,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new319(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          new328(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new318(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=I, X= 0, 
          new319(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new318(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=I, X= 0, 
          new319(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new318(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=I, X= 0, 
          new320(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new317(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new318(A,B,C,D,E,F,G,H,W,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new316(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new329(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new315(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=J, X= 0, 
          new316(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new315(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=J, X= 0, 
          new316(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new315(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=J, X= 0, 
          new317(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new304(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new315(A,B,C,D,E,F,G,H,I,W,K,L,M,N,O,P,Q,R,S,T,U,V).
new303(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=K, X= 0, 
          new304(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new303(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=K, X= 0, 
          new304(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new302(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new303(A,B,C,D,E,F,G,H,I,J,W,L,M,N,O,P,Q,R,S,T,U,V).
new301(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new302(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new300(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=F, X= 0, 
          new301(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new299(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=E, X= 0, 
          new300(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new298(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, W=D, X= 1, 
          new299(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new297(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new298(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new296(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new297(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new295(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          Y=Z- 1, Z=A, A1= 1, B1=C1- 1, C1=D, D1= 1, E1=F1+ 1, F1=B, G1= 1, 
          H1=I1+ 1, I1=E, J1= 1, 
          new267(Y,E1,C,B1,H1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new294(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=F, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=C, D1= 1, 
          new267(A,Y,B1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new293(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=B, D1= 1, 
          new267(A,B1,Y,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new292(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=D, G1=F, 
          H1= 0, new267(B1,Y,C,E1,E,H1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new291(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=F, G1=E, 
          H1= 0, new267(B1,B,Y,D,H1,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new290(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=G, X= 0, 
          new291(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new290(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=G, X= 0, 
          new291(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new290(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=G, X= 0, 
          new292(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new289(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new290(A,B,C,D,E,F,W,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new288(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          new293(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new287(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=H, X= 0, 
          new288(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new287(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=H, X= 0, 
          new288(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new287(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=H, X= 0, 
          new289(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new286(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new287(A,B,C,D,E,F,G,W,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new285(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 1, L=B, M= 1.
new285(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          new294(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new284(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=I, X= 0, 
          new285(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new284(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=I, X= 0, 
          new285(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new284(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=I, X= 0, 
          new286(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new283(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new284(A,B,C,D,E,F,G,H,W,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new282(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new295(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new281(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=J, X= 0, 
          new282(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new281(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=J, X= 0, 
          new282(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new281(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=J, X= 0, 
          new283(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new270(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new281(A,B,C,D,E,F,G,H,I,W,K,L,M,N,O,P,Q,R,S,T,U,V).
new269(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=K, X= 0, 
          new270(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new269(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=K, X= 0, 
          new270(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new268(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new269(A,B,C,D,E,F,G,H,I,J,W,L,M,N,O,P,Q,R,S,T,U,V).
new267(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new268(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new266(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=F, X= 0, 
          new267(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new265(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=E, X= 0, 
          new266(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new264(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, W=D, X= 1, 
          new265(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new263(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new264(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new262(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new263(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new261(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          Y=Z- 1, Z=A, A1= 1, B1=C1- 1, C1=D, D1= 1, E1=F1+ 1, F1=B, G1= 1, 
          H1=I1+ 1, I1=E, J1= 1, 
          new233(Y,E1,C,B1,H1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new260(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 1, L=F, M= 1.
new260(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=F, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=C, D1= 1, 
          new233(A,Y,B1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new259(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=B, D1= 1, 
          new233(A,B1,Y,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new258(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=D, G1=F, 
          H1= 0, new233(B1,Y,C,E1,E,H1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new257(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=F, G1=E, 
          H1= 0, new233(B1,B,Y,D,H1,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new256(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=G, X= 0, 
          new257(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new256(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=G, X= 0, 
          new257(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new256(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=G, X= 0, 
          new258(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new255(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new256(A,B,C,D,E,F,W,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new254(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          new259(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new253(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=H, X= 0, 
          new254(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new253(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=H, X= 0, 
          new254(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new253(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=H, X= 0, 
          new255(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new252(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new253(A,B,C,D,E,F,G,W,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new251(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          new260(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new250(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=I, X= 0, 
          new251(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new250(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=I, X= 0, 
          new251(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new250(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=I, X= 0, 
          new252(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new249(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new250(A,B,C,D,E,F,G,H,W,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new248(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new261(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new247(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=J, X= 0, 
          new248(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new247(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=J, X= 0, 
          new248(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new247(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=J, X= 0, 
          new249(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new236(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new247(A,B,C,D,E,F,G,H,I,W,K,L,M,N,O,P,Q,R,S,T,U,V).
new235(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=K, X= 0, 
          new236(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new235(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=K, X= 0, 
          new236(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new234(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new235(A,B,C,D,E,F,G,H,I,J,W,L,M,N,O,P,Q,R,S,T,U,V).
new233(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new234(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new232(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=F, X= 0, 
          new233(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new231(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=E, X= 0, 
          new232(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new230(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, W=D, X= 1, 
          new231(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new229(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new230(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new228(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new229(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new227(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          Y=Z- 1, Z=A, A1= 1, B1=C1- 1, C1=D, D1= 1, E1=F1+ 1, F1=B, G1= 1, 
          H1=I1+ 1, I1=E, J1= 1, 
          new199(Y,E1,C,B1,H1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new226(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=F, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=C, D1= 1, 
          new199(A,Y,B1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new225(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=B, D1= 1, 
          new199(A,B1,Y,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new224(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=D, G1=F, 
          H1= 0, new199(B1,Y,C,E1,E,H1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new223(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=F, G1=E, 
          H1= 0, new199(B1,B,Y,D,H1,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new222(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=G, X= 0, 
          new223(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new222(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=G, X= 0, 
          new223(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new222(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=G, X= 0, 
          new224(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new221(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new222(A,B,C,D,E,F,W,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new220(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 1, L=D, M= 1.
new220(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          new225(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new219(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=H, X= 0, 
          new220(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new219(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=H, X= 0, 
          new220(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new219(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=H, X= 0, 
          new221(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new218(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new219(A,B,C,D,E,F,G,W,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new217(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          new226(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new216(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=I, X= 0, 
          new217(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new216(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=I, X= 0, 
          new217(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new216(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=I, X= 0, 
          new218(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new215(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new216(A,B,C,D,E,F,G,H,W,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new214(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new227(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new213(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=J, X= 0, 
          new214(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new213(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=J, X= 0, 
          new214(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new213(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=J, X= 0, 
          new215(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new202(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new213(A,B,C,D,E,F,G,H,I,W,K,L,M,N,O,P,Q,R,S,T,U,V).
new201(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=K, X= 0, 
          new202(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new201(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=K, X= 0, 
          new202(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new200(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new201(A,B,C,D,E,F,G,H,I,J,W,L,M,N,O,P,Q,R,S,T,U,V).
new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new200(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new198(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=F, X= 0, 
          new199(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new197(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=E, X= 0, 
          new198(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new196(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, W=D, X= 1, 
          new197(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new195(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new196(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new194(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new195(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          Y=Z- 1, Z=A, A1= 1, B1=C1- 1, C1=D, D1= 1, E1=F1+ 1, F1=B, G1= 1, 
          H1=I1+ 1, I1=E, J1= 1, 
          new165(Y,E1,C,B1,H1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new192(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=F, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=C, D1= 1, 
          new165(A,Y,B1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new191(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 1, L=C, M= 1.
new191(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=B, D1= 1, 
          new165(A,B1,Y,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new190(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=D, G1=F, 
          H1= 0, new165(B1,Y,C,E1,E,H1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new189(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=F, G1=E, 
          H1= 0, new165(B1,B,Y,D,H1,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new188(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=G, X= 0, 
          new189(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new188(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=G, X= 0, 
          new189(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new188(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=G, X= 0, 
          new190(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new188(A,B,C,D,E,F,W,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          new191(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new185(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=H, X= 0, 
          new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new185(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=H, X= 0, 
          new186(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new185(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=H, X= 0, 
          new187(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new184(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new185(A,B,C,D,E,F,G,W,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new183(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          new192(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=I, X= 0, 
          new183(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=I, X= 0, 
          new183(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new182(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=I, X= 0, 
          new184(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new181(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new182(A,B,C,D,E,F,G,H,W,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new193(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=J, X= 0, 
          new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=J, X= 0, 
          new180(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new179(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=J, X= 0, 
          new181(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new168(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new179(A,B,C,D,E,F,G,H,I,W,K,L,M,N,O,P,Q,R,S,T,U,V).
new167(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=K, X= 0, 
          new168(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new167(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=K, X= 0, 
          new168(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new166(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new167(A,B,C,D,E,F,G,H,I,J,W,L,M,N,O,P,Q,R,S,T,U,V).
new165(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new166(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new164(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=F, X= 0, 
          new165(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new163(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=E, X= 0, 
          new164(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new162(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, W=D, X= 1, 
          new163(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new161(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new162(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new160(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new161(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new159(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          Y=Z- 1, Z=A, A1= 1, B1=C1- 1, C1=D, D1= 1, E1=F1+ 1, F1=B, G1= 1, 
          H1=I1+ 1, I1=E, J1= 1, 
          new131(Y,E1,C,B1,H1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new158(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=F, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=C, D1= 1, 
          new131(A,Y,B1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new157(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=B, D1= 1, 
          new131(A,B1,Y,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=D, G1=F, 
          H1= 0, new131(B1,Y,C,E1,E,H1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new155(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 1, L=C, M= 1.
new155(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=F, G1=E, 
          H1= 0, new131(B1,B,Y,D,H1,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new154(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=G, X= 0, 
          new155(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new154(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=G, X= 0, 
          new155(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new154(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=G, X= 0, 
          new156(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new153(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new154(A,B,C,D,E,F,W,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new152(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          new157(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new151(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=H, X= 0, 
          new152(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new151(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=H, X= 0, 
          new152(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new151(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=H, X= 0, 
          new153(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new150(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new151(A,B,C,D,E,F,G,W,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new149(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          new158(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=I, X= 0, 
          new149(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=I, X= 0, 
          new149(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new148(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=I, X= 0, 
          new150(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new147(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new148(A,B,C,D,E,F,G,H,W,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new146(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new159(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new145(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=J, X= 0, 
          new146(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new145(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=J, X= 0, 
          new146(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new145(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=J, X= 0, 
          new147(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new145(A,B,C,D,E,F,G,H,I,W,K,L,M,N,O,P,Q,R,S,T,U,V).
new133(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=K, X= 0, 
          new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new133(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=K, X= 0, 
          new134(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new132(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new133(A,B,C,D,E,F,G,H,I,J,W,L,M,N,O,P,Q,R,S,T,U,V).
new131(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new132(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new130(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=F, X= 0, 
          new131(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=E, X= 0, 
          new130(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new128(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, W=D, X= 1, 
          new129(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new127(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new128(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new126(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new127(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new125(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          Y=Z- 1, Z=A, A1= 1, B1=C1- 1, C1=D, D1= 1, E1=F1+ 1, F1=B, G1= 1, 
          H1=I1+ 1, I1=E, J1= 1, 
          new97(Y,E1,C,B1,H1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new124(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=F, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=C, D1= 1, 
          new97(A,Y,B1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new123(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=B, D1= 1, 
          new97(A,B1,Y,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new122(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 1, L=B, M= 1.
new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          Y=Z- 1, Z=B, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=D, G1=F, 
          H1= 0, new97(B1,Y,C,E1,E,H1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new121(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, 
          Y=Z- 1, Z=C, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=F, G1=E, 
          H1= 0, new97(B1,B,Y,D,H1,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=G, X= 0, 
          new121(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=G, X= 0, 
          new121(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new120(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=G, X= 0, 
          new122(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new119(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new120(A,B,C,D,E,F,W,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          new123(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new117(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=H, X= 0, 
          new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new117(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=H, X= 0, 
          new118(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new117(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=H, X= 0, 
          new119(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new117(A,B,C,D,E,F,G,W,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          new124(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new114(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=I, X= 0, 
          new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new114(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=I, X= 0, 
          new115(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new114(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=I, X= 0, 
          new116(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new114(A,B,C,D,E,F,G,H,W,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new112(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new125(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=J, X= 0, 
          new112(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=J, X= 0, 
          new112(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new111(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=J, X= 0, 
          new113(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new111(A,B,C,D,E,F,G,H,I,W,K,L,M,N,O,P,Q,R,S,T,U,V).
new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=K, X= 0, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new99(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=K, X= 0, 
          new100(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new99(A,B,C,D,E,F,G,H,I,J,W,L,M,N,O,P,Q,R,S,T,U,V).
new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new98(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new96(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=F, X= 0, 
          new97(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new95(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=E, X= 0, 
          new96(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, W=D, X= 1, 
          new95(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new92(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, Y=Z- 1, 
          Z=A, A1= 1, B1=C1- 1, C1=D, D1= 1, E1=F1+ 1, F1=B, G1= 1, H1=I1+ 1, 
          I1=E, J1= 1, new63(Y,E1,C,B1,H1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=F, X= 1, Y=Z- 1, 
          Z=B, A1= 1, B1=C1+ 1, C1=C, D1= 1, 
          new63(A,Y,B1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, Y=Z- 1, 
          Z=C, A1= 1, B1=C1+ 1, C1=B, D1= 1, 
          new63(A,B1,Y,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, Y=Z- 1, 
          Z=B, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=D, G1=F, H1= 0, 
          new63(B1,Y,C,E1,E,H1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, Y=Z- 1, 
          Z=C, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=F, G1=E, H1= 0, 
          new63(B1,B,Y,D,H1,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=G, X= 0, 
          new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=G, X= 0, 
          new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=G, X= 0, 
          new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new86(A,B,C,D,E,F,W,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=H, X= 0, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=H, X= 0, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=H, X= 0, 
          new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new83(A,B,C,D,E,F,G,W,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=I, X= 0, 
          new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=I, X= 0, 
          new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=I, X= 0, 
          new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new80(A,B,C,D,E,F,G,H,W,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=J, X= 0, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=J, X= 0, 
          new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=J, X= 0, 
          new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new76(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 1, M>= 1, M=N+O, 
          N=P+Q, P=A, Q=B, O=C, R= 1, new14(A,B,C,D,E,F,L,S).
new76(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 0, M+ 1=< 1, M=N+O, 
          N=P+Q, P=A, Q=B, O=C, R= 1, new14(A,B,C,D,E,F,L,S).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>=Y, X=Z+A1, Z=A, 
          A1=B, Y=B1+C1, B1=D, C1=E, new14(A,B,C,D,E,F,W,D1), 
          new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=<Y, X=Z+A1, 
          Z=A, A1=B, Y=B1+C1, B1=D, C1=E, new14(A,B,C,D,E,F,W,D1), 
          new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>= 1, X=Y+Z, Y=A, 
          Z=E, A1= 1, new14(A,B,C,D,E,F,W,B1), 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=< 1, X=Y+Z, 
          Y=A, Z=E, A1= 1, new14(A,B,C,D,E,F,W,B1), 
          new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>= 0, X=B, Y= 0, 
          new14(A,B,C,D,E,F,W,Z), 
          new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=< 0, X=B, 
          Y= 0, new14(A,B,C,D,E,F,W,Z), 
          new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>= 0, X=C, Y= 0, 
          new14(A,B,C,D,E,F,W,Z), 
          new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=< 0, X=C, 
          Y= 0, new14(A,B,C,D,E,F,W,Z), 
          new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>= 0, X=D, Y= 0, 
          new14(A,B,C,D,E,F,W,Z), 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=< 0, X=D, 
          Y= 0, new14(A,B,C,D,E,F,W,Z), 
          new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>= 0, X=E, Y= 0, 
          new14(A,B,C,D,E,F,W,Z), 
          new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=< 0, X=E, 
          Y= 0, new14(A,B,C,D,E,F,W,Z), 
          new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X=< 1, X=Y+Z, Y=D, 
          Z=E, A1= 1, new14(A,B,C,D,E,F,W,B1), 
          new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X>= 2, X=Y+Z, Y=D, 
          Z=E, A1= 1, new14(A,B,C,D,E,F,W,B1), 
          new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X=< 0, X=Y- 1, 
          Y=Z+A1, Z=B1+C1, B1=D, C1=E, A1=F, D1= 1, E1= 0, 
          new14(A,B,C,D,E,F,W,F1), 
          new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X>= 1, X=Y- 1, 
          Y=Z+A1, Z=B1+C1, B1=D, C1=E, A1=F, D1= 1, E1= 0, 
          new14(A,B,C,D,E,F,W,F1), 
          new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>= 0, X=Y- 1, 
          Y=Z+A1, Z=B1+C1, B1=D, C1=E, A1=F, D1= 1, E1= 0, 
          new14(A,B,C,D,E,F,W,F1), 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=< 0, X=Y- 1, 
          Y=Z+A1, Z=B1+C1, B1=D, C1=E, A1=F, D1= 1, E1= 0, 
          new14(A,B,C,D,E,F,W,F1), 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new77(A,B,C,D,E,F,G,H,I,W,K,L,M,N,O,P,Q,R,S,T,U,V).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=K, X= 0, 
          new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=K, X= 0, 
          new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=K, X= 0, 
          new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new65(A,B,C,D,E,F,G,H,I,J,W,L,M,N,O,P,Q,R,S,T,U,V).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=F, X= 0, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=E, X= 0, 
          new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, W=D, X= 1, 
          new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new92(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new126(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new160(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new194(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new52(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new228(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new51(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new262(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new50(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new296(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new49(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new330(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new48(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new364(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new47(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new398(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new46(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new432(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new466(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new500(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new43(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L+ 1=< 1, L=A, M= 1.
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new43(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new44(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new45(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new46(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new47(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new48(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new49(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new50(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new51(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new52(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new53(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new54(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new55(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new56(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new57(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
safe :- init(A,B,C,D,E,F), new41(A,B,C,D,E,F,G,H,I,J,K,L).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, Y=Z- 1, 
          Z=A, A1= 1, B1=C1- 1, C1=D, D1= 1, E1=F1+ 1, F1=B, G1= 1, H1=I1+ 1, 
          I1=E, J1= 1, new8(Y,E1,C,B1,H1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=F, X= 1, Y=Z- 1, 
          Z=B, A1= 1, B1=C1+ 1, C1=C, D1= 1, 
          new8(A,Y,B1,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, Y=Z- 1, 
          Z=C, A1= 1, B1=C1+ 1, C1=B, D1= 1, 
          new8(A,B1,Y,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, Y=Z- 1, 
          Z=B, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=D, G1=F, H1= 0, 
          new8(B1,Y,C,E1,E,H1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=C, X= 1, Y=Z- 1, 
          Z=C, A1= 1, B1=C1+ 1, C1=A, D1= 1, E1=F1+G1, F1=F, G1=E, H1= 0, 
          new8(B1,B,Y,D,H1,E1,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=G, X= 0, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=G, X= 0, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=G, X= 0, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new35(A,B,C,D,E,F,W,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=D, X= 1, 
          new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=H, X= 0, 
          new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=H, X= 0, 
          new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=H, X= 0, 
          new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new32(A,B,C,D,E,F,G,W,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=B, X= 1, 
          new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=I, X= 0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=I, X= 0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=I, X= 0, 
          new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new29(A,B,C,D,E,F,G,H,W,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=J, X= 0, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=J, X= 0, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=J, X= 0, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new25(A,B,C,D,E,F,G,G).
new23(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 1, M>= 1, M=N+O, 
          N=P+Q, P=A, Q=B, O=C, R= 1, new13(A,B,C,D,E,F,L,S).
new23(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 0, M+ 1=< 1, M=N+O, 
          N=P+Q, P=A, Q=B, O=C, R= 1, new13(A,B,C,D,E,F,L,S).
new22(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 1, M>=N, M=O+P, O=A, 
          P=B, N=Q+R, Q=D, R=E, new13(A,B,C,D,E,F,L,S).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>=Y, X=Z+A1, Z=A, 
          A1=B, Y=B1+C1, B1=D, C1=E, new14(A,B,C,D,E,F,W,D1), 
          new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new22(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 0, M+ 1=<N, M=O+P, 
          O=A, P=B, N=Q+R, Q=D, R=E, new13(A,B,C,D,E,F,L,S).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=<Y, X=Z+A1, 
          Z=A, A1=B, Y=B1+C1, B1=D, C1=E, new14(A,B,C,D,E,F,W,D1), 
          new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new21(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 1, M>= 1, M=N+O, N=A, 
          O=E, P= 1, new13(A,B,C,D,E,F,L,Q).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>= 1, X=Y+Z, Y=A, 
          Z=E, A1= 1, new14(A,B,C,D,E,F,W,B1), 
          new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new21(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 0, M+ 1=< 1, M=N+O, 
          N=A, O=E, P= 1, new13(A,B,C,D,E,F,L,Q).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=< 1, X=Y+Z, 
          Y=A, Z=E, A1= 1, new14(A,B,C,D,E,F,W,B1), 
          new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new20(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 1, M>= 0, M=B, N= 0, 
          new13(A,B,C,D,E,F,L,O).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>= 0, X=B, Y= 0, 
          new14(A,B,C,D,E,F,W,Z), 
          new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new20(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 0, M+ 1=< 0, M=B, 
          N= 0, new13(A,B,C,D,E,F,L,O).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=< 0, X=B, 
          Y= 0, new14(A,B,C,D,E,F,W,Z), 
          new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new19(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 1, M>= 0, M=C, N= 0, 
          new13(A,B,C,D,E,F,L,O).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>= 0, X=C, Y= 0, 
          new14(A,B,C,D,E,F,W,Z), 
          new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new19(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 0, M+ 1=< 0, M=C, 
          N= 0, new13(A,B,C,D,E,F,L,O).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=< 0, X=C, 
          Y= 0, new14(A,B,C,D,E,F,W,Z), 
          new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new18(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 1, M>= 0, M=D, N= 0, 
          new13(A,B,C,D,E,F,L,O).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>= 0, X=D, Y= 0, 
          new14(A,B,C,D,E,F,W,Z), 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new18(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 0, M+ 1=< 0, M=D, 
          N= 0, new13(A,B,C,D,E,F,L,O).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=< 0, X=D, 
          Y= 0, new14(A,B,C,D,E,F,W,Z), 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new17(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 1, M>= 0, M=E, N= 0, 
          new13(A,B,C,D,E,F,L,O).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>= 0, X=E, Y= 0, 
          new14(A,B,C,D,E,F,W,Z), 
          new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new17(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 0, M+ 1=< 0, M=E, 
          N= 0, new13(A,B,C,D,E,F,L,O).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=< 0, X=E, 
          Y= 0, new14(A,B,C,D,E,F,W,Z), 
          new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new16(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 1, M=< 1, M=N+O, N=D, 
          O=E, P= 1, new13(A,B,C,D,E,F,L,Q).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X=< 1, X=Y+Z, Y=D, 
          Z=E, A1= 1, new14(A,B,C,D,E,F,W,B1), 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new16(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 0, M>= 2, M=N+O, N=D, 
          O=E, P= 1, new13(A,B,C,D,E,F,L,Q).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X>= 2, X=Y+Z, Y=D, 
          Z=E, A1= 1, new14(A,B,C,D,E,F,W,B1), 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new15(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 1, M=< 0, M=N- 1, 
          N=O+P, O=Q+R, Q=D, R=E, P=F, S= 1, T= 0, new13(A,B,C,D,E,F,L,U).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X=< 0, X=Y- 1, 
          Y=Z+A1, Z=B1+C1, B1=D, C1=E, A1=F, D1= 1, E1= 0, 
          new14(A,B,C,D,E,F,W,F1), 
          new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new15(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 0, M>= 1, M=N- 1, 
          N=O+P, O=Q+R, Q=D, R=E, P=F, S= 1, T= 0, new13(A,B,C,D,E,F,L,U).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X>= 1, X=Y- 1, 
          Y=Z+A1, Z=B1+C1, B1=D, C1=E, A1=F, D1= 1, E1= 0, 
          new14(A,B,C,D,E,F,W,F1), 
          new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new14(A,B,C,D,E,F,G,G) :- H>= 1, H=G, I= 0.
new14(A,B,C,D,E,F,G,G) :- H+ 1=< 0, H=G, I= 0.
new13(A,B,C,D,E,F,G,H) :- I= 0, I=G, J= 0, new25(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 1, M>= 0, M=N- 1, 
          N=O+P, O=Q+R, Q=D, R=E, P=F, S= 1, T= 0, new13(A,B,C,D,E,F,L,U).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, X>= 0, X=Y- 1, 
          Y=Z+A1, Z=B1+C1, B1=D, C1=E, A1=F, D1= 1, E1= 0, 
          new14(A,B,C,D,E,F,W,F1), 
          new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new12(A,B,C,D,E,F,G,H,I,J,K,A,B,C,D,E,F,G,H,I,J,K) :- L= 0, M+ 1=< 0, M=N- 1, 
          N=O+P, O=Q+R, Q=D, R=E, P=F, S= 1, T= 0, new13(A,B,C,D,E,F,L,U).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, X+ 1=< 0, X=Y- 1, 
          Y=Z+A1, Z=B1+C1, B1=D, C1=E, A1=F, D1= 1, E1= 0, 
          new14(A,B,C,D,E,F,W,F1), 
          new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new26(A,B,C,D,E,F,G,H,I,W,K,L,M,N,O,P,Q,R,S,T,U,V).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=K, X= 0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W+ 1=< 0, W=K, X= 0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=K, X= 0, 
          new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new10(A,B,C,D,E,F,G,H,I,J,W,L,M,N,O,P,Q,R,S,T,U,V).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=F, X= 0, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=E, X= 0, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 1, W=D, X= 1, 
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=C, X= 0, 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W= 0, W=B, X= 0, 
          new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) :- W>= 1, W=A, X= 1, 
          new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).
new1(A,B,C,D,E,F,G,H,I,J,K,L) :- 
          new2(A,B,C,D,E,F,M,N,O,P,Q,G,H,I,J,K,L,R,S,T,U,V).
init(A,B,C,D,E,F).
false :- init(A,B,C,D,E,F), new1(A,B,C,D,E,F,G,H,I,J,K,L).
spec :- false.
spec :- safe.
