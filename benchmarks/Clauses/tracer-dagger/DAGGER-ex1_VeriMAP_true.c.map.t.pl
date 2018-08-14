new23(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N-O, N=C, O= 2*P, Q= 2, P=D, R=S+T, S= 2*U, 
          V= 2, U=C, T=D, new15(M,R,C,D,E,F,G,H,I,J,K,L).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, O=P+Q, P=D, Q=C, 
          new23(A,B,C,O,E,F,G,H,I,J,K,L).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, O=P+Q, P=D, Q=C, 
          new23(A,B,C,O,E,F,G,H,I,J,K,L).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, O=P-Q, P=D, Q=C, 
          new23(A,B,C,O,E,F,G,H,I,J,K,L).
new20(A,B,C,C) :- D>= 1, D=C, E= 0.
new20(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new19(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H>= 0, H=I+J, I=A, J= 2*K, L= 2, K=B, 
          M= 0, new20(A,B,G,N).
new19(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H+ 1=< 0, H=I+J, I=A, J= 2*K, L= 2, 
          K=B, M= 0, new20(A,B,G,N).
new18(A,B,C,D,E,F,G,H,I,J,K,L) :- new22(A,B,C,D,M,F,G,H,I,J,K,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, O=P+Q, P=A, Q= 2*R, S= 2, 
          R=B, T=U+V, U= -2*W, X= -2, W=A, V=B, Y=Z+ 1, Z=O, A1= 1, 
          new18(A,B,Y,T,E,F,G,H,I,J,K,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, O=P+Q, P=A, Q= 2*R, 
          S= 2, R=B, T=U+V, U= -2*W, X= -2, W=A, V=B, Y=Z+ 1, Z=O, A1= 1, 
          new18(A,B,Y,T,E,F,G,H,I,J,K,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=F, N= 0, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- new17(A,B,C,D,E,M,G,H,I,J,K,L).
new15(A,B,C,D,E,F,G,H,I,J,K,L) :- new16(A,B,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- new15(A,B,C,D,E,F,G,H,I,J,K,L).
new12(A,B,C,D) :- new14(A,B,E,F,G,H,C,D,I,J,K,L).
safe :- init(A,B), new12(A,B,C,D).
new11(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N-O, N=C, O= 2*P, Q= 2, P=D, R=S+T, S= 2*U, 
          V= 2, U=C, T=D, new3(M,R,C,D,E,F,G,H,I,J,K,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, O=P+Q, P=D, Q=C, 
          new11(A,B,C,O,E,F,G,H,I,J,K,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, O=P+Q, P=D, Q=C, 
          new11(A,B,C,O,E,F,G,H,I,J,K,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, O=P-Q, P=D, Q=C, 
          new11(A,B,C,O,E,F,G,H,I,J,K,L).
new9(A,B,C,C).
new8(A,B,C,D) :- E= 0, E=C, F= 0, new9(A,B,C,D).
new7(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H>= 0, H=I+J, I=A, J= 2*K, L= 2, K=B, 
          M= 0, new8(A,B,G,N).
new7(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H+ 1=< 0, H=I+J, I=A, J= 2*K, L= 2, K=B, 
          M= 0, new8(A,B,G,N).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- new10(A,B,C,D,M,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, O=P+Q, P=A, Q= 2*R, S= 2, 
          R=B, T=U+V, U= -2*W, X= -2, W=A, V=B, Y=Z+ 1, Z=O, A1= 1, 
          new6(A,B,Y,T,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, O=P+Q, P=A, Q= 2*R, S= 2, 
          R=B, T=U+V, U= -2*W, X= -2, W=A, V=B, Y=Z+ 1, Z=O, A1= 1, 
          new6(A,B,Y,T,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=F, N= 0, new7(A,B,C,D,E,F,G,H,I,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- new5(A,B,C,D,E,M,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- new4(A,B,C,D,E,F,G,H,I,J,K,L).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- new3(A,B,C,D,E,F,G,H,I,J,K,L).
new1(A,B,C,D) :- new2(A,B,E,F,G,H,C,D,I,J,K,L).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
