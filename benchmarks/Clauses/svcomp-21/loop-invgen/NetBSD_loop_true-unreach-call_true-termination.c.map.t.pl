new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=P+ 1, P=A, Q= 1, 
          R=S+ 1, S=C, T= 1, new9(A,B,M,U), new21(A,B,R,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=P+ 1, P=A, Q= 1, R=S+ 1, 
          S=C, T= 1, new9(A,B,M,U), new21(A,B,R,D,E,F,G,H,I,J,K,L).
new23(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new9(A,B,M,P), 
          new24(A,B,C,D,E,F,G,H,I,J,K,L).
new23(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new9(A,B,M,P), 
          new24(A,B,C,D,E,F,G,H,I,J,K,L).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M=<N, M=C, N=F, 
          new23(A,B,C,D,E,F,G,H,I,J,K,L).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=C, N=F, 
          new20(A,B,C,D,E,F,G,H,I,J,K,L).
new21(A,B,C,D,E,F,G,H,I,J,K,L) :- new22(A,B,C,D,E,F,G,H,I,J,K,L).
new20(A,B,C,D,E,F,A,B,C,D,E,F).
new19(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 2147483647, M=A, N= 2147483647, 
          O=P- 1, P=Q+R, Q=B, R=S+ 1, S=A, T= 1, U= 1, V=B, W=O, X=V, 
          new21(A,B,X,O,V,W,G,H,I,J,K,L).
new19(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 2147483647, M=A, N= 2147483647, 
          new20(A,B,C,D,E,F,G,H,I,J,K,L).
new18(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=A, N= 0, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L).
new18(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=A, N= 0, 
          new20(A,B,C,D,E,F,G,H,I,J,K,L).
new16(A,B,C,D) :- new18(A,B,E,F,G,H,C,D,I,J,K,L).
safe :- init(A,B), new16(A,B,C,D).
new15(A,B).
new13(A,B,C,C) :- new15(A,B).
new12(A,B,C,C).
new10(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H+ 1=<I, H=C, I=J+ 1, J=A, K= 1, 
          new8(A,B,G,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N+ 1=<O, N=C, O=P+ 1, P=A, Q= 1, 
          R=S+ 1, S=C, T= 1, new9(A,B,M,U), new5(A,B,R,D,E,F,G,H,I,J,K,L).
new10(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>=I, H=C, I=J+ 1, J=A, K= 1, 
          new8(A,B,G,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N>=O, N=C, O=P+ 1, P=A, Q= 1, R=S+ 1, 
          S=C, T= 1, new9(A,B,M,U), new5(A,B,R,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D) :- E>= 1, E=C, F= 0, new12(A,B,C,D).
new9(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new12(A,B,C,D).
new8(A,B,C,D) :- E= 0, E=C, F= 0, new13(A,B,C,D).
new7(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1,  0=<H, I= 0, H=C, new8(A,B,G,J).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1,  0=<N, O= 0, N=C, new9(A,B,M,P), 
          new10(A,B,C,D,E,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0,  0>=H+ 1, I= 0, H=C, new8(A,B,G,J).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0,  0>=N+ 1, O= 0, N=C, new9(A,B,M,P), 
          new10(A,B,C,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M=<N, M=C, N=F, new7(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- new6(A,B,C,D,E,F,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 2147483647, M=A, N= 2147483647, O=P- 1, 
          P=Q+R, Q=B, R=S+ 1, S=A, T= 1, U= 1, V=B, W=O, X=V, 
          new5(A,B,X,O,V,W,G,H,I,J,K,L).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=A, N= 0, 
          new3(A,B,C,D,E,F,G,H,I,J,K,L).
new1(A,B,C,D) :- new2(A,B,E,F,G,H,C,D,I,J,K,L).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
