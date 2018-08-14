new29(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N+ 1, N=E, O= 1, 
          new24(A,B,C,D,M,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, O=P+ 1, P=B, Q= 1, R=S+ 2, 
          S=C, T= 2, new29(A,O,R,D,E,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, O=P+ 1, P=B, Q= 1, 
          R=S+ 2, S=C, T= 2, new29(A,O,R,D,E,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=F, N= 0, O=P+ 2, P=B, Q= 2, R=S+ 1, 
          S=C, T= 1, new29(A,O,R,D,E,F,G,H,I,J,K,L).
new27(A,B,C,D,E,F,A,B,C,D,E,F) :- G=H, G=I+J, I=B, J=C, H= 3*K, L= 3, K=D.
new26(A,B,C,D,E,F,G,H,I,J,K,L) :- new28(A,B,C,D,E,M,G,H,I,J,K,L).
new25(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=E, N=D, 
          new26(A,B,C,D,E,F,G,H,I,J,K,L).
new25(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=E, N=D, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- new25(A,B,C,D,E,F,G,H,I,J,K,L).
new23(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N>= 0, N=D, O= 0, P= 0, 
          new4(A,B,C,M,Q), new24(A,B,C,D,P,F,G,H,I,J,K,L).
new23(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N+ 1=< 0, N=D, O= 0, P= 0, 
          new4(A,B,C,M,Q), new24(A,B,C,D,P,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- new23(A,B,C,D,E,F,G,H,I,J,K,L).
new12(A,B,C,D,E,F) :- new14(A,B,C,G,H,I,D,E,F,J,K,L).
safe :- init(A,B,C), new12(A,B,C,D,E,F).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N+ 1, N=E, O= 1, 
          new5(A,B,C,D,M,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, O=P+ 1, P=B, Q= 1, R=S+ 2, 
          S=C, T= 2, new10(A,O,R,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, O=P+ 1, P=B, Q= 1, 
          R=S+ 2, S=C, T= 2, new10(A,O,R,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=F, N= 0, O=P+ 2, P=B, Q= 2, R=S+ 1, 
          S=C, T= 1, new10(A,O,R,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,A,B,C,D,E,F) :- G>=H+ 1, G=I+J, I=B, J=C, H= 3*K, L= 3, K=D.
new8(A,B,C,D,E,F,A,B,C,D,E,F) :- G+ 1=<H, G=I+J, I=B, J=C, H= 3*K, L= 3, K=D.
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- new9(A,B,C,D,E,M,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=E, N=D, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=E, N=D, new8(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- new6(A,B,C,D,E,F,G,H,I,J,K,L).
new4(A,B,C,D,D) :- E>= 1, E=D, F= 0.
new4(A,B,C,D,D) :- E+ 1=< 0, E=D, F= 0.
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N>= 0, N=D, O= 0, P= 0, new4(A,B,C,M,Q), 
          new5(A,B,C,D,P,F,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N+ 1=< 0, N=D, O= 0, P= 0, 
          new4(A,B,C,M,Q), new5(A,B,C,D,P,F,G,H,I,J,K,L).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- new3(A,B,C,D,E,F,G,H,I,J,K,L).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,H,I,D,E,F,J,K,L).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
