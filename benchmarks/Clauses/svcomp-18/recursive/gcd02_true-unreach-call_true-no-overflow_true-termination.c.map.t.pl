new102(A,B,C,D,E,A,B,C,D,E) :- F>= 2147483648, F=A, G= 2147483647.
new93(A,B,C,D,E,A,B,C,D,E) :- F=< 0, F=B, G= 0.
new92(A,B,C,D,E,F,G,H,I,J) :- K=< 2147483647, K=A, L= 2147483647, 
          new93(A,B,C,D,E,F,G,H,I,J).
new84(A,B,C,D,E,A,B,C,D,E) :- F>= 2147483648, F=B, G= 2147483647.
new83(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=B, L= 0, new84(A,B,C,D,E,F,G,H,I,J).
new82(A,B,C,D,E,F,G,H,I,J) :- K=< 2147483647, K=A, L= 2147483647, 
          new83(A,B,C,D,E,F,G,H,I,J).
new80(A,B,C,D,E,A,B,C,D,E) :- F>= 1, F=E, G= 0.
new80(A,B,C,D,E,A,B,C,D,E) :- F+ 1=< 0, F=E, G= 0.
new79(A,B,C,D,E,F,G,H,I,J) :- K=C, L=A, M= 1, new28(A,B,K,L,N,O,P,Q,R,S), 
          new80(O,P,C,D,M,F,G,H,I,J).
new79(A,B,C,D,E,F,G,H,I,J) :- K=C, L=A, M= 0, new30(A,B,K,L,N,O,P,Q,R,S), 
          new80(O,P,C,D,M,F,G,H,I,J).
new79(A,B,C,D,E,F,G,H,I,J) :- K=C, L=A, M=N, new31(A,B,K,L,O,P,Q,R,S,N), 
          new80(P,Q,C,D,M,F,G,H,I,J).
new78(A,B,C,D,E,F,G,H,I,J) :- K=A, L=B, M=N, O=M, 
          new11(A,B,K,L,P,Q,R,S,N,T,U,V), new79(R,S,O,M,E,F,G,H,I,J).
new78(A,B,C,D,E,F,G,H,I,J) :- K=A, L=B, M=N, O=M, 
          new13(A,B,K,L,P,Q,R,S,T,U,N,V), new79(R,S,O,M,E,F,G,H,I,J).
new78(A,B,C,D,E,F,G,H,I,J) :- K=A, L=B, M=N, O=M, 
          new14(A,B,K,L,P,Q,R,S,T,U,V,N), new79(R,S,O,M,E,F,G,H,I,J).
new76(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=B, L= 0, new78(A,B,C,D,E,F,G,H,I,J).
new75(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=A, L= 0, new76(A,B,C,D,E,F,G,H,I,J).
new74(A,B,C,D,E,F,G,H,I,J) :- K=< 2147483647, K=B, L= 2147483647, 
          new75(A,B,C,D,E,F,G,H,I,J).
new73(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=B, L= 0, new74(A,B,C,D,E,F,G,H,I,J).
new72(A,B,C,D,E,F,G,H,I,J) :- K=< 2147483647, K=A, L= 2147483647, 
          new73(A,B,C,D,E,F,G,H,I,J).
new67(A,B,C,D,E,A,B,C,D,E).
new66(A,B,C,D,E,F,G,H,I,J) :- K=< 0, K=B, L= 0, new67(A,B,C,D,E,F,G,H,I,J).
new65(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=A, L= 0, new66(A,B,C,D,E,F,G,H,I,J).
new65(A,B,C,D,E,F,G,H,I,J) :- K=< 0, K=A, L= 0, new67(A,B,C,D,E,F,G,H,I,J).
new64(A,B,C,D,E,F,G,H,I,J) :- K=< 2147483647, K=B, L= 2147483647, 
          new65(A,B,C,D,E,F,G,H,I,J).
new63(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=B, L= 0, new64(A,B,C,D,E,F,G,H,I,J).
new62(A,B,C,D,E,F,G,H,I,J) :- K=< 2147483647, K=A, L= 2147483647, 
          new63(A,B,C,D,E,F,G,H,I,J).
new61(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=A, L= 0, new62(A,B,C,D,E,F,G,H,I,J).
new60(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=A, L= 0, new72(A,B,C,D,E,F,G,H,I,J).
new59(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=A, L= 0, new82(A,B,C,D,E,F,G,H,I,J).
new58(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=A, L= 0, new92(A,B,C,D,E,F,G,H,I,J).
new57(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=A, L= 0, new102(A,B,C,D,E,F,G,H,I,J).
new56(A,B,C,D,E,A,B,C,D,E) :- F=< 0, F=A, G= 0.
new54(A,B,C,D) :- new56(A,B,E,F,G,C,D,H,I,J).
new54(A,B,C,D) :- new57(A,B,E,F,G,C,D,H,I,J).
new54(A,B,C,D) :- new58(A,B,E,F,G,C,D,H,I,J).
new54(A,B,C,D) :- new59(A,B,E,F,G,C,D,H,I,J).
new54(A,B,C,D) :- new60(A,B,E,F,G,C,D,H,I,J).
new54(A,B,C,D) :- new61(A,B,E,F,G,C,D,H,I,J).
safe :- init(A,B), new54(A,B,C,D).
new53(A,B,C,D,E,F,G,H,C,D,E,F) :- I=C, J=K-L, K=D, L=C, 
          new10(A,B,I,J,M,N,G,H,O,P,Q,R).
new52(A,B,C,D,E,F,G,H,C,D,E,F) :- I=J-K, J=C, K=D, L=D, 
          new10(A,B,I,L,M,N,G,H,O,P,Q,R).
new51(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=C, N=D, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L).
new51(A,B,C,D,E,F,G,H,I,J,K,L) :- M=<N, M=C, N=D, 
          new53(A,B,C,D,E,F,G,H,I,J,K,L).
new50(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=C, N=D, 
          new51(A,B,C,D,E,F,G,H,I,J,K,L).
new50(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=C, N=D, 
          new51(A,B,C,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=D, N= 0, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L).
new48(A,B,C,D,E,F,A,B,C,D,E,F).
new44(A,B,C,D,E,F,A,B,C,D,E,F) :- G=H, G=C, H=D.
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L).
new37(A,B).
new36(A,B,C,D,E,A,B,C,D,E) :- new37(A,B).
new34(A,B,C,D,E,A,B,C,D,E) :- F>=G+ 1, F=C, G=D.
new33(A,B,C,D,E,F,G,C,D,H) :- I=C, J=K-L, K=D, L=C, H= 1, 
          new28(A,B,I,J,M,F,G,N,O,P).
new33(A,B,C,D,E,F,G,C,D,H) :- I=C, J=K-L, K=D, L=C, H= 0, 
          new30(A,B,I,J,M,F,G,N,O,P).
new33(A,B,C,D,E,F,G,C,D,H) :- I=C, J=K-L, K=D, L=C, H=M, 
          new31(A,B,I,J,N,F,G,O,P,M).
new32(A,B,C,D,E,F,G,H,I,J) :- K=<L, K=C, L=D, new33(A,B,C,D,E,F,G,H,I,J).
new31(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=D, L= 0, new32(A,B,C,D,E,F,G,H,I,J).
new31(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=D, L= 0, new32(A,B,C,D,E,F,G,H,I,J).
new30(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=D, L= 0, new34(A,B,C,D,E,F,G,H,I,J).
new30(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=D, L= 0, new34(A,B,C,D,E,F,G,H,I,J).
new29(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, new36(A,B,C,D,E,F,G,H,I,J).
new28(A,B,C,D,E,A,B,C,D,E) :- F= 0, F=D, G= 0.
new25(A,B,C,D,E,F,G,H,C,D,I,F) :- J=K-L, K=C, L=D, M=D, I=N, 
          new11(A,B,J,M,O,P,G,H,N,Q,R,S).
new25(A,B,C,D,E,F,G,H,C,D,I,F) :- J=K-L, K=C, L=D, M=D, I=N, 
          new13(A,B,J,M,O,P,G,H,Q,R,N,S).
new25(A,B,C,D,E,F,G,H,C,D,I,F) :- J=K-L, K=C, L=D, M=D, I=N, 
          new14(A,B,J,M,O,P,G,H,Q,R,S,N).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=C, N=D, 
          new25(A,B,C,D,E,F,G,H,I,J,K,L).
new23(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=C, N=D, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L).
new23(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=C, N=D, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new23(A,B,C,D,E,F,G,H,I,J,K,L).
new20(A,B,C,D,E,F,G,H,C,D,E,I) :- J=C, K=L-M, L=D, M=C, I=N, 
          new11(A,B,J,K,O,P,G,H,N,Q,R,S).
new20(A,B,C,D,E,F,G,H,C,D,E,I) :- J=C, K=L-M, L=D, M=C, I=N, 
          new13(A,B,J,K,O,P,G,H,Q,R,N,S).
new20(A,B,C,D,E,F,G,H,C,D,E,I) :- J=C, K=L-M, L=D, M=C, I=N, 
          new14(A,B,J,K,O,P,G,H,Q,R,S,N).
new18(A,B,C,D,E,F,G,H,I,J,K,L) :- M=<N, M=C, N=D, 
          new20(A,B,C,D,E,F,G,H,I,J,K,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+ 1, M=C, N=D, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=<N, M=C, N=D, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new17(A,B,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new16(A,B,C,D,E,F,G,H,I,J,K,L).
new13(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L).
new12(A,B,C,D,E,F,G,H,I,J) :- K=C, L=A, M= 1, new28(A,B,K,L,N,O,P,Q,R,S), 
          new29(O,P,C,D,M,F,G,H,I,J).
new12(A,B,C,D,E,F,G,H,I,J) :- K=C, L=A, M= 0, new30(A,B,K,L,N,O,P,Q,R,S), 
          new29(O,P,C,D,M,F,G,H,I,J).
new12(A,B,C,D,E,F,G,H,I,J) :- K=C, L=A, M=N, new31(A,B,K,L,O,P,Q,R,S,N), 
          new29(P,Q,C,D,M,F,G,H,I,J).
new11(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=C, N= 0, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new49(A,B,C,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,C,D,E) :- H=A, I=B, new10(A,B,H,I,J,K,F,G,L,M,N,O).
new9(A,B,C,D,E,F,G,H,I,J) :- K=A, L=B, M=N, O=M, 
          new11(A,B,K,L,P,Q,R,S,N,T,U,V), new12(R,S,O,M,E,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :- K=A, L=B, M=N, O=M, 
          new13(A,B,K,L,P,Q,R,S,T,U,N,V), new12(R,S,O,M,E,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :- K=A, L=B, M=N, O=M, 
          new14(A,B,K,L,P,Q,R,S,T,U,V,N), new12(R,S,O,M,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=B, L= 0, new9(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=A, L= 0, new7(A,B,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- K=< 2147483647, K=B, L= 2147483647, 
          new6(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=B, L= 0, new5(A,B,C,D,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- K=< 2147483647, K=A, L= 2147483647, 
          new4(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=A, L= 0, new3(A,B,C,D,E,F,G,H,I,J).
new1(A,B,C,D) :- new2(A,B,E,F,G,C,D,H,I,J).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
