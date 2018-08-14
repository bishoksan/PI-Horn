new162(A,B,C,D,E,F,A,B,C,D,E,F) :- G>= 1, G=B, H= 0.
new162(A,B,C,D,E,F,A,B,C,D,E,F) :- G+ 1=< 0, G=B, H= 0.
new161(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 9, M=A, N= 9, O=P+ 1, P=A, Q= 1, R=S+ 3, 
          S=B, T= 3, new145(O,R,C,D,E,F,G,H,I,J,K,L).
new160(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 7, N=A, O= 7, P= 0, Q=R+ 2, R=A, 
          S= 2, T=U+ 1, U=B, V= 1, new145(Q,T,C,D,E,F,G,H,I,J,K,L).
new159(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 4, N=A, O= 4, P= 0, Q=R+ 1, R=A, 
          S= 1, T=U+ 2, U=B, V= 2, new145(Q,T,C,D,E,F,G,H,I,J,K,L).
new158(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 0, M=N- 5, N=A, O= 5, P= 0, 
          new160(A,B,C,D,E,F,G,H,I,J,K,L).
new157(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new158(A,B,C,D,E,F,G,H,I,J,K,L).
new157(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, 
          new158(A,B,C,D,E,F,G,H,I,J,K,L).
new157(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new159(A,B,C,D,E,F,G,H,I,J,K,L).
new156(A,B,C,D,E,F,G,H,I,J,K,L) :- new157(A,B,M,D,E,F,G,H,I,J,K,L).
new155(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 7, M=A, N= 7, 
          new161(A,B,C,D,E,F,G,H,I,J,K,L).
new154(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new155(A,B,C,D,E,F,G,H,I,J,K,L).
new154(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=D, N= 0, 
          new155(A,B,C,D,E,F,G,H,I,J,K,L).
new154(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=D, N= 0, 
          new156(A,B,C,D,E,F,G,H,I,J,K,L).
new153(A,B,C,D,E,F,G,H,I,J,K,L) :- new154(A,B,C,M,E,F,G,H,I,J,K,L).
new152(A,B,C,D,E,F,A,B,C,D,E,F) :- G+ 1=< 9, G=A, H= 9.
new152(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 9, M=A, N= 9, O=P+ 2, P=A, Q= 2, R=S+ 1, 
          S=B, T= 1, new145(O,R,C,D,E,F,G,H,I,J,K,L).
new151(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new152(A,B,C,D,E,F,G,H,I,J,K,L).
new151(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, 
          new152(A,B,C,D,E,F,G,H,I,J,K,L).
new151(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new153(A,B,C,D,E,F,G,H,I,J,K,L).
new148(A,B,C,D,E,F,G,H,I,J,K,L) :- new151(A,B,C,D,M,F,G,H,I,J,K,L).
new147(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new148(A,B,C,D,E,F,G,H,I,J,K,L).
new147(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new148(A,B,C,D,E,F,G,H,I,J,K,L).
new146(A,B,C,D,E,F,G,H,I,J,K,L) :- new147(A,B,C,D,E,M,G,H,I,J,K,L).
new145(A,B,C,D,E,F,G,H,I,J,K,L) :- new146(A,B,C,D,E,F,G,H,I,J,K,L).
new144(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=B, N= 0, 
          new145(A,B,C,D,E,F,G,H,I,J,K,L).
new143(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 9, M=A, N= 9, O=P+ 1, P=A, Q= 1, R=S+ 3, 
          S=B, T= 3, new127(O,R,C,D,E,F,G,H,I,J,K,L).
new142(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 7, N=A, O= 7, P= 0, Q=R+ 2, R=A, 
          S= 2, T=U+ 1, U=B, V= 1, new127(Q,T,C,D,E,F,G,H,I,J,K,L).
new141(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 4, N=A, O= 4, P= 0, Q=R+ 1, R=A, 
          S= 1, T=U+ 2, U=B, V= 2, new127(Q,T,C,D,E,F,G,H,I,J,K,L).
new140(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 0, M=N- 5, N=A, O= 5, P= 0, 
          new142(A,B,C,D,E,F,G,H,I,J,K,L).
new139(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new140(A,B,C,D,E,F,G,H,I,J,K,L).
new139(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, 
          new140(A,B,C,D,E,F,G,H,I,J,K,L).
new139(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new141(A,B,C,D,E,F,G,H,I,J,K,L).
new138(A,B,C,D,E,F,G,H,I,J,K,L) :- new139(A,B,M,D,E,F,G,H,I,J,K,L).
new137(A,B,C,D,E,F,A,B,C,D,E,F) :- G+ 1=< 7, G=A, H= 7.
new137(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 7, M=A, N= 7, 
          new143(A,B,C,D,E,F,G,H,I,J,K,L).
new136(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new137(A,B,C,D,E,F,G,H,I,J,K,L).
new136(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=D, N= 0, 
          new137(A,B,C,D,E,F,G,H,I,J,K,L).
new136(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=D, N= 0, 
          new138(A,B,C,D,E,F,G,H,I,J,K,L).
new135(A,B,C,D,E,F,G,H,I,J,K,L) :- new136(A,B,C,M,E,F,G,H,I,J,K,L).
new134(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 9, M=A, N= 9, O=P+ 2, P=A, Q= 2, R=S+ 1, 
          S=B, T= 1, new127(O,R,C,D,E,F,G,H,I,J,K,L).
new133(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new134(A,B,C,D,E,F,G,H,I,J,K,L).
new133(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, 
          new134(A,B,C,D,E,F,G,H,I,J,K,L).
new133(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new135(A,B,C,D,E,F,G,H,I,J,K,L).
new130(A,B,C,D,E,F,G,H,I,J,K,L) :- new133(A,B,C,D,M,F,G,H,I,J,K,L).
new129(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new130(A,B,C,D,E,F,G,H,I,J,K,L).
new129(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new130(A,B,C,D,E,F,G,H,I,J,K,L).
new128(A,B,C,D,E,F,G,H,I,J,K,L) :- new129(A,B,C,D,E,M,G,H,I,J,K,L).
new127(A,B,C,D,E,F,G,H,I,J,K,L) :- new128(A,B,C,D,E,F,G,H,I,J,K,L).
new126(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=B, N= 0, 
          new127(A,B,C,D,E,F,G,H,I,J,K,L).
new125(A,B,C,D,E,F,A,B,C,D,E,F) :- G>= 10, G=A, H= 9.
new125(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 9, M=A, N= 9, O=P+ 1, P=A, Q= 1, R=S+ 3, 
          S=B, T= 3, new109(O,R,C,D,E,F,G,H,I,J,K,L).
new124(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 7, N=A, O= 7, P= 0, Q=R+ 2, R=A, 
          S= 2, T=U+ 1, U=B, V= 1, new109(Q,T,C,D,E,F,G,H,I,J,K,L).
new123(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 4, N=A, O= 4, P= 0, Q=R+ 1, R=A, 
          S= 1, T=U+ 2, U=B, V= 2, new109(Q,T,C,D,E,F,G,H,I,J,K,L).
new122(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 0, M=N- 5, N=A, O= 5, P= 0, 
          new124(A,B,C,D,E,F,G,H,I,J,K,L).
new121(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new122(A,B,C,D,E,F,G,H,I,J,K,L).
new121(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, 
          new122(A,B,C,D,E,F,G,H,I,J,K,L).
new121(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new123(A,B,C,D,E,F,G,H,I,J,K,L).
new120(A,B,C,D,E,F,G,H,I,J,K,L) :- new121(A,B,M,D,E,F,G,H,I,J,K,L).
new119(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 7, M=A, N= 7, 
          new125(A,B,C,D,E,F,G,H,I,J,K,L).
new118(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new119(A,B,C,D,E,F,G,H,I,J,K,L).
new118(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=D, N= 0, 
          new119(A,B,C,D,E,F,G,H,I,J,K,L).
new118(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=D, N= 0, 
          new120(A,B,C,D,E,F,G,H,I,J,K,L).
new117(A,B,C,D,E,F,G,H,I,J,K,L) :- new118(A,B,C,M,E,F,G,H,I,J,K,L).
new116(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 9, M=A, N= 9, O=P+ 2, P=A, Q= 2, R=S+ 1, 
          S=B, T= 1, new109(O,R,C,D,E,F,G,H,I,J,K,L).
new115(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new116(A,B,C,D,E,F,G,H,I,J,K,L).
new115(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, 
          new116(A,B,C,D,E,F,G,H,I,J,K,L).
new115(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new117(A,B,C,D,E,F,G,H,I,J,K,L).
new112(A,B,C,D,E,F,G,H,I,J,K,L) :- new115(A,B,C,D,M,F,G,H,I,J,K,L).
new111(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new112(A,B,C,D,E,F,G,H,I,J,K,L).
new111(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new112(A,B,C,D,E,F,G,H,I,J,K,L).
new110(A,B,C,D,E,F,G,H,I,J,K,L) :- new111(A,B,C,D,E,M,G,H,I,J,K,L).
new109(A,B,C,D,E,F,G,H,I,J,K,L) :- new110(A,B,C,D,E,F,G,H,I,J,K,L).
new108(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=B, N= 0, 
          new109(A,B,C,D,E,F,G,H,I,J,K,L).
new107(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 9, M=A, N= 9, O=P+ 1, P=A, Q= 1, R=S+ 3, 
          S=B, T= 3, new91(O,R,C,D,E,F,G,H,I,J,K,L).
new106(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 7, N=A, O= 7, P= 0, Q=R+ 2, R=A, 
          S= 2, T=U+ 1, U=B, V= 1, new91(Q,T,C,D,E,F,G,H,I,J,K,L).
new105(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 4, N=A, O= 4, P= 0, Q=R+ 1, R=A, 
          S= 1, T=U+ 2, U=B, V= 2, new91(Q,T,C,D,E,F,G,H,I,J,K,L).
new104(A,B,C,D,E,F,A,B,C,D,E,F) :- G+ 1=< 0, G=H- 5, H=A, I= 5, J= 0.
new104(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 0, M=N- 5, N=A, O= 5, P= 0, 
          new106(A,B,C,D,E,F,G,H,I,J,K,L).
new103(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new104(A,B,C,D,E,F,G,H,I,J,K,L).
new103(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, 
          new104(A,B,C,D,E,F,G,H,I,J,K,L).
new103(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new105(A,B,C,D,E,F,G,H,I,J,K,L).
new102(A,B,C,D,E,F,G,H,I,J,K,L) :- new103(A,B,M,D,E,F,G,H,I,J,K,L).
new101(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 7, M=A, N= 7, 
          new107(A,B,C,D,E,F,G,H,I,J,K,L).
new100(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new101(A,B,C,D,E,F,G,H,I,J,K,L).
new100(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=D, N= 0, 
          new101(A,B,C,D,E,F,G,H,I,J,K,L).
new100(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=D, N= 0, 
          new102(A,B,C,D,E,F,G,H,I,J,K,L).
new99(A,B,C,D,E,F,G,H,I,J,K,L) :- new100(A,B,C,M,E,F,G,H,I,J,K,L).
new98(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 9, M=A, N= 9, O=P+ 2, P=A, Q= 2, R=S+ 1, 
          S=B, T= 1, new91(O,R,C,D,E,F,G,H,I,J,K,L).
new97(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new98(A,B,C,D,E,F,G,H,I,J,K,L).
new97(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, 
          new98(A,B,C,D,E,F,G,H,I,J,K,L).
new97(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new99(A,B,C,D,E,F,G,H,I,J,K,L).
new94(A,B,C,D,E,F,G,H,I,J,K,L) :- new97(A,B,C,D,M,F,G,H,I,J,K,L).
new93(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new94(A,B,C,D,E,F,G,H,I,J,K,L).
new93(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new94(A,B,C,D,E,F,G,H,I,J,K,L).
new92(A,B,C,D,E,F,G,H,I,J,K,L) :- new93(A,B,C,D,E,M,G,H,I,J,K,L).
new91(A,B,C,D,E,F,G,H,I,J,K,L) :- new92(A,B,C,D,E,F,G,H,I,J,K,L).
new90(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=B, N= 0, 
          new91(A,B,C,D,E,F,G,H,I,J,K,L).
new89(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 9, M=A, N= 9, O=P+ 1, P=A, Q= 1, R=S+ 3, 
          S=B, T= 3, new73(O,R,C,D,E,F,G,H,I,J,K,L).
new88(A,B,C,D,E,F,A,B,C,D,E,F) :- G>= 1, G=H- 7, H=A, I= 7, J= 0.
new88(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 7, N=A, O= 7, P= 0, Q=R+ 2, R=A, 
          S= 2, T=U+ 1, U=B, V= 1, new73(Q,T,C,D,E,F,G,H,I,J,K,L).
new87(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 4, N=A, O= 4, P= 0, Q=R+ 1, R=A, 
          S= 1, T=U+ 2, U=B, V= 2, new73(Q,T,C,D,E,F,G,H,I,J,K,L).
new86(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 0, M=N- 5, N=A, O= 5, P= 0, 
          new88(A,B,C,D,E,F,G,H,I,J,K,L).
new85(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new86(A,B,C,D,E,F,G,H,I,J,K,L).
new85(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, 
          new86(A,B,C,D,E,F,G,H,I,J,K,L).
new85(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new87(A,B,C,D,E,F,G,H,I,J,K,L).
new84(A,B,C,D,E,F,G,H,I,J,K,L) :- new85(A,B,M,D,E,F,G,H,I,J,K,L).
new83(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 7, M=A, N= 7, 
          new89(A,B,C,D,E,F,G,H,I,J,K,L).
new82(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new83(A,B,C,D,E,F,G,H,I,J,K,L).
new82(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=D, N= 0, 
          new83(A,B,C,D,E,F,G,H,I,J,K,L).
new82(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=D, N= 0, 
          new84(A,B,C,D,E,F,G,H,I,J,K,L).
new81(A,B,C,D,E,F,G,H,I,J,K,L) :- new82(A,B,C,M,E,F,G,H,I,J,K,L).
new80(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 9, M=A, N= 9, O=P+ 2, P=A, Q= 2, R=S+ 1, 
          S=B, T= 1, new73(O,R,C,D,E,F,G,H,I,J,K,L).
new79(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new80(A,B,C,D,E,F,G,H,I,J,K,L).
new79(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, 
          new80(A,B,C,D,E,F,G,H,I,J,K,L).
new79(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new81(A,B,C,D,E,F,G,H,I,J,K,L).
new76(A,B,C,D,E,F,G,H,I,J,K,L) :- new79(A,B,C,D,M,F,G,H,I,J,K,L).
new75(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new76(A,B,C,D,E,F,G,H,I,J,K,L).
new75(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new76(A,B,C,D,E,F,G,H,I,J,K,L).
new74(A,B,C,D,E,F,G,H,I,J,K,L) :- new75(A,B,C,D,E,M,G,H,I,J,K,L).
new73(A,B,C,D,E,F,G,H,I,J,K,L) :- new74(A,B,C,D,E,F,G,H,I,J,K,L).
new72(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=B, N= 0, 
          new73(A,B,C,D,E,F,G,H,I,J,K,L).
new71(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 9, M=A, N= 9, O=P+ 1, P=A, Q= 1, R=S+ 3, 
          S=B, T= 3, new55(O,R,C,D,E,F,G,H,I,J,K,L).
new70(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 7, N=A, O= 7, P= 0, Q=R+ 2, R=A, 
          S= 2, T=U+ 1, U=B, V= 1, new55(Q,T,C,D,E,F,G,H,I,J,K,L).
new69(A,B,C,D,E,F,A,B,C,D,E,F) :- G>= 1, G=H- 4, H=A, I= 4, J= 0.
new69(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 4, N=A, O= 4, P= 0, Q=R+ 1, R=A, 
          S= 1, T=U+ 2, U=B, V= 2, new55(Q,T,C,D,E,F,G,H,I,J,K,L).
new68(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 0, M=N- 5, N=A, O= 5, P= 0, 
          new70(A,B,C,D,E,F,G,H,I,J,K,L).
new67(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L).
new67(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L).
new67(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new69(A,B,C,D,E,F,G,H,I,J,K,L).
new66(A,B,C,D,E,F,G,H,I,J,K,L) :- new67(A,B,M,D,E,F,G,H,I,J,K,L).
new65(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 7, M=A, N= 7, 
          new71(A,B,C,D,E,F,G,H,I,J,K,L).
new64(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new65(A,B,C,D,E,F,G,H,I,J,K,L).
new64(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=D, N= 0, 
          new65(A,B,C,D,E,F,G,H,I,J,K,L).
new64(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=D, N= 0, 
          new66(A,B,C,D,E,F,G,H,I,J,K,L).
new63(A,B,C,D,E,F,G,H,I,J,K,L) :- new64(A,B,C,M,E,F,G,H,I,J,K,L).
new62(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 9, M=A, N= 9, O=P+ 2, P=A, Q= 2, R=S+ 1, 
          S=B, T= 1, new55(O,R,C,D,E,F,G,H,I,J,K,L).
new61(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new62(A,B,C,D,E,F,G,H,I,J,K,L).
new61(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, 
          new62(A,B,C,D,E,F,G,H,I,J,K,L).
new61(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L).
new58(A,B,C,D,E,F,G,H,I,J,K,L) :- new61(A,B,C,D,M,F,G,H,I,J,K,L).
new57(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L).
new57(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L).
new56(A,B,C,D,E,F,G,H,I,J,K,L) :- new57(A,B,C,D,E,M,G,H,I,J,K,L).
new55(A,B,C,D,E,F,G,H,I,J,K,L) :- new56(A,B,C,D,E,F,G,H,I,J,K,L).
new54(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=B, N= 0, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L).
new53(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 9, M=A, N= 9, O=P+ 1, P=A, Q= 1, R=S+ 3, 
          S=B, T= 3, new37(O,R,C,D,E,F,G,H,I,J,K,L).
new52(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 7, N=A, O= 7, P= 0, Q=R+ 2, R=A, 
          S= 2, T=U+ 1, U=B, V= 1, new37(Q,T,C,D,E,F,G,H,I,J,K,L).
new51(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 4, N=A, O= 4, P= 0, Q=R+ 1, R=A, 
          S= 1, T=U+ 2, U=B, V= 2, new37(Q,T,C,D,E,F,G,H,I,J,K,L).
new50(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 0, M=N- 5, N=A, O= 5, P= 0, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new51(A,B,C,D,E,F,G,H,I,J,K,L).
new48(A,B,C,D,E,F,G,H,I,J,K,L) :- new49(A,B,M,D,E,F,G,H,I,J,K,L).
new47(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 7, M=A, N= 7, 
          new53(A,B,C,D,E,F,G,H,I,J,K,L).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new47(A,B,C,D,E,F,G,H,I,J,K,L).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=D, N= 0, 
          new47(A,B,C,D,E,F,G,H,I,J,K,L).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=D, N= 0, 
          new48(A,B,C,D,E,F,G,H,I,J,K,L).
new45(A,B,C,D,E,F,G,H,I,J,K,L) :- new46(A,B,C,M,E,F,G,H,I,J,K,L).
new44(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 9, M=A, N= 9, O=P+ 2, P=A, Q= 2, R=S+ 1, 
          S=B, T= 1, new37(O,R,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new42(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H>= 0, H=I-J, I= 3*K, L= 3, K=A, J=B, 
          M= 0, new10(A,B,G,N).
new42(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H+ 1=< 0, H=I-J, I= 3*K, L= 3, K=A, 
          J=B, M= 0, new10(A,B,G,N).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N>= 0, N=O+P, O= -1*Q, R= -1, Q=A, 
          P= 2*S, T= 2, S=B, U= 0, new10(A,B,M,V), 
          new42(A,B,C,D,E,F,G,H,I,J,K,L).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N+ 1=< 0, N=O+P, O= -1*Q, R= -1, Q=A, 
          P= 2*S, T= 2, S=B, U= 0, new10(A,B,M,V), 
          new42(A,B,C,D,E,F,G,H,I,J,K,L).
new40(A,B,C,D,E,F,G,H,I,J,K,L) :- new43(A,B,C,D,M,F,G,H,I,J,K,L).
new39(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L).
new39(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L).
new39(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=F, N= 0, 
          new41(A,B,C,D,E,F,G,H,I,J,K,L).
new38(A,B,C,D,E,F,G,H,I,J,K,L) :- new39(A,B,C,D,E,M,G,H,I,J,K,L).
new37(A,B,C,D,E,F,G,H,I,J,K,L) :- new38(A,B,C,D,E,F,G,H,I,J,K,L).
new36(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=B, N= 0, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L).
new35(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=A, N= 0, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L).
new34(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=A, N= 0, 
          new54(A,B,C,D,E,F,G,H,I,J,K,L).
new33(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=A, N= 0, 
          new72(A,B,C,D,E,F,G,H,I,J,K,L).
new32(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=A, N= 0, 
          new90(A,B,C,D,E,F,G,H,I,J,K,L).
new31(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=A, N= 0, 
          new108(A,B,C,D,E,F,G,H,I,J,K,L).
new30(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=A, N= 0, 
          new126(A,B,C,D,E,F,G,H,I,J,K,L).
new29(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=A, N= 0, 
          new144(A,B,C,D,E,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=A, N= 0, 
          new162(A,B,C,D,E,F,G,H,I,J,K,L).
new27(A,B,C,D,E,F,A,B,C,D,E,F) :- G>= 1, G=A, H= 0.
new27(A,B,C,D,E,F,A,B,C,D,E,F) :- G+ 1=< 0, G=A, H= 0.
new25(A,B,C,D) :- new27(A,B,E,F,G,H,C,D,I,J,K,L).
new25(A,B,C,D) :- new28(A,B,E,F,G,H,C,D,I,J,K,L).
new25(A,B,C,D) :- new29(A,B,E,F,G,H,C,D,I,J,K,L).
new25(A,B,C,D) :- new30(A,B,E,F,G,H,C,D,I,J,K,L).
new25(A,B,C,D) :- new31(A,B,E,F,G,H,C,D,I,J,K,L).
new25(A,B,C,D) :- new32(A,B,E,F,G,H,C,D,I,J,K,L).
new25(A,B,C,D) :- new33(A,B,E,F,G,H,C,D,I,J,K,L).
new25(A,B,C,D) :- new34(A,B,E,F,G,H,C,D,I,J,K,L).
new25(A,B,C,D) :- new35(A,B,E,F,G,H,C,D,I,J,K,L).
safe :- init(A,B), new25(A,B,C,D).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 9, M=A, N= 9, O=P+ 1, P=A, Q= 1, R=S+ 3, 
          S=B, T= 3, new4(O,R,C,D,E,F,G,H,I,J,K,L).
new23(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 7, N=A, O= 7, P= 0, Q=R+ 2, R=A, 
          S= 2, T=U+ 1, U=B, V= 1, new4(Q,T,C,D,E,F,G,H,I,J,K,L).
new22(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=N- 4, N=A, O= 4, P= 0, Q=R+ 1, R=A, 
          S= 1, T=U+ 2, U=B, V= 2, new4(Q,T,C,D,E,F,G,H,I,J,K,L).
new21(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 0, M=N- 5, N=A, O= 5, P= 0, 
          new23(A,B,C,D,E,F,G,H,I,J,K,L).
new20(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L).
new20(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L).
new20(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L).
new19(A,B,C,D,E,F,G,H,I,J,K,L) :- new20(A,B,M,D,E,F,G,H,I,J,K,L).
new18(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 7, M=A, N= 7, 
          new24(A,B,C,D,E,F,G,H,I,J,K,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=D, N= 0, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=D, N= 0, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=D, N= 0, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- new17(A,B,C,M,E,F,G,H,I,J,K,L).
new15(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 9, M=A, N= 9, O=P+ 2, P=A, Q= 2, R=S+ 1, 
          S=B, T= 1, new4(O,R,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new15(A,B,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, 
          new15(A,B,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new16(A,B,C,D,E,F,G,H,I,J,K,L).
new13(A,B,C,C).
new11(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H>= 0, H=I-J, I= 3*K, L= 3, K=A, J=B, 
          M= 0, new9(A,B,G,N).
new11(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H+ 1=< 0, H=I-J, I= 3*K, L= 3, K=A, 
          J=B, M= 0, new9(A,B,G,N).
new10(A,B,C,C) :- D>= 1, D=C, E= 0.
new10(A,B,C,C) :- D+ 1=< 0, D=C, E= 0.
new9(A,B,C,D) :- E= 0, E=C, F= 0, new13(A,B,C,D).
new8(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H>= 0, H=I+J, I= -1*K, L= -1, K=A, 
          J= 2*M, N= 2, M=B, O= 0, new9(A,B,G,P).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 1, N>= 0, N=O+P, O= -1*Q, R= -1, Q=A, 
          P= 2*S, T= 2, S=B, U= 0, new10(A,B,M,V), 
          new11(A,B,C,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H+ 1=< 0, H=I+J, I= -1*K, L= -1, K=A, 
          J= 2*M, N= 2, M=B, O= 0, new9(A,B,G,P).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, N+ 1=< 0, N=O+P, O= -1*Q, R= -1, Q=A, 
          P= 2*S, T= 2, S=B, U= 0, new10(A,B,M,V), 
          new11(A,B,C,D,E,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- new14(A,B,C,D,M,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=F, N= 0, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=F, N= 0, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=F, N= 0, new8(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- new6(A,B,C,D,E,M,G,H,I,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- new5(A,B,C,D,E,F,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=B, N= 0, new4(A,B,C,D,E,F,G,H,I,J,K,L).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=A, N= 0, new3(A,B,C,D,E,F,G,H,I,J,K,L).
new1(A,B,C,D) :- new2(A,B,E,F,G,H,C,D,I,J,K,L).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
