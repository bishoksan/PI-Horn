new75(A,B,C,D,E,F,A,B,C,D,E,F) :- G+ 1=< -1000000, G=B, H= -1000000.
new58(A,B,C,C).
new56(A,B,C,D) :- E>= 1, E=C, F= 0, new58(A,B,C,D).
new56(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new58(A,B,C,D).
new55(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H=< 0, H=B, I= 0, new56(A,B,G,J).
new55(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>= 1, H=B, I= 0, new56(A,B,G,J).
new54(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=B, N= 0, O=P-Q, P=B, Q=A, 
          new53(A,O,C,D,E,F,G,H,I,J,K,L).
new54(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=B, N= 0, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L).
new53(A,B,C,D,E,F,G,H,I,J,K,L) :- new54(A,B,C,D,E,F,G,H,I,J,K,L).
new52(A,B,C,D,E,F,G,H,I,J,K,L) :- new53(A,B,C,D,E,F,G,H,I,J,K,L).
new51(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, O=P- 1, P=A, Q= 1, 
          new52(O,B,C,D,E,F,G,H,I,J,K,L).
new51(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, O=P- 1, P=A, Q= 1, 
          new52(O,B,C,D,E,F,G,H,I,J,K,L).
new51(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new52(A,B,C,D,E,F,G,H,I,J,K,L).
new50(A,B,C,D,E,F,G,H,I,J,K,L) :- new24(A,B,M,N,O,P,Q,R,S,T,U,V,W,X), 
          new51(R,S,C,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, 
          new50(A,B,C,D,E,F,G,H,I,J,K,L).
new49(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new51(A,B,C,D,E,F,G,H,I,J,K,L).
new48(A,B,C,D,E,F,G,H,I,J,K,L) :- new49(A,B,C,D,M,F,G,H,I,J,K,L).
new47(A,B,C,D,E,F,G,H,I,J,K,L) :- new48(A,B,M,D,E,F,G,H,I,J,K,L).
new46(A,B,C,D,E,F,G,H,I,J,K,L) :- new47(A,B,C,D,E,F,G,H,I,J,K,L).
new45(A,B,C,D,E,F,G,H,I,J,K,L) :- new24(A,B,M,N,O,P,Q,R,S,T,U,V,W,X), 
          new46(R,S,C,D,E,F,G,H,I,J,K,L).
new44(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new44(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, 
          new45(A,B,C,D,E,F,G,H,I,J,K,L).
new44(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new46(A,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, O=P- 1, P=A, Q= 1, 
          new44(O,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, O=P- 1, P=A, Q= 1, 
          new44(O,B,C,D,E,F,G,H,I,J,K,L).
new43(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L).
new42(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N, new43(A,B,C,D,M,N,G,H,I,J,K,L).
new41(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N, new42(A,B,M,N,E,F,G,H,I,J,K,L).
new40(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= -1000000, M=B, N= -1000000, 
          new41(A,B,C,D,E,F,G,H,I,J,K,L).
new39(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 1000000, M=B, N= 1000000, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L).
new38(A,B,C,D,E,F,A,B,C,D,E,F) :- G>= 1000001, G=B, H= 1000000.
new37(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 1000000, M=B, N= 1000000, 
          new75(A,B,C,D,E,F,G,H,I,J,K,L).
new35(A,B,C,D) :- new37(A,B,E,F,G,H,C,D,I,J,K,L).
new35(A,B,C,D) :- new38(A,B,E,F,G,H,C,D,I,J,K,L).
new35(A,B,C,D) :- new39(A,B,E,F,G,H,C,D,I,J,K,L).
safe :- init(A,B), new35(A,B,C,D).
new29(A,B,C,D,E,F,G,A,B,C,D,E,F,G).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=F, P= 0, Q=R- 1, R=C, S= 1, 
          new29(A,B,Q,D,E,F,G,H,I,J,K,L,M,N).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=F, P= 0, Q=R- 1, R=C, S= 1, 
          new29(A,B,Q,D,E,F,G,H,I,J,K,L,M,N).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=F, P= 0, Q=R+ 10, R=C, S= 10, 
          new29(A,B,Q,D,E,F,G,H,I,J,K,L,M,N).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=D, P= 0, Q=R+ 1, R=C, S= 1, 
          new28(A,B,Q,D,E,F,G,H,I,J,K,L,M,N).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=D, P= 0, Q=R+ 1, R=C, S= 1, 
          new28(A,B,Q,D,E,F,G,H,I,J,K,L,M,N).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=D, P= 0, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=P, new27(A,B,C,D,E,O,P,H,I,J,K,L,M,N).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=P, new26(A,B,C,O,P,F,G,H,I,J,K,L,M,N).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, new25(A,B,O,D,E,F,G,H,I,J,K,L,M,N).
new22(A,B).
new20(A,B,C,C) :- new22(A,B).
new19(A,B,C,D) :- E= 0, E=C, F= 0, new20(A,B,C,D).
new18(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 1, H=< 0, H=B, I= 0, new19(A,B,G,J).
new18(A,B,C,D,E,F,A,B,C,D,E,F) :- G= 0, H>= 1, H=B, I= 0, new19(A,B,G,J).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=B, N= 0, O=P-Q, P=B, Q=A, 
          new16(A,O,C,D,E,F,G,H,I,J,K,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 0, M=B, N= 0, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L).
new16(A,B,C,D,E,F,G,H,I,J,K,L) :- new17(A,B,C,D,E,F,G,H,I,J,K,L).
new15(A,B,C,D,E,F,G,H,I,J,K,L) :- new16(A,B,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, O=P- 1, P=A, Q= 1, 
          new15(O,B,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, O=P- 1, P=A, Q= 1, 
          new15(O,B,C,D,E,F,G,H,I,J,K,L).
new14(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, 
          new15(A,B,C,D,E,F,G,H,I,J,K,L).
new13(A,B,C,D,E,F,G,H,I,J,K,L) :- new24(A,B,M,N,O,P,Q,R,S,T,U,V,W,X), 
          new14(R,S,C,D,E,F,G,H,I,J,K,L).
new12(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, 
          new13(A,B,C,D,E,F,G,H,I,J,K,L).
new12(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, 
          new13(A,B,C,D,E,F,G,H,I,J,K,L).
new12(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L).
new11(A,B,C,D,E,F,G,H,I,J,K,L) :- new12(A,B,C,D,M,F,G,H,I,J,K,L).
new10(A,B,C,D,E,F,G,H,I,J,K,L) :- new11(A,B,M,D,E,F,G,H,I,J,K,L).
new9(A,B,C,D,E,F,G,H,I,J,K,L) :- new10(A,B,C,D,E,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- new24(A,B,M,N,O,P,Q,R,S,T,U,V,W,X), 
          new9(R,S,C,D,E,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=E, N= 0, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=E, N= 0, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=E, N= 0, new9(A,B,C,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= 1, M=C, N= 0, O=P- 1, P=A, Q= 1, 
          new7(O,B,C,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M+ 1=< 0, M=C, N= 0, O=P- 1, P=A, Q= 1, 
          new7(O,B,C,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M= 0, M=C, N= 0, new7(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N, new6(A,B,C,D,M,N,G,H,I,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- M=N, new5(A,B,M,N,E,F,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- M>= -1000000, M=B, N= -1000000, 
          new4(A,B,C,D,E,F,G,H,I,J,K,L).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- M=< 1000000, M=B, N= 1000000, 
          new3(A,B,C,D,E,F,G,H,I,J,K,L).
new1(A,B,C,D) :- new2(A,B,E,F,G,H,C,D,I,J,K,L).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
