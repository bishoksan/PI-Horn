new34(A,B,C,D,E,F,G,H,I,J,K,L) :- M=:=N+O, N=:=C, O=:=1, 
          new24(A,B,M,D,E,F,G,H,I,J,K,L).
new33(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+1, M=:=D, N=:=0, O=:=C,
          new34(O,B,C,D,E,F,G,H,I,J,K,L).
new33(A,B,C,D,E,F,G,H,I,J,K,L) :- M+1=<N, M=:=D, N=:=0, O=:=C,
          new34(O,B,C,D,E,F,G,H,I,J,K,L).
new33(A,B,C,D,E,F,G,H,I,J,K,L) :- M=:=N, M=:=D, N=:=0, 
          new34(A,B,C,D,E,F,G,H,I,J,K,L).
new32(A,B,C,D,E,F,G,H,I,J,K,L) :- M=<N, M=:=B, N=:=0, O=:=1, 
          new31(A,B,C,D,E,O,G,H,I,J,K,L).
new32(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+1, M=:=B, N=:=0, O=:=0, 
          new31(A,B,C,D,E,O,G,H,I,J,K,L).
new31(A,B,C,D,E,F,A,B,C,D,E,F) :- G=:=F, new10(A,B,G,H).
new30(A,B,C,D,E,F,G,H,I,J,K,L) :- M+1=<N, M=:=A, N=:=B, O=:=1, 
          new31(A,B,C,D,E,O,G,H,I,J,K,L).
new30(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=:=A, N=:=B, 
          new32(A,B,C,D,E,F,G,H,I,J,K,L).
new29(A,B,C,D,E,F,G,H,I,J,K,L) :- M=<N, M=:=B, N=:=0, O=:=1, 
          new28(A,B,C,D,O,F,G,H,I,J,K,L).
new29(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+1, M=:=B, N=:=0, O=:=0, 
          new28(A,B,C,D,O,F,G,H,I,J,K,L).
new28(A,B,C,D,E,F,G,H,I,J,K,L) :- M=:=E, new10(A,B,M,N), 
          new30(A,B,C,D,E,F,G,H,I,J,K,L).
new27(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=:=A, N=:=0, O=:=1, 
          new28(A,B,C,D,O,F,G,H,I,J,K,L).
new27(A,B,C,D,E,F,G,H,I,J,K,L) :- M+1=<N, M=:=A, N=:=0, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L).
new26(A,B,C,D,E,F,G,H,I,J,K,L) :- new33(A,B,C,M,E,F,G,H,I,J,K,L).
new25(A,B,C,D,E,F,G,H,I,J,K,L) :- M+1=<N, M=:=C, N=:=B, 
          new26(A,B,C,D,E,F,G,H,I,J,K,L).
new25(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=:=C, N=:=B, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L).
new24(A,B,C,D,E,F,G,H,I,J,K,L) :- new25(A,B,C,D,E,F,G,H,I,J,K,L).
new23(A,B,C,D,E,F,G,H,I,J,K,L) :- M=:=0, new24(A,B,M,D,E,F,G,H,I,J,K,L).
new21(A,B,C,D) :- new23(A,B,E,F,G,H,C,D,I,J,K,L).
safe :- init(A,B), new21(A,B,C,D).
new20(A,B,C,D,E,F,G,H,I,J,K,L) :- M=:=N+O, N=:=C, O=:=1, 
          new3(A,B,M,D,E,F,G,H,I,J,K,L).
new19(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+1, M=:=D, N=:=0, O=:=C,
          new20(O,B,C,D,E,F,G,H,I,J,K,L).
new19(A,B,C,D,E,F,G,H,I,J,K,L) :- M+1=<N, M=:=D, N=:=0, O=:=C,
          new20(O,B,C,D,E,F,G,H,I,J,K,L).
new19(A,B,C,D,E,F,G,H,I,J,K,L) :- M=:=N, M=:=D, N=:=0, 
          new20(A,B,C,D,E,F,G,H,I,J,K,L).
new18(A,B).
new16(A,B,C,C) :- new18(A,B).
new15(A,B,C,C).
new13(A,B,C,D,E,F,G,H,I,J,K,L) :- M=<N, M=:=B, N=:=0, O=:=1, 
          new12(A,B,C,D,E,O,G,H,I,J,K,L).
new13(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+1, M=:=B, N=:=0, O=:=0, 
          new12(A,B,C,D,E,O,G,H,I,J,K,L).
new12(A,B,C,D,E,F,A,B,C,D,E,F) :- G=:=F, new9(A,B,G,H).
new11(A,B,C,D,E,F,G,H,I,J,K,L) :- M+1=<N, M=:=A, N=:=B, O=:=1, 
          new12(A,B,C,D,E,O,G,H,I,J,K,L).
new11(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=:=A, N=:=B, 
          new13(A,B,C,D,E,F,G,H,I,J,K,L).
new10(A,B,C,D) :- E>=F+1, E=:=C, F=:=0, new15(A,B,C,D).
new10(A,B,C,D) :- E+1=<F, E=:=C, F=:=0, new15(A,B,C,D).
new9(A,B,C,D) :- E=:=F, E=:=C, F=:=0, new16(A,B,C,D).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M=<N, M=:=B, N=:=0, O=:=1, 
          new7(A,B,C,D,O,F,G,H,I,J,K,L).
new8(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N+1, M=:=B, N=:=0, O=:=0, 
          new7(A,B,C,D,O,F,G,H,I,J,K,L).
new7(A,B,C,D,E,F,A,B,C,D,E,F) :- G=:=E, new9(A,B,G,H).
new7(A,B,C,D,E,F,G,H,I,J,K,L) :- M=:=E, new10(A,B,M,N), 
          new11(A,B,C,D,E,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=:=A, N=:=0, O=:=1, 
          new7(A,B,C,D,O,F,G,H,I,J,K,L).
new6(A,B,C,D,E,F,G,H,I,J,K,L) :- M+1=<N, M=:=A, N=:=0, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L).
new5(A,B,C,D,E,F,G,H,I,J,K,L) :- new19(A,B,C,M,E,F,G,H,I,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- M+1=<N, M=:=C, N=:=B, 
          new5(A,B,C,D,E,F,G,H,I,J,K,L).
new4(A,B,C,D,E,F,G,H,I,J,K,L) :- M>=N, M=:=C, N=:=B, 
          new6(A,B,C,D,E,F,G,H,I,J,K,L).
new3(A,B,C,D,E,F,G,H,I,J,K,L) :- new4(A,B,C,D,E,F,G,H,I,J,K,L).
new2(A,B,C,D,E,F,G,H,I,J,K,L) :- M=:=0, new3(A,B,M,D,E,F,G,H,I,J,K,L).
new1(A,B,C,D) :- new2(A,B,E,F,G,H,C,D,I,J,K,L).
init(A,B). % :- A=:=0, B=:=0.
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
