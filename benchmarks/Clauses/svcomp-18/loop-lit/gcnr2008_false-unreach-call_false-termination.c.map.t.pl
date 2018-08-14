new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R, Q=:=C, R=:=100*T, S=:=100, T=:=A,
          U=:= -1*W, V=:= -1, W=:=B, new39(A,U,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=C, R=:=100*T, S=:=100,
          T=:=A, new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R+1, Q=:=B, R=:=10*T, S=:=10,
          T=:=D, new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=<R, Q=:=B, R=:=10*T, S=:=10, T=:=D,
          new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R, Q=:=A, R=:=4, S=:=T+U, T=:=A, 
          U=:=1, V=:=W+X, W=:=B, X=:=1, new39(S,V,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=A, R=:=4, 
          new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R+1, Q=:=E, R=:=0,
          new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=E, R=:=0,
          new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=:=R, Q=:=E, R=:=0, 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new41(A,B,C,D,Q,F,G,H,I,J,K,L,M,N,O,P).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=:=R+S, R=:=D, S=:=1, T=:=U+V, 
          U=:=C, V=:=10, new26(A,B,T,Q,E,F,G,H,I,J,K,L,M,N,O,P).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R+1, Q=:=F, R=:=0, S=:=T+U, T=:=A,
          U=:=1, V=:=W+X, W=:=B, X=:=100, 
          new39(S,V,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=F, R=:=0, S=:=T+U, T=:=A,
          U=:=1, V=:=W+X, W=:=B, X=:=100, 
          new39(S,V,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=:=R, Q=:=F, R=:=0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new38(A,B,C,D,E,Q,G,H,I,J,K,L,M,N,O,P).
new36(A,B,C,C).
new34(A,B,C,D) :- E>=F+1, E=:=C, F=:=0, new36(A,B,C,D).
new34(A,B,C,D) :- E+1=<F, E=:=C, F=:=0, new36(A,B,C,D).
new33(A,B,C,D,E,F,G,H,A,B,C,D,E,F,G,H) :- I=:=H, new34(A,B,I,J).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=<R, Q=:=B, R=:=2, S=:=1, 
          new33(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R+1, Q=:=B, R=:=2, S=:=0, 
          new33(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R, Q=:=A, R=:=4, 
          new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=A, R=:=4, S=:=0, 
          new33(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=B, R=:=10000, 
          new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R, Q=:=B, R=:=10000, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R+1, Q=:=G, R=:=0,
          new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=G, R=:=0,
          new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=:=R, Q=:=G, R=:=0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new28(A,B,C,D,E,F,Q,H,I,J,K,L,M,N,O,P).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=:=B, R=:=0, S=:=R, 
          new26(Q,B,S,R,E,F,G,H,I,J,K,L,M,N,O,P).
new23(A,B,C,D) :- new25(A,B,E,F,G,H,I,J,C,D,K,L,M,N,O,P).
safe :- init(A,B), new23(A,B,C,D).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R, Q=:=C, R=:=100*T, S=:=100, T=:=A,
          U=:= -1*W, V=:= -1, W=:=B, new17(A,U,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=C, R=:=100*T, S=:=100,
          T=:=A, new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R+1, Q=:=B, R=:=10*T, S=:=10,
          T=:=D, new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=<R, Q=:=B, R=:=10*T, S=:=10, T=:=D,
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R, Q=:=A, R=:=4, S=:=T+U, T=:=A, 
          U=:=1, V=:=W+X, W=:=B, X=:=1, new17(S,V,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=A, R=:=4, 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R+1, Q=:=E, R=:=0,
          new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=E, R=:=0,
          new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=:=R, Q=:=E, R=:=0, 
          new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new19(A,B,C,D,Q,F,G,H,I,J,K,L,M,N,O,P).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=:=R+S, R=:=D, S=:=1, T=:=U+V, 
          U=:=C, V=:=10, new3(A,B,T,Q,E,F,G,H,I,J,K,L,M,N,O,P).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R+1, Q=:=F, R=:=0, S=:=T+U, T=:=A,
          U=:=1, V=:=W+X, W=:=B, X=:=100, 
          new17(S,V,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=F, R=:=0, S=:=T+U, T=:=A,
          U=:=1, V=:=W+X, W=:=B, X=:=100, 
          new17(S,V,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=:=R, Q=:=F, R=:=0, 
          new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new16(A,B,C,D,E,Q,G,H,I,J,K,L,M,N,O,P).
new14(A,B).
new12(A,B,C,C) :- new14(A,B).
new11(A,B,C,D) :- E=:=F, E=:=C, F=:=0, new12(A,B,C,D).
new10(A,B,C,D,E,F,G,H,A,B,C,D,E,F,G,H) :- I=:=H, new11(A,B,I,J).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=<R, Q=:=B, R=:=2, S=:=1, 
          new10(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R+1, Q=:=B, R=:=2, S=:=0, 
          new10(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R, Q=:=A, R=:=4, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=A, R=:=4, S=:=0, 
          new10(A,B,C,D,E,F,G,S,I,J,K,L,M,N,O,P).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=B, R=:=10000, 
          new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R, Q=:=B, R=:=10000, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R+1, Q=:=G, R=:=0,
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+1=<R, Q=:=G, R=:=0,
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=:=R, Q=:=G, R=:=0, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new5(A,B,C,D,E,F,Q,H,I,J,K,L,M,N,O,P).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=:=B, R=:=0, S=:=R, 
          new3(Q,B,S,R,E,F,G,H,I,J,K,L,M,N,O,P).
new1(A,B,C,D) :- new2(A,B,E,F,G,H,I,J,C,D,K,L,M,N,O,P).
init(A,B). % :- A=:=0, B=:=0.
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
