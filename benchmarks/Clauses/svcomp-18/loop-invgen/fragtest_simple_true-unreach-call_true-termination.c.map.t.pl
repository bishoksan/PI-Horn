new44(A,B,C,D,E,F,G,H,A,B,C,D,E,F,G,H) :- I>=J, I=H, J=E.
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+ 1=<R, Q=H, R=E, 
          new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 1, R>= 0, R=A, S= 0, T=U- 1, U=A, 
          V= 1, W=X- 1, X=B, Y= 1, Z=A1+ 1, A1=H, B1= 1, new19(A,Q,C1), 
          new44(T,W,C,D,E,F,G,Z,I,J,K,L,M,N,O,P).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 0, R+ 1=< 0, R=A, S= 0, T=U- 1, 
          U=A, V= 1, W=X- 1, X=B, Y= 1, Z=A1+ 1, A1=H, B1= 1, new19(A,Q,C1), 
          new44(T,W,C,D,E,F,G,Z,I,J,K,L,M,N,O,P).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 0, R=B, 
          new42(A,B,C,D,R,F,G,Q,I,J,K,L,M,N,O,P).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=< 1000000, Q=B, R= 1000000, S=B, 
          T=U+ 1, U=B, V= 1, W=X+ 1, X=A, Y= 1, 
          new37(W,T,C,S,E,F,G,H,I,J,K,L,M,N,O,P).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>= 1000001, Q=B, R= 1000000, 
          new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>= 1, Q=G, R= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+ 1=< 0, Q=G, R= 0, 
          new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 0, Q=G, R= 0, 
          new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new39(A,B,C,D,E,F,Q,H,I,J,K,L,M,N,O,P).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 0, 
          new37(A,Q,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R+ 1, Q=B, R=C, S=B, 
          new36(A,B,S,D,E,F,G,H,I,J,K,L,M,N,O,P).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=<R, Q=B, R=C, 
          new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=< 1000000, Q=B, R= 1000000, S=T+ 1, 
          T=B, U= 1, new30(A,S,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>= 1000001, Q=B, R= 1000000, 
          new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>= 1, Q=F, R= 0, 
          new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+ 1=< 0, Q=F, R= 0, 
          new33(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 0, Q=F, R= 0, 
          new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new32(A,B,C,D,E,Q,G,H,I,J,K,L,M,N,O,P).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new30(A,B,Q,D,E,F,G,H,I,J,K,L,M,N,O,P).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 0, 
          new29(A,Q,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new26(A,B) :- new28(A,C,D,E,F,G,H,I,B,J,K,L,M,N,O,P).
safe :- init(A), new26(A,B).
new25(A).
new23(A,B,B) :- new25(A).
new22(A,B,B).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+ 1=<R, Q=H, R=E, 
          new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new19(A,B,C) :- D>= 1, D=B, E= 0, new22(A,B,C).
new19(A,B,C) :- D+ 1=< 0, D=B, E= 0, new22(A,B,C).
new18(A,B,C) :- D= 0, D=B, E= 0, new23(A,B,C).
new17(A,B,C,D,E,F,G,H,A,B,C,D,E,F,G,H) :- I= 1, J>= 0, J=A, K= 0, new18(A,I,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 1, R>= 0, R=A, S= 0, T=U- 1, U=A, 
          V= 1, W=X- 1, X=B, Y= 1, Z=A1+ 1, A1=H, B1= 1, new19(A,Q,C1), 
          new20(T,W,C,D,E,F,G,Z,I,J,K,L,M,N,O,P).
new17(A,B,C,D,E,F,G,H,A,B,C,D,E,F,G,H) :- I= 0, J+ 1=< 0, J=A, K= 0, 
          new18(A,I,L).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 0, R+ 1=< 0, R=A, S= 0, T=U- 1, 
          U=A, V= 1, W=X- 1, X=B, Y= 1, Z=A1+ 1, A1=H, B1= 1, new19(A,Q,C1), 
          new20(T,W,C,D,E,F,G,Z,I,J,K,L,M,N,O,P).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 0, R=B, 
          new16(A,B,C,D,R,F,G,Q,I,J,K,L,M,N,O,P).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=< 1000000, Q=B, R= 1000000, S=B, 
          T=U+ 1, U=B, V= 1, W=X+ 1, X=A, Y= 1, 
          new11(W,T,C,S,E,F,G,H,I,J,K,L,M,N,O,P).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>= 1000001, Q=B, R= 1000000, 
          new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>= 1, Q=G, R= 0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+ 1=< 0, Q=G, R= 0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 0, Q=G, R= 0, 
          new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new13(A,B,C,D,E,F,Q,H,I,J,K,L,M,N,O,P).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- 
          new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 0, 
          new11(A,Q,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>=R+ 1, Q=B, R=C, S=B, 
          new10(A,B,S,D,E,F,G,H,I,J,K,L,M,N,O,P).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=<R, Q=B, R=C, 
          new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q=< 1000000, Q=B, R= 1000000, S=T+ 1, 
          T=B, U= 1, new4(A,S,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>= 1000001, Q=B, R= 1000000, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q>= 1, Q=F, R= 0, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q+ 1=< 0, Q=F, R= 0, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 0, Q=F, R= 0, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new6(A,B,C,D,E,Q,G,H,I,J,K,L,M,N,O,P).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- new4(A,B,Q,D,E,F,G,H,I,J,K,L,M,N,O,P).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- Q= 0, 
          new3(A,Q,C,D,E,F,G,H,I,J,K,L,M,N,O,P).
new1(A,B) :- new2(A,C,D,E,F,G,H,I,B,J,K,L,M,N,O,P).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
