new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P+ 1, O=D, P=B, Q= -1*R, S= -1, R=A, 
          T= -1*U, V= -1, U=B, new19(Q,T,C,D,E,F,G,H,I,J,K,L,M,N).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=<P, O=D, P=B, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P, O=A, P=C, 
          new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=<P, O=A, P=C, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 4, O=A, P= 4, Q=R+ 1, R=A, S= 1, 
          T=U+ 3, U=B, V= 3, W=X+ 10, X=C, Y= 10, Z=A1+ 10, A1=D, B1= 10, 
          new19(Q,T,W,Z,E,F,G,H,I,J,K,L,M,N).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 4, O=A, P= 4, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=E, P= 0, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=E, P= 0, 
          new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=E, P= 0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new28(A,B,C,D,O,F,G,H,I,J,K,L,M,N).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=F, P= 0, Q=R+ 1, R=A, S= 1, 
          T=U+ 2, U=B, V= 2, new19(Q,T,C,D,E,F,G,H,I,J,K,L,M,N).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=F, P= 0, Q=R+ 1, R=A, S= 1, 
          T=U+ 2, U=B, V= 2, new19(Q,T,C,D,E,F,G,H,I,J,K,L,M,N).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=F, P= 0, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new24(A,B,C,D,E,E) :- F>= 1, F=E, G= 0.
new24(A,B,C,D,E,E) :- F+ 1=< 0, F=E, G= 0.
new23(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 1, I>=J, I= 3*K, L= 3, K=A, J=B, 
          new24(A,B,C,D,H,M).
new23(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 0, I+ 1=<J, I= 3*K, L= 3, K=A, J=B, 
          new24(A,B,C,D,H,M).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new26(A,B,C,D,E,O,G,H,I,J,K,L,M,N).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=G, P= 0, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=G, P= 0, 
          new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=G, P= 0, 
          new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new21(A,B,C,D,E,F,O,H,I,J,K,L,M,N).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new16(A,B,C,D,E,F,G,H) :- new18(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
safe :- init(A,B,C,D), new16(A,B,C,D,E,F,G,H).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P+ 1, O=D, P=B, Q= -1*R, S= -1, R=A, 
          T= -1*U, V= -1, U=B, new3(Q,T,C,D,E,F,G,H,I,J,K,L,M,N).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=<P, O=D, P=B, 
          new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P, O=A, P=C, 
          new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=<P, O=A, P=C, 
          new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 4, O=A, P= 4, Q=R+ 1, R=A, S= 1, 
          T=U+ 3, U=B, V= 3, W=X+ 10, X=C, Y= 10, Z=A1+ 10, A1=D, B1= 10, 
          new3(Q,T,W,Z,E,F,G,H,I,J,K,L,M,N).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 4, O=A, P= 4, 
          new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=E, P= 0, 
          new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=E, P= 0, 
          new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=E, P= 0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new12(A,B,C,D,O,F,G,H,I,J,K,L,M,N).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=F, P= 0, Q=R+ 1, R=A, S= 1, 
          T=U+ 2, U=B, V= 2, new3(Q,T,C,D,E,F,G,H,I,J,K,L,M,N).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=F, P= 0, Q=R+ 1, R=A, S= 1, 
          T=U+ 2, U=B, V= 2, new3(Q,T,C,D,E,F,G,H,I,J,K,L,M,N).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=F, P= 0, 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new9(A,B,C,D,E,E).
new8(A,B,C,D,E,F) :- G= 0, G=E, H= 0, new9(A,B,C,D,E,F).
new7(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 1, I>=J, I= 3*K, L= 3, K=A, J=B, 
          new8(A,B,C,D,H,M).
new7(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H= 0, I+ 1=<J, I= 3*K, L= 3, K=A, J=B, 
          new8(A,B,C,D,H,M).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new10(A,B,C,D,E,O,G,H,I,J,K,L,M,N).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>= 1, O=G, P= 0, 
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+ 1=< 0, O=G, P= 0, 
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O= 0, O=G, P= 0, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new5(A,B,C,D,E,F,O,H,I,J,K,L,M,N).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new1(A,B,C,D,E,F,G,H) :- new2(A,B,C,D,I,J,K,E,F,G,H,L,M,N).
init(A,B,C,D).
false :- init(A,B,C,D), new1(A,B,C,D,E,F,G,H).
spec :- false.
spec :- safe.
