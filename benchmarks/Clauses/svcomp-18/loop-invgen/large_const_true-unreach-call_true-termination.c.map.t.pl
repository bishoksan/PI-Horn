new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, U=B, V= 1, W=X+Y, X=A, 
          Y=F, new22(W,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 2, U=B, V= 1, W=X+Y, X=A, 
          Y=G, new22(W,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 1, U=B, V= 1, W=X+Y, 
          X=A, Y=G, new22(W,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V>= 1, V=A, W= 0, 
          X=Y+ 1, Y=J, Z= 1, A1=B1- 1, B1=A, C1= 1, new11(A,B,U,D1), 
          new25(A1,B,C,D,E,F,G,H,I,X,K,L,M,N,O,P,Q,R,S,T).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V=< 0, V=A, W= 0, 
          X=Y+ 1, Y=J, Z= 1, A1=B1- 1, B1=A, C1= 1, new11(A,B,U,D1), 
          new25(A1,B,C,D,E,F,G,H,I,X,K,L,M,N,O,P,Q,R,S,T).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=J, V=H, 
          new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new26(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K>=L, K=J, L=H.
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, U=B, V= 0, W=X+Y, X=A, 
          Y=E, new22(W,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=B, V= 0, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=B, V= 0, 
          new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=I, V=H, W=X+ 1, 
          X=I, Y= 1, new24(A,B,C,D,E,F,G,H,W,J,K,L,M,N,O,P,Q,R,S,T).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V, U=I, V=H, W= 0, 
          new25(A,B,C,D,E,F,G,H,I,W,K,L,M,N,O,P,Q,R,S,T).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, 
          new22(A,B,C,D,E,F,G,V,U,J,K,L,M,N,O,P,Q,R,S,T).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 4000, V= 2000, W= 10000, 
          new21(A,B,C,D,U,V,W,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new18(A,B,C,D) :- new20(A,B,E,F,G,H,I,J,K,L,C,D,M,N,O,P,Q,R,S,T).
safe :- init(A,B), new18(A,B,C,D).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, U=B, V= 1, W=X+Y, X=A, 
          Y=F, new4(W,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 2, U=B, V= 1, W=X+Y, X=A, 
          Y=G, new4(W,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 1, U=B, V= 1, W=X+Y, 
          X=A, Y=G, new4(W,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new16(A,B).
new14(A,B,C,C) :- new16(A,B).
new13(A,B,C,C).
new11(A,B,C,D) :- E>= 1, E=C, F= 0, new13(A,B,C,D).
new11(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new13(A,B,C,D).
new10(A,B,C,D) :- E= 0, E=C, F= 0, new14(A,B,C,D).
new9(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1, L>= 1, L=A, M= 0, 
          new10(A,B,K,N).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V>= 1, V=A, W= 0, 
          X=Y+ 1, Y=J, Z= 1, A1=B1- 1, B1=A, C1= 1, new11(A,B,U,D1), 
          new7(A1,B,C,D,E,F,G,H,I,X,K,L,M,N,O,P,Q,R,S,T).
new9(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0, L=< 0, L=A, M= 0, 
          new10(A,B,K,N).
new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V=< 0, V=A, W= 0, 
          X=Y+ 1, Y=J, Z= 1, A1=B1- 1, B1=A, C1= 1, new11(A,B,U,D1), 
          new7(A1,B,C,D,E,F,G,H,I,X,K,L,M,N,O,P,Q,R,S,T).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=J, V=H, 
          new9(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, U=B, V= 0, W=X+Y, X=A, 
          Y=E, new4(W,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=B, V= 0, 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=B, V= 0, 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=I, V=H, W=X+ 1, 
          X=I, Y= 1, new6(A,B,C,D,E,F,G,H,W,J,K,L,M,N,O,P,Q,R,S,T).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V, U=I, V=H, W= 0, 
          new7(A,B,C,D,E,F,G,H,I,W,K,L,M,N,O,P,Q,R,S,T).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, 
          new4(A,B,C,D,E,F,G,V,U,J,K,L,M,N,O,P,Q,R,S,T).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 4000, V= 2000, W= 10000, 
          new3(A,B,C,D,U,V,W,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new1(A,B,C,D) :- new2(A,B,E,F,G,H,I,J,K,L,C,D,M,N,O,P,Q,R,S,T).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
