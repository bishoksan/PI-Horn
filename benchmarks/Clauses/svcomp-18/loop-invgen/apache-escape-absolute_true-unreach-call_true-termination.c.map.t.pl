new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=D, 
          X=Y+ 1, Y=D, Z= 1, new10(A,B,C,D,U,A1), 
          new65(A,B,C,X,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=D, 
          X=Y+ 1, Y=D, Z= 1, new10(A,B,C,D,U,A1), 
          new65(A,B,C,X,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=D, W=A, 
          new10(A,B,C,D,U,X), new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=D, W=A, 
          new10(A,B,C,D,U,X), new94(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new92(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=G, V= 0, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new92(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=G, V= 0, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new92(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, U=G, V= 0, 
          new93(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=D, 
          new10(A,B,C,D,U,X), new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=D, 
          new10(A,B,C,D,U,X), new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=D, W=A, 
          new10(A,B,C,D,U,X), new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=D, W=A, 
          new10(A,B,C,D,U,X), new91(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=F, 
          new10(A,B,C,D,U,X), new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=F, 
          new10(A,B,C,D,U,X), new90(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=V+ 1, V=D, W= 1, 
          new79(A,B,C,U,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=F, W=B, 
          new10(A,B,C,D,U,X), new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new87(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=F, W=B, 
          new10(A,B,C,D,U,X), new89(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=H, V= 0, W=X+ 1, 
          X=F, Y= 1, new87(A,B,C,D,E,W,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=H, V= 0, W=X+ 1, 
          X=F, Y= 1, new87(A,B,C,D,E,W,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new86(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, U=H, V= 0, 
          new88(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new86(A,B,C,D,E,F,G,U,I,J,K,L,M,N,O,P,Q,R,S,T).
new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=D, 
          new10(A,B,C,D,U,X), new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=D, 
          new10(A,B,C,D,U,X), new85(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=D, W=A, 
          new10(A,B,C,D,U,X), new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=D, W=A, 
          new10(A,B,C,D,U,X), new84(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=F, V=W- 1, W=B, 
          X= 1, new83(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V, U=F, V=W- 1, W=B, X= 1, 
          new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V+ 1, U=D, V=W- 1, W=A, 
          X= 1, new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=D, V=W- 1, W=A, 
          X= 1, new81(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=V, U=D, V=W- 1, W=A, X= 1, 
          new82(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new80(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=D, 
          new10(A,B,C,D,U,X), new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=D, 
          new10(A,B,C,D,U,X), new79(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=D, W=A, 
          new10(A,B,C,D,U,X), new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new77(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=D, W=A, 
          new10(A,B,C,D,U,X), new78(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=I, V= 0, W= 0, 
          new77(A,B,C,D,E,W,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=I, V= 0, W= 0, 
          new77(A,B,C,D,E,W,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new76(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, U=I, V= 0, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new75(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new76(A,B,C,D,E,F,G,H,U,J,K,L,M,N,O,P,Q,R,S,T).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=V, U=W+ 1, W=D, X= 1, 
          V=Y- 1, Y=A, Z= 1, new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V+ 1, U=W+ 1, W=D, X= 1, 
          V=Y- 1, Y=A, Z= 1, A1=B1+ 1, B1=D, C1= 1, D1=A1, 
          new75(A,B,C,A1,D1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=W+ 1, W=D, X= 1, 
          V=Y- 1, Y=A, Z= 1, A1=B1+ 1, B1=D, C1= 1, D1=A1, 
          new75(A,B,C,A1,D1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=X+ 1, 
          X=D, Y= 1, new10(A,B,C,D,U,Z), 
          new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=X+ 1, 
          X=D, Y= 1, new10(A,B,C,D,U,Z), 
          new74(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=X+ 1, X=D, 
          Y= 1, W=A, new10(A,B,C,D,U,Z), 
          new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=X+ 1, X=D, 
          Y= 1, W=A, new10(A,B,C,D,U,Z), 
          new73(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=V, U=D, V=W- 1, W=A, X= 1, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V+ 1, U=D, V=W- 1, W=A, 
          X= 1, new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=D, V=W- 1, W=A, 
          X= 1, new72(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=D, 
          new10(A,B,C,D,U,X), new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=D, 
          new10(A,B,C,D,U,X), new71(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=D, W=A, 
          new10(A,B,C,D,U,X), new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=D, W=A, 
          new10(A,B,C,D,U,X), new70(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new69(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new92(A,B,C,D,E,F,U,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V+ 1, U=D, V=W- 1, W=A, 
          X= 1, new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=D, V=W- 1, W=A, 
          X= 1, new67(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=V, U=D, V=W- 1, W=A, X= 1, 
          new68(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new66(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=D, 
          new10(A,B,C,D,U,X), new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=D, 
          new10(A,B,C,D,U,X), new65(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=D, W=A, 
          new10(A,B,C,D,U,X), new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=D, W=A, 
          new10(A,B,C,D,U,X), new64(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=J, V= 0, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=J, V= 0, 
          new63(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new62(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, U=J, V= 0, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new62(A,B,C,D,E,F,G,H,I,U,K,L,M,N,O,P,Q,R,S,T).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=X- 1, 
          X=D, Y= 1, new10(A,B,C,D,U,Z), 
          new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=X- 1, 
          X=D, Y= 1, new10(A,B,C,D,U,Z), 
          new61(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=X- 1, X=D, 
          Y= 1, W=A, new10(A,B,C,D,U,Z), 
          new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=X- 1, X=D, 
          Y= 1, W=A, new10(A,B,C,D,U,Z), 
          new60(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=W- 1, W=A, X= 1, 
          V=E, new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V, U=W- 1, W=A, X= 1, V=E, 
          new59(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, U=E, V= 0, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=E, V= 0, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=E, V= 0, 
          new58(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 0, U=E, V= 0, 
          new57(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=E, V= 0, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new55(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=B, V= 0, 
          new56(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=< 0, U=B, V= 0, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=A, V= 0, 
          new54(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new53(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=< 0, U=A, V= 0, 
          new55(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new51(A,B,C,D,E,F,G,H) :- new53(A,B,C,D,I,J,K,L,M,N,E,F,G,H,O,P,Q,R,S,T).
safe :- init(A,B,C,D), new51(A,B,C,D,E,F,G,H).
new50(A,B,C,D).
new48(A,B,C,D,E,E) :- new50(A,B,C,D).
new47(A,B,C,D,E,E).
new45(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1,  0=<L, M= 0, L=D, 
          new9(A,B,C,D,K,N).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=D, 
          X=Y+ 1, Y=D, Z= 1, new10(A,B,C,D,U,A1), 
          new16(A,B,C,X,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new45(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0,  0>=L+ 1, M= 0, L=D, 
          new9(A,B,C,D,K,N).
new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=D, 
          X=Y+ 1, Y=D, Z= 1, new10(A,B,C,D,U,A1), 
          new16(A,B,C,X,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new44(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1, L+ 1=<M, L=D, M=A, 
          new9(A,B,C,D,K,N).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=D, W=A, 
          new10(A,B,C,D,U,X), new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new44(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0, L>=M, L=D, M=A, 
          new9(A,B,C,D,K,N).
new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=D, W=A, 
          new10(A,B,C,D,U,X), new45(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=G, V= 0, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=G, V= 0, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new43(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, U=G, V= 0, 
          new44(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new42(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1,  0=<L, M= 0, L=D, 
          new9(A,B,C,D,K,N).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=D, 
          new10(A,B,C,D,U,X), new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new42(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0,  0>=L+ 1, M= 0, L=D, 
          new9(A,B,C,D,K,N).
new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=D, 
          new10(A,B,C,D,U,X), new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new41(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1, L+ 1=<M, L=D, M=A, 
          new9(A,B,C,D,K,N).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=D, W=A, 
          new10(A,B,C,D,U,X), new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new41(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0, L>=M, L=D, M=A, 
          new9(A,B,C,D,K,N).
new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=D, W=A, 
          new10(A,B,C,D,U,X), new42(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new40(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1,  0=<L, M= 0, L=F, 
          new9(A,B,C,D,K,N).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=F, 
          new10(A,B,C,D,U,X), new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new40(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0,  0>=L+ 1, M= 0, L=F, 
          new9(A,B,C,D,K,N).
new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=F, 
          new10(A,B,C,D,U,X), new41(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=V+ 1, V=D, W= 1, 
          new30(A,B,C,U,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new38(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1, L+ 1=<M, L=F, M=B, 
          new9(A,B,C,D,K,N).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=F, W=B, 
          new10(A,B,C,D,U,X), new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new38(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0, L>=M, L=F, M=B, 
          new9(A,B,C,D,K,N).
new38(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=F, W=B, 
          new10(A,B,C,D,U,X), new40(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=H, V= 0, W=X+ 1, 
          X=F, Y= 1, new38(A,B,C,D,E,W,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=H, V= 0, W=X+ 1, 
          X=F, Y= 1, new38(A,B,C,D,E,W,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new37(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, U=H, V= 0, 
          new39(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new37(A,B,C,D,E,F,G,U,I,J,K,L,M,N,O,P,Q,R,S,T).
new35(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1,  0=<L, M= 0, L=D, 
          new9(A,B,C,D,K,N).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=D, 
          new10(A,B,C,D,U,X), new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new35(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0,  0>=L+ 1, M= 0, L=D, 
          new9(A,B,C,D,K,N).
new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=D, 
          new10(A,B,C,D,U,X), new36(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new34(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1, L+ 1=<M, L=D, M=A, 
          new9(A,B,C,D,K,N).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=D, W=A, 
          new10(A,B,C,D,U,X), new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new34(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0, L>=M, L=D, M=A, 
          new9(A,B,C,D,K,N).
new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=D, W=A, 
          new10(A,B,C,D,U,X), new35(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=F, V=W- 1, W=B, 
          X= 1, new34(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V+ 1, U=D, V=W- 1, W=A, 
          X= 1, new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=D, V=W- 1, W=A, 
          X= 1, new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new29(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1,  0=<L, M= 0, L=D, 
          new9(A,B,C,D,K,N).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=D, 
          new10(A,B,C,D,U,X), new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new29(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0,  0>=L+ 1, M= 0, L=D, 
          new9(A,B,C,D,K,N).
new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=D, 
          new10(A,B,C,D,U,X), new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new28(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1, L+ 1=<M, L=D, M=A, 
          new9(A,B,C,D,K,N).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=D, W=A, 
          new10(A,B,C,D,U,X), new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new28(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0, L>=M, L=D, M=A, 
          new9(A,B,C,D,K,N).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=D, W=A, 
          new10(A,B,C,D,U,X), new29(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=I, V= 0, W= 0, 
          new28(A,B,C,D,E,W,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=I, V= 0, W= 0, 
          new28(A,B,C,D,E,W,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new27(A,B,C,D,E,F,G,H,U,J,K,L,M,N,O,P,Q,R,S,T).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V+ 1, U=W+ 1, W=D, X= 1, 
          V=Y- 1, Y=A, Z= 1, A1=B1+ 1, B1=D, C1= 1, D1=A1, 
          new26(A,B,C,A1,D1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=W+ 1, W=D, X= 1, 
          V=Y- 1, Y=A, Z= 1, A1=B1+ 1, B1=D, C1= 1, D1=A1, 
          new26(A,B,C,A1,D1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new24(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1,  0=<L, M= 0, L=N+ 1, 
          N=D, O= 1, new9(A,B,C,D,K,P).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=X+ 1, 
          X=D, Y= 1, new10(A,B,C,D,U,Z), 
          new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new24(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0,  0>=L+ 1, M= 0, L=N+ 1, 
          N=D, O= 1, new9(A,B,C,D,K,P).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=X+ 1, 
          X=D, Y= 1, new10(A,B,C,D,U,Z), 
          new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new23(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1, L+ 1=<M, L=N+ 1, N=D, 
          O= 1, M=A, new9(A,B,C,D,K,P).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=X+ 1, X=D, 
          Y= 1, W=A, new10(A,B,C,D,U,Z), 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new23(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0, L>=M, L=N+ 1, N=D, 
          O= 1, M=A, new9(A,B,C,D,K,P).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=X+ 1, X=D, 
          Y= 1, W=A, new10(A,B,C,D,U,Z), 
          new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V+ 1, U=D, V=W- 1, W=A, 
          X= 1, new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=D, V=W- 1, W=A, 
          X= 1, new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new21(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1,  0=<L, M= 0, L=D, 
          new9(A,B,C,D,K,N).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=D, 
          new10(A,B,C,D,U,X), new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new21(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0,  0>=L+ 1, M= 0, L=D, 
          new9(A,B,C,D,K,N).
new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=D, 
          new10(A,B,C,D,U,X), new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new20(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1, L+ 1=<M, L=D, M=A, 
          new9(A,B,C,D,K,N).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=D, W=A, 
          new10(A,B,C,D,U,X), new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new20(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0, L>=M, L=D, M=A, 
          new9(A,B,C,D,K,N).
new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=D, W=A, 
          new10(A,B,C,D,U,X), new21(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new43(A,B,C,D,E,F,U,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V+ 1, U=D, V=W- 1, W=A, 
          X= 1, new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=<V, U=D, V=W- 1, W=A, 
          X= 1, new18(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U=V, U=D, V=W- 1, W=A, X= 1, 
          new19(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new17(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new15(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1,  0=<L, M= 0, L=D, 
          new9(A,B,C,D,K,N).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=D, 
          new10(A,B,C,D,U,X), new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new15(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0,  0>=L+ 1, M= 0, L=D, 
          new9(A,B,C,D,K,N).
new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=D, 
          new10(A,B,C,D,U,X), new16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new14(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1, L+ 1=<M, L=D, M=A, 
          new9(A,B,C,D,K,N).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=D, W=A, 
          new10(A,B,C,D,U,X), new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new14(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0, L>=M, L=D, M=A, 
          new9(A,B,C,D,K,N).
new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=D, W=A, 
          new10(A,B,C,D,U,X), new15(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=J, V= 0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new13(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=J, V= 0, 
          new14(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- 
          new13(A,B,C,D,E,F,G,H,I,U,K,L,M,N,O,P,Q,R,S,T).
new11(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1,  0=<L, M= 0, L=N- 1, 
          N=D, O= 1, new9(A,B,C,D,K,P).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1,  0=<V, W= 0, V=X- 1, 
          X=D, Y= 1, new10(A,B,C,D,U,Z), 
          new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new11(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0,  0>=L+ 1, M= 0, L=N- 1, 
          N=D, O= 1, new9(A,B,C,D,K,P).
new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0,  0>=V+ 1, W= 0, V=X- 1, 
          X=D, Y= 1, new10(A,B,C,D,U,Z), 
          new12(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new10(A,B,C,D,E,F) :- G>= 1, G=E, H= 0, new47(A,B,C,D,E,F).
new10(A,B,C,D,E,F) :- G+ 1=< 0, G=E, H= 0, new47(A,B,C,D,E,F).
new9(A,B,C,D,E,F) :- G= 0, G=E, H= 0, new48(A,B,C,D,E,F).
new8(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 1, L+ 1=<M, L=N- 1, N=D, 
          O= 1, M=A, new9(A,B,C,D,K,P).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 1, V+ 1=<W, V=X- 1, X=D, 
          Y= 1, W=A, new10(A,B,C,D,U,Z), 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new8(A,B,C,D,E,F,G,H,I,J,A,B,C,D,E,F,G,H,I,J) :- K= 0, L>=M, L=N- 1, N=D, O= 1, 
          M=A, new9(A,B,C,D,K,P).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U= 0, V>=W, V=X- 1, X=D, Y= 1, 
          W=A, new10(A,B,C,D,U,Z), 
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>=V, U=W- 1, W=A, X= 1, V=E, 
          new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=E, V= 0, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U+ 1=< 0, U=E, V= 0, 
          new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 0, U=E, V= 0, 
          new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=B, V= 0, 
          new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- U>= 1, U=A, V= 0, 
          new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).
new1(A,B,C,D,E,F,G,H) :- new2(A,B,C,D,I,J,K,L,M,N,E,F,G,H,O,P,Q,R,S,T).
init(A,B,C,D).
false :- init(A,B,C,D), new1(A,B,C,D,E,F,G,H).
spec :- false.
spec :- safe.
