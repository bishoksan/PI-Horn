new22(A,B,C,D,E,F,G,H) :- I=:=D, new9(A,B,C,I,J), new18(A,B,C,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I=:=J, I=:=B, B>=0, J=:=2*L, K=:=2, L=:=A, A>=0,
          M=:=1, new22(A,B,C,M,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I>=J+1, I=:=B, B>=0, J=:=2*L, K=:=2, L=:=A, A>=0,
          M=:=0, new22(A,B,C,M,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I+1=<J, I=:=B, B>=0, J=:=2*L, K=:=2, L=:=A, A>=0,
          M=:=0, new22(A,B,C,M,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I=:=J, I=:=C, C>=0, J=:=3*L, K=:=3, L=:=A, A>=0,
          new21(A,B,C,D,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I>=J+1, I=:=C, C>=0, J=:=3*L, K=:=3, L=:=A, A>=0,
          M=:=0, new22(A,B,C,M,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I+1=<J, I=:=C, C>=0, J=:=3*L, K=:=3, L=:=A, A>=0,
          M=:=0, new22(A,B,C,M,E,F,G,H).
new19(A,B,C,D,E,F,G,H) :- I+1=<J, I=:=C, C>=0, J=:=268435455, K=:=L+M, L=:=A, 
          A>=0, M=:=1, N=:=O+P, O=:=B, B>=0, P=:=2, Q=:=R+S, R=:=C, C>=0, 
          S=:=3, new20(K,N,Q,D,E,F,G,H).
new19(A,B,C,D,A,B,C,D) :- E>=F, E=:=C, C>=0, F=:=268435455.
new18(A,B,C,D,E,F,G,H) :- new19(A,B,C,D,E,F,G,H).
new17(A,B,C,D,E,F,G,H) :- new18(A,B,C,D,E,F,G,H).
new15(A,B,C,D,E,F) :- new17(A,B,C,G,D,E,F,H).
safe :- init(A,B,C), new15(A,B,C,D,E,F).
new14(A,B,C).
new12(A,B,C,D,D) :- new14(A,B,C).
new11(A,B,C,D,D).
new9(A,B,C,D,E) :- F>=G+1, F=:=D, G=:=0, new11(A,B,C,D,E).
new9(A,B,C,D,E) :- F+1=<G, F=:=D, G=:=0, new11(A,B,C,D,E).
new8(A,B,C,D,E) :- F=:=G, F=:=D, G=:=0, new12(A,B,C,D,E).
new7(A,B,C,D,A,B,C,D) :- E=:=D, new8(A,B,C,E,F).
new7(A,B,C,D,E,F,G,H) :- I=:=D, new9(A,B,C,I,J), new3(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I=:=J, I=:=B, B>=0, J=:=2*L, K=:=2, L=:=A, A>=0,
          M=:=1, new7(A,B,C,M,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- J+1=<I, I=:=B, B>=0, J=:=2*L, K=:=2, L=:=A, A>=0,
          M=:=0, new7(A,B,C,M,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I+1=<J, I=:=B, B>=0, J=:=2*L, K=:=2, L=:=A, A>=0,
          M=:=0, new7(A,B,C,M,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I=:=J, I=:=C, C>=0, J=:=3*L, K=:=3, L=:=A, A>=0,
          new6(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- J+1=<I, I=:=C, C>=0, J=:=3*L, K=:=3, L=:=A, A>=0,
          M=:=0, new7(A,B,C,M,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I+1=<J, I=:=C, C>=0, J=:=3*L, K=:=3, L=:=A, A>=0,
          M=:=0, new7(A,B,C,M,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I+1=<J, I=:=C, C>=0, J=:=268435455, K=:=L+M, L=:=A, 
          A>=0, M=:=1, N=:=O+P, O=:=B, B>=0, P=:=2, Q=:=R+S, R=:=C, C>=0, 
          S=:=3, new5(K,N,Q,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,B,C,D,E,F,G,H).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,D,E,F,H).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
