new35(A,B,C,D,E,F,G,H) :- I= 1, J>= 1, J=B, K= 0, L=M+ 1, M=D, N= 1, 
          new11(A,B,C,I,O), new29(A,B,C,L,E,F,G,H).
new35(A,B,C,D,E,F,G,H) :- I= 1, J+ 1=< 0, J=B, K= 0, L=M+ 1, M=D, N= 1, 
          new11(A,B,C,I,O), new29(A,B,C,L,E,F,G,H).
new35(A,B,C,D,E,F,G,H) :- I= 0, J= 0, J=B, K= 0, L=M+ 1, M=D, N= 1, 
          new11(A,B,C,I,O), new29(A,B,C,L,E,F,G,H).
new34(A,B,C,D,A,B,C,D) :- E= 0, E=C, F= 0.
new34(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K=L+M, L=B, M=C, 
          new35(A,K,C,D,E,F,G,H).
new34(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K=L+M, L=B, M=C, 
          new35(A,K,C,D,E,F,G,H).
new33(A,B,C,D,E,F,G,H) :- new34(A,B,I,D,E,F,G,H).
new31(A,B,C,D,E,F,G,H) :- I= 1, J= 0, J=B, K= 0, new11(A,B,C,I,L), 
          new33(A,B,C,D,E,F,G,H).
new31(A,B,C,D,E,F,G,H) :- I= 0, J>= 1, J=B, K= 0, new11(A,B,C,I,L), 
          new33(A,B,C,D,E,F,G,H).
new31(A,B,C,D,E,F,G,H) :- I= 0, J+ 1=< 0, J=B, K= 0, new11(A,B,C,I,L), 
          new33(A,B,C,D,E,F,G,H).
new30(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=D, J=A, K=L-M, L=B, M=C, 
          new31(A,K,C,D,E,F,G,H).
new29(A,B,C,D,E,F,G,H) :- new30(A,B,C,D,E,F,G,H).
new28(A,B,C,D,E,F,G,H) :- I= 1, J>= 1, J=B, K= 0, L=M+ 1, M=D, N= 1, 
          new11(A,B,C,I,O), new22(A,B,C,L,E,F,G,H).
new28(A,B,C,D,E,F,G,H) :- I= 1, J+ 1=< 0, J=B, K= 0, L=M+ 1, M=D, N= 1, 
          new11(A,B,C,I,O), new22(A,B,C,L,E,F,G,H).
new28(A,B,C,D,E,F,G,H) :- I= 0, J= 0, J=B, K= 0, L=M+ 1, M=D, N= 1, 
          new11(A,B,C,I,O), new22(A,B,C,L,E,F,G,H).
new27(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K=L+M, L=B, M=C, 
          new28(A,K,C,D,E,F,G,H).
new27(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K=L+M, L=B, M=C, 
          new28(A,K,C,D,E,F,G,H).
new26(A,B,C,D,E,F,G,H) :- new27(A,B,I,D,E,F,G,H).
new25(A,B,C,D,A,B,C,D) :- E= 1, F= 0, F=B, G= 0, new11(A,B,C,E,H).
new25(A,B,C,D,A,B,C,D) :- E= 0, F>= 1, F=B, G= 0, new11(A,B,C,E,H).
new25(A,B,C,D,A,B,C,D) :- E= 0, F+ 1=< 0, F=B, G= 0, new11(A,B,C,E,H).
new24(A,B,C,D,E,F,G,H) :- I= 1, J= 0, J=B, K= 0, new11(A,B,C,I,L), 
          new26(A,B,C,D,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I= 0, J>= 1, J=B, K= 0, new11(A,B,C,I,L), 
          new26(A,B,C,D,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I= 0, J+ 1=< 0, J=B, K= 0, new11(A,B,C,I,L), 
          new26(A,B,C,D,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=D, J=A, K=L-M, L=B, M=C, 
          new24(A,K,C,D,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- I>=J, I=D, J=A, new25(A,B,C,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- new23(A,B,C,D,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- I>= 1, I=A, J= 0, K= 0, new22(A,B,C,K,E,F,G,H).
new20(A,B,C,D,E,F,G,H) :- I>= 1, I=A, J= 0, K= 0, new29(A,B,C,K,E,F,G,H).
new19(A,B,C,D,A,B,C,D) :- E=< 0, E=A, F= 0.
new17(A,B,C,D,E,F) :- new19(A,B,C,G,D,E,F,H).
new17(A,B,C,D,E,F) :- new20(A,B,C,G,D,E,F,H).
new17(A,B,C,D,E,F) :- new21(A,B,C,G,D,E,F,H).
safe :- init(A,B,C), new17(A,B,C,D,E,F).
new16(A,B,C,D,D).
new14(A,B,C,D,A,B,C,D) :- E= 1, F>= 1, F=B, G= 0, new7(A,B,C,E,H).
new14(A,B,C,D,E,F,G,H) :- I= 1, J>= 1, J=B, K= 0, L=M+ 1, M=D, N= 1, 
          new11(A,B,C,I,O), new3(A,B,C,L,E,F,G,H).
new14(A,B,C,D,A,B,C,D) :- E= 1, F+ 1=< 0, F=B, G= 0, new7(A,B,C,E,H).
new14(A,B,C,D,E,F,G,H) :- I= 1, J+ 1=< 0, J=B, K= 0, L=M+ 1, M=D, N= 1, 
          new11(A,B,C,I,O), new3(A,B,C,L,E,F,G,H).
new14(A,B,C,D,A,B,C,D) :- E= 0, F= 0, F=B, G= 0, new7(A,B,C,E,H).
new14(A,B,C,D,E,F,G,H) :- I= 0, J= 0, J=B, K= 0, L=M+ 1, M=D, N= 1, 
          new11(A,B,C,I,O), new3(A,B,C,L,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I>= 1, I=C, J= 0, K=L+M, L=B, M=C, 
          new14(A,K,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I+ 1=< 0, I=C, J= 0, K=L+M, L=B, M=C, 
          new14(A,K,C,D,E,F,G,H).
new12(A,B,C,D,E,F,G,H) :- new13(A,B,I,D,E,F,G,H).
new11(A,B,C,D,E) :- F>= 1, F=D, G= 0, new16(A,B,C,D,E).
new11(A,B,C,D,E) :- F+ 1=< 0, F=D, G= 0, new16(A,B,C,D,E).
new10(A,B,C).
new8(A,B,C,D,D) :- new10(A,B,C).
new7(A,B,C,D,E) :- F= 0, F=D, G= 0, new8(A,B,C,D,E).
new6(A,B,C,D,A,B,C,D) :- E= 1, F= 0, F=B, G= 0, new7(A,B,C,E,H).
new6(A,B,C,D,A,B,C,D) :- E= 0, F>= 1, F=B, G= 0, new7(A,B,C,E,H).
new6(A,B,C,D,A,B,C,D) :- E= 0, F+ 1=< 0, F=B, G= 0, new7(A,B,C,E,H).
new5(A,B,C,D,A,B,C,D) :- E= 1, F= 0, F=B, G= 0, new7(A,B,C,E,H).
new5(A,B,C,D,E,F,G,H) :- I= 1, J= 0, J=B, K= 0, new11(A,B,C,I,L), 
          new12(A,B,C,D,E,F,G,H).
new5(A,B,C,D,A,B,C,D) :- E= 0, F>= 1, F=B, G= 0, new7(A,B,C,E,H).
new5(A,B,C,D,E,F,G,H) :- I= 0, J>= 1, J=B, K= 0, new11(A,B,C,I,L), 
          new12(A,B,C,D,E,F,G,H).
new5(A,B,C,D,A,B,C,D) :- E= 0, F+ 1=< 0, F=B, G= 0, new7(A,B,C,E,H).
new5(A,B,C,D,E,F,G,H) :- I= 0, J+ 1=< 0, J=B, K= 0, new11(A,B,C,I,L), 
          new12(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=D, J=A, K=L-M, L=B, M=C, 
          new5(A,K,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- I>=J, I=D, J=A, new6(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- new4(A,B,C,D,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- I>= 1, I=A, J= 0, K= 0, new3(A,B,C,K,E,F,G,H).
new1(A,B,C,D,E,F) :- new2(A,B,C,G,D,E,F,H).
init(A,B,C).
false :- init(A,B,C), new1(A,B,C,D,E,F).
spec :- false.
spec :- safe.
