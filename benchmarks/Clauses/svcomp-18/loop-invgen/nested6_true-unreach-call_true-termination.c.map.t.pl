new36(A,B,C,D,E,F,G,H,I,J) :- K= 1, L>=M, L=A, M= 2*N, O= 2, N=C, P=Q+ 1, Q=A, 
          R= 1, new12(A,B,K,S), new31(P,B,C,D,E,F,G,H,I,J).
new36(A,B,C,D,E,F,G,H,I,J) :- K= 0, L+ 1=<M, L=A, M= 2*N, O= 2, N=C, P=Q+ 1, 
          Q=A, R= 1, new12(A,B,K,S), new31(P,B,C,D,E,F,G,H,I,J).
new35(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=A, L=B, new36(A,B,C,D,E,F,G,H,I,J).
new35(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=A, L=B, new34(A,B,C,D,E,F,G,H,I,J).
new34(A,B,C,D,E,F,G,H,I,J) :- K=L+ 1, L=D, M= 1, new27(A,B,C,K,E,F,G,H,I,J).
new33(A,B,C,D,E,F,G,H,I,J) :- K= 1, L=<M, L=A, M=B, new12(A,B,K,N), 
          new34(A,B,C,D,E,F,G,H,I,J).
new33(A,B,C,D,E,F,G,H,I,J) :- K= 0, L>=M+ 1, L=A, M=B, new12(A,B,K,N), 
          new34(A,B,C,D,E,F,G,H,I,J).
new32(A,B,C,D,E,F,G,H,I,J) :- K= 1, L>=M, L=A, M=B, new12(A,B,K,N), 
          new33(A,B,C,D,E,F,G,H,I,J).
new32(A,B,C,D,E,F,G,H,I,J) :- K= 0, L+ 1=<M, L=A, M=B, new12(A,B,K,N), 
          new33(A,B,C,D,E,F,G,H,I,J).
new31(A,B,C,D,E,F,G,H,I,J) :- new35(A,B,C,D,E,F,G,H,I,J).
new30(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, M=D, new31(M,B,C,D,E,F,G,H,I,J).
new30(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=E, L= 0, M=D, 
          new31(M,B,C,D,E,F,G,H,I,J).
new30(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, new32(A,B,C,D,E,F,G,H,I,J).
new29(A,B,C,D,E,F,G,H,I,J) :- new30(A,B,C,D,K,F,G,H,I,J).
new28(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=B, new29(A,B,C,D,E,F,G,H,I,J).
new28(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=D, L=B, M=N+ 1, N=C, O= 1, 
          new25(A,B,M,D,E,F,G,H,I,J).
new27(A,B,C,D,E,F,G,H,I,J) :- new28(A,B,C,D,E,F,G,H,I,J).
new26(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=B, M= 2*N, O= 2, N=C, 
          new27(A,B,C,M,E,F,G,H,I,J).
new26(A,B,C,D,E,A,B,C,D,E) :- F>=G, F=C, G=B.
new25(A,B,C,D,E,F,G,H,I,J) :- new26(A,B,C,D,E,F,G,H,I,J).
new24(A,B,C,D,E,F,G,H,I,J) :- K= 0, new25(A,B,K,D,E,F,G,H,I,J).
new22(A,B,C,D) :- new24(A,B,E,F,G,C,D,H,I,J).
safe :- init(A,B), new22(A,B,C,D).
new21(A,B,C,D,E,A,B,C,D,E) :- F= 1, G>=H, G=A, H= 2*I, J= 2, I=C, 
          new11(A,B,F,K).
new21(A,B,C,D,E,F,G,H,I,J) :- K= 1, L>=M, L=A, M= 2*N, O= 2, N=C, P=Q+ 1, Q=A, 
          R= 1, new12(A,B,K,S), new9(P,B,C,D,E,F,G,H,I,J).
new21(A,B,C,D,E,A,B,C,D,E) :- F= 0, G+ 1=<H, G=A, H= 2*I, J= 2, I=C, 
          new11(A,B,F,K).
new21(A,B,C,D,E,F,G,H,I,J) :- K= 0, L+ 1=<M, L=A, M= 2*N, O= 2, N=C, P=Q+ 1, 
          Q=A, R= 1, new12(A,B,K,S), new9(P,B,C,D,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=A, L=B, new21(A,B,C,D,E,F,G,H,I,J).
new20(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=A, L=B, new14(A,B,C,D,E,F,G,H,I,J).
new19(A,B).
new17(A,B,C,C) :- new19(A,B).
new16(A,B,C,C).
new14(A,B,C,D,E,F,G,H,I,J) :- K=L+ 1, L=D, M= 1, new5(A,B,C,K,E,F,G,H,I,J).
new13(A,B,C,D,E,A,B,C,D,E) :- F= 1, G=<H, G=A, H=B, new11(A,B,F,I).
new13(A,B,C,D,E,F,G,H,I,J) :- K= 1, L=<M, L=A, M=B, new12(A,B,K,N), 
          new14(A,B,C,D,E,F,G,H,I,J).
new13(A,B,C,D,E,A,B,C,D,E) :- F= 0, G>=H+ 1, G=A, H=B, new11(A,B,F,I).
new13(A,B,C,D,E,F,G,H,I,J) :- K= 0, L>=M+ 1, L=A, M=B, new12(A,B,K,N), 
          new14(A,B,C,D,E,F,G,H,I,J).
new12(A,B,C,D) :- E>= 1, E=C, F= 0, new16(A,B,C,D).
new12(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new16(A,B,C,D).
new11(A,B,C,D) :- E= 0, E=C, F= 0, new17(A,B,C,D).
new10(A,B,C,D,E,A,B,C,D,E) :- F= 1, G>=H, G=A, H=B, new11(A,B,F,I).
new10(A,B,C,D,E,F,G,H,I,J) :- K= 1, L>=M, L=A, M=B, new12(A,B,K,N), 
          new13(A,B,C,D,E,F,G,H,I,J).
new10(A,B,C,D,E,A,B,C,D,E) :- F= 0, G+ 1=<H, G=A, H=B, new11(A,B,F,I).
new10(A,B,C,D,E,F,G,H,I,J) :- K= 0, L+ 1=<M, L=A, M=B, new12(A,B,K,N), 
          new13(A,B,C,D,E,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :- new20(A,B,C,D,E,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, M=D, new9(M,B,C,D,E,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=E, L= 0, M=D, 
          new9(M,B,C,D,E,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, new10(A,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,F,G,H,I,J) :- new8(A,B,C,D,K,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=D, L=B, new7(A,B,C,D,E,F,G,H,I,J).
new6(A,B,C,D,E,F,G,H,I,J) :- K>=L, K=D, L=B, M=N+ 1, N=C, O= 1, 
          new3(A,B,M,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- new6(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- K+ 1=<L, K=C, L=B, M= 2*N, O= 2, N=C, 
          new5(A,B,C,M,E,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- new4(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- K= 0, new3(A,B,K,D,E,F,G,H,I,J).
new1(A,B,C,D) :- new2(A,B,E,F,G,C,D,H,I,J).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
