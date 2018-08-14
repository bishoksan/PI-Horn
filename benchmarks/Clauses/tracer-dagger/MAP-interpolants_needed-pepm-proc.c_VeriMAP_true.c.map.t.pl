new33(A,B,C,D,E,F,G,H,I,J) :- K>= 4, K=A, L= 4, M=N+ 1, N=A, O= 1, P=Q+ 3, Q=B, 
          R= 3, new25(M,P,C,D,E,F,G,H,I,J).
new33(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 4, K=A, L= 4, new25(A,B,C,D,E,F,G,H,I,J).
new32(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=C, L= 0, new33(A,B,C,D,E,F,G,H,I,J).
new32(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=C, L= 0, new33(A,B,C,D,E,F,G,H,I,J).
new32(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=C, L= 0, new25(A,B,C,D,E,F,G,H,I,J).
new31(A,B,C,D,E,F,G,H,I,J) :- new32(A,B,K,D,E,F,G,H,I,J).
new30(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=D, L= 0, M=N+ 1, N=A, O= 1, P=Q+ 2, Q=B, 
          R= 2, new25(M,P,C,D,E,F,G,H,I,J).
new30(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=D, L= 0, M=N+ 1, N=A, O= 1, P=Q+ 2, 
          Q=B, R= 2, new25(M,P,C,D,E,F,G,H,I,J).
new30(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=D, L= 0, new31(A,B,C,D,E,F,G,H,I,J).
new29(A,B,C,D,E,A,B,C,D,E) :- F>=G, F= 3*H, I= 3, H=A, G=B.
new28(A,B,C,D,E,F,G,H,I,J) :- new30(A,B,C,K,E,F,G,H,I,J).
new27(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, new28(A,B,C,D,E,F,G,H,I,J).
new27(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=E, L= 0, new28(A,B,C,D,E,F,G,H,I,J).
new27(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, new29(A,B,C,D,E,F,G,H,I,J).
new26(A,B,C,D,E,F,G,H,I,J) :- new27(A,B,C,D,K,F,G,H,I,J).
new25(A,B,C,D,E,F,G,H,I,J) :- new26(A,B,C,D,E,F,G,H,I,J).
new14(A,B,C,D,E,F,G,H,I,J) :- new25(A,B,C,D,E,F,G,H,I,J).
new12(A,B,C,D) :- new14(A,B,E,F,G,C,D,H,I,J).
safe :- init(A,B), new12(A,B,C,D).
new11(A,B,C,D,E,F,G,H,I,J) :- K>= 4, K=A, L= 4, M=N+ 1, N=A, O= 1, P=Q+ 3, Q=B, 
          R= 3, new3(M,P,C,D,E,F,G,H,I,J).
new11(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 4, K=A, L= 4, new3(A,B,C,D,E,F,G,H,I,J).
new10(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=C, L= 0, new11(A,B,C,D,E,F,G,H,I,J).
new10(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=C, L= 0, new11(A,B,C,D,E,F,G,H,I,J).
new10(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=C, L= 0, new3(A,B,C,D,E,F,G,H,I,J).
new9(A,B,C,D,E,F,G,H,I,J) :- new10(A,B,K,D,E,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=D, L= 0, M=N+ 1, N=A, O= 1, P=Q+ 2, Q=B, 
          R= 2, new3(M,P,C,D,E,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=D, L= 0, M=N+ 1, N=A, O= 1, P=Q+ 2, 
          Q=B, R= 2, new3(M,P,C,D,E,F,G,H,I,J).
new8(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=D, L= 0, new9(A,B,C,D,E,F,G,H,I,J).
new7(A,B,C,D,E,A,B,C,D,E) :- F+ 1=<G, F= 3*H, I= 3, H=A, G=B.
new6(A,B,C,D,E,F,G,H,I,J) :- new8(A,B,C,K,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- K>= 1, K=E, L= 0, new6(A,B,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- K+ 1=< 0, K=E, L= 0, new6(A,B,C,D,E,F,G,H,I,J).
new5(A,B,C,D,E,F,G,H,I,J) :- K= 0, K=E, L= 0, new7(A,B,C,D,E,F,G,H,I,J).
new4(A,B,C,D,E,F,G,H,I,J) :- new5(A,B,C,D,K,F,G,H,I,J).
new3(A,B,C,D,E,F,G,H,I,J) :- new4(A,B,C,D,E,F,G,H,I,J).
new2(A,B,C,D,E,F,G,H,I,J) :- new3(A,B,C,D,E,F,G,H,I,J).
new1(A,B,C,D) :- new2(A,B,E,F,G,C,D,H,I,J).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
