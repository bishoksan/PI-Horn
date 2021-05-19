new33(A,B,C,A,B,C) :- D= 91, D=B, E= 91.
new32(A,B,C,D,E,F) :- G=A, H=I- 10, I=J, K= 10, L=H, new5(A,G,M,N,O,J,P,Q), 
          new33(O,L,H,D,E,F).
new32(A,B,C,D,E,F) :- G=A, H=I, J=H, new7(A,G,K,L,M,N,O,I), new33(M,J,H,D,E,F).
new30(A,B,C,A,B,C) :- D=E, D=B, E=F- 10, F=A, G= 10.
new29(A,B,C,D,E,F) :- G>= 103, G=A, H= 102, new30(A,B,C,D,E,F).
new28(A,B,C,D,E,F) :- G>= 92, G=B, H= 91, new29(A,B,C,D,E,F).
new28(A,B,C,D,E,F) :- G+ 1=< 91, G=B, H= 91, new29(A,B,C,D,E,F).
new27(A,B,C,D,E,F) :- G=A, H=I- 10, I=J, K= 10, L=H, new5(A,G,M,N,O,J,P,Q), 
          new28(O,L,H,D,E,F).
new27(A,B,C,D,E,F) :- G=A, H=I, J=H, new7(A,G,K,L,M,N,O,I), new28(M,J,H,D,E,F).
new20(A,B,C,D,E,F) :- new27(A,B,C,D,E,F).
new19(A,B,C,D,E,F) :- new32(A,B,C,D,E,F).
new17(A,B) :- new19(A,C,D,B,E,F).
new17(A,B) :- new20(A,C,D,B,E,F).
safe :- init(A), new17(A,B).
new12(A,B,C,A,B,C).
new11(A,B,C,D,E,F) :- G>=H+ 1, G=B, H=I- 10, I=A, J= 10, new12(A,B,C,D,E,F).
new11(A,B,C,D,E,F) :- G+ 1=<H, G=B, H=I- 10, I=A, J= 10, new12(A,B,C,D,E,F).
new10(A,B,C,D,E,F) :- G>= 103, G=A, H= 102, new11(A,B,C,D,E,F).
new10(A,B,C,D,E,F) :- G=< 102, G=A, H= 102, new12(A,B,C,D,E,F).
new9(A,B,C,D,E,B,C,F) :- G=C, F=H- 10, H=I, J= 10, new5(A,G,K,L,E,I,M,N).
new9(A,B,C,D,E,B,C,F) :- G=C, F=H, new7(A,G,I,J,E,K,L,H).
new8(A,B,C,D,E,F,G,H) :- I=J+ 11, J=B, K= 11, L=M- 10, M=N, O= 10, 
          new5(A,I,P,Q,R,N,S,T), new9(R,B,L,D,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I=J+ 11, J=B, K= 11, L=M, new7(A,I,N,O,P,Q,R,M), 
          new9(P,B,L,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I=< 100, I=B, J= 100, new8(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F) :- G>= 92, G=B, H= 91, new10(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G+ 1=< 91, G=B, H= 91, new10(A,B,C,D,E,F).
new5(A,B,C,D,A,B,C,D) :- E>= 101, E=B, F= 100.
new3(A,B,C,D,E,F) :- G=A, H=I- 10, I=J, K= 10, L=H, new5(A,G,M,N,O,J,P,Q), 
          new6(O,L,H,D,E,F).
new3(A,B,C,D,E,F) :- G=A, H=I, J=H, new7(A,G,K,L,M,N,O,I), new6(M,J,H,D,E,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
