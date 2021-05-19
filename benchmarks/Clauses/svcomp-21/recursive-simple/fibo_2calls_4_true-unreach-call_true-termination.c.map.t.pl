new46(A,B,C,A,B,C).
new44(A,B,C,D,E,F) :- G= 3, G=B, H= 3, new46(A,B,C,D,E,F).
new43(A,B,C,D,E,F) :- G=A, H= 0, I= 0, new5(A,G,J,K,L,M,N,O), 
          new44(L,I,H,D,E,F).
new43(A,B,C,D,E,F) :- G=A, H= 1, I= 1, new7(A,G,J,K,L,M,N,O), 
          new44(L,I,H,D,E,F).
new43(A,B,C,D,E,F) :- G=A, H=I+J, I=K, J=L, M=H, new8(A,G,N,O,P,Q,K,L), 
          new44(P,M,H,D,E,F).
new42(A,B,C,D,E,F) :- new43(A,B,C,D,E,F).
new40(A,B) :- new42(A,C,D,B,E,F).
safe :- init(A), new40(A,B).
new29(A).
new27(A,B,C,A,B,C) :- new29(A).
new24(A,B,C,D,A,B,C,D) :- E= 1, E=B, F= 1.
new18(A,B,C,D,A,B,C,D) :- E= 1, E=B, F= 1.
new17(A,B,C,D,E,B,C,F) :- G=H- 2, H=B, I= 2, F= 0, new5(A,G,J,K,E,L,M,N).
new17(A,B,C,D,E,B,C,F) :- G=H- 2, H=B, I= 2, F= 1, new7(A,G,J,K,E,L,M,N).
new17(A,B,C,D,E,B,C,F) :- G=H- 2, H=B, I= 2, F=J+K, J=L, K=M, 
          new8(A,G,N,O,E,P,L,M).
new16(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L= 0, new5(A,I,M,N,O,P,Q,R), 
          new17(O,B,L,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L= 1, new7(A,I,M,N,O,P,Q,R), 
          new17(O,B,L,D,E,F,G,H).
new16(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L=M+N, M=O, N=P, 
          new8(A,I,Q,R,S,T,O,P), new17(S,B,L,D,E,F,G,H).
new15(A,B,C,D,E,F,G,H) :- I>= 2, I=B, J= 1, new16(A,B,C,D,E,F,G,H).
new15(A,B,C,D,E,F,G,H) :- I+ 1=< 1, I=B, J= 1, new16(A,B,C,D,E,F,G,H).
new14(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 1, new15(A,B,C,D,E,F,G,H).
new13(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 1, new18(A,B,C,D,E,F,G,H).
new12(A,B,C,D,E,B,C,F) :- G=H- 2, H=B, I= 2, F= 0, new11(A,G,J,K,E,L,M,N).
new12(A,B,C,D,E,B,C,F) :- G=H- 2, H=B, I= 2, F= 1, new13(A,G,J,K,E,L,M,N).
new12(A,B,C,D,E,B,C,F) :- G=H- 2, H=B, I= 2, F=J+K, J=L, K=M, 
          new14(A,G,N,O,E,P,L,M).
new11(A,B,C,D,A,B,C,D) :- E+ 1=< 1, E=B, F= 1.
new10(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L= 0, new11(A,I,M,N,O,P,Q,R), 
          new12(O,B,L,D,E,F,G,H).
new10(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L= 1, new13(A,I,M,N,O,P,Q,R), 
          new12(O,B,L,D,E,F,G,H).
new10(A,B,C,D,E,F,G,H) :- I=J- 1, J=B, K= 1, L=M+N, M=O, N=P, 
          new14(A,I,Q,R,S,T,O,P), new12(S,B,L,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I>= 2, I=B, J= 1, new10(A,B,C,D,E,F,G,H).
new9(A,B,C,D,E,F,G,H) :- I+ 1=< 1, I=B, J= 1, new10(A,B,C,D,E,F,G,H).
new8(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 1, new9(A,B,C,D,E,F,G,H).
new7(A,B,C,D,E,F,G,H) :- I>= 1, I=B, J= 1, new24(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F) :- G>= 4, G=B, H= 3, new27(A,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G+ 1=< 3, G=B, H= 3, new27(A,B,C,D,E,F).
new5(A,B,C,D,A,B,C,D) :- E+ 1=< 1, E=B, F= 1.
new3(A,B,C,D,E,F) :- G=A, H= 0, I= 0, new5(A,G,J,K,L,M,N,O), new6(L,I,H,D,E,F).
new3(A,B,C,D,E,F) :- G=A, H= 1, I= 1, new7(A,G,J,K,L,M,N,O), new6(L,I,H,D,E,F).
new3(A,B,C,D,E,F) :- G=A, H=I+J, I=K, J=L, M=H, new8(A,G,N,O,P,Q,K,L), 
          new6(P,M,H,D,E,F).
new2(A,B,C,D,E,F) :- new3(A,B,C,D,E,F).
new1(A,B) :- new2(A,C,D,B,E,F).
init(A).
false :- init(A), new1(A,B).
spec :- false.
spec :- safe.
