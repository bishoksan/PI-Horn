new30(A,B,C,D,E,F,G,H) :- I= 1, J+ 1=<K, L=J* 4+M, M>= 0, M+ 1=< 4, L=D, N= 4, 
          K=B, O=P+ 1, P=D, Q= 1, new10(A,B,I,R), new23(A,B,C,O,E,F,G,H).
new30(A,B,C,D,E,F,G,H) :- I= 0, J>=K, L=J* 4+M, M>= 0, M+ 1=< 4, L=D, N= 4, 
          K=B, O=P+ 1, P=D, Q= 1, new10(A,B,I,R), new23(A,B,C,O,E,F,G,H).
new29(A,B,C,D,E,F,G,H) :- I= 1,  0=<J, K= 0, L=J* 4+M, M>= 0, M+ 1=< 4, L=D, 
          N= 4, new10(A,B,I,O), new30(A,B,C,D,E,F,G,H).
new29(A,B,C,D,E,F,G,H) :- I= 0,  0>=J+ 1, K= 0, L=J* 4+M, M>= 0, M+ 1=< 4, L=D, 
          N= 4, new10(A,B,I,O), new30(A,B,C,D,E,F,G,H).
new28(A,B,C,D,E,F,G,H) :- I= 1, J+ 1=<K, J=D, K=A, new10(A,B,I,L), 
          new29(A,B,C,D,E,F,G,H).
new28(A,B,C,D,E,F,G,H) :- I= 0, J>=K, J=D, K=A, new10(A,B,I,L), 
          new29(A,B,C,D,E,F,G,H).
new27(A,B,C,D,E,F,G,H) :- I= 1,  0=<J, K= 0, J=D, new10(A,B,I,L), 
          new28(A,B,C,D,E,F,G,H).
new27(A,B,C,D,E,F,G,H) :- I= 0,  0>=J+ 1, K= 0, J=D, new10(A,B,I,L), 
          new28(A,B,C,D,E,F,G,H).
new26(A,B,C,D,A,B,C,D).
new25(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=D, J=A, new27(A,B,C,D,E,F,G,H).
new25(A,B,C,D,E,F,G,H) :- I>=J, I=D, J=A, new26(A,B,C,D,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=D, K=J* 8+L, L>= 0, L+ 1=< 8, K=C, M= 8, 
          new25(A,B,C,D,E,F,G,H).
new24(A,B,C,D,E,F,G,H) :- I>=J, I=D, K=J* 8+L, L>= 0, L+ 1=< 8, K=C, M= 8, 
          new26(A,B,C,D,E,F,G,H).
new23(A,B,C,D,E,F,G,H) :- new24(A,B,C,D,E,F,G,H).
new22(A,B,C,D,E,F,G,H) :- I= 0, new23(A,B,J,I,E,F,G,H).
new21(A,B,C,D,E,F,G,H) :- new22(A,B,C,D,E,F,G,H).
new19(A,B,C,D) :- new21(A,B,E,F,C,D,G,H).
safe :- init(A,B), new19(A,B,C,D).
new18(A,B).
new16(A,B,C,C) :- new18(A,B).
new15(A,B,C,C).
new13(A,B,C,D,A,B,C,D) :- E= 1, F+ 1=<G, H=F* 4+I, I>= 0, I+ 1=< 4, H=D, J= 4, 
          G=B, new9(A,B,E,K).
new13(A,B,C,D,E,F,G,H) :- I= 1, J+ 1=<K, L=J* 4+M, M>= 0, M+ 1=< 4, L=D, N= 4, 
          K=B, O=P+ 1, P=D, Q= 1, new10(A,B,I,R), new4(A,B,C,O,E,F,G,H).
new13(A,B,C,D,A,B,C,D) :- E= 0, F>=G, H=F* 4+I, I>= 0, I+ 1=< 4, H=D, J= 4, 
          G=B, new9(A,B,E,K).
new13(A,B,C,D,E,F,G,H) :- I= 0, J>=K, L=J* 4+M, M>= 0, M+ 1=< 4, L=D, N= 4, 
          K=B, O=P+ 1, P=D, Q= 1, new10(A,B,I,R), new4(A,B,C,O,E,F,G,H).
new12(A,B,C,D,A,B,C,D) :- E= 1,  0=<F, G= 0, H=F* 4+I, I>= 0, I+ 1=< 4, H=D, 
          J= 4, new9(A,B,E,K).
new12(A,B,C,D,E,F,G,H) :- I= 1,  0=<J, K= 0, L=J* 4+M, M>= 0, M+ 1=< 4, L=D, 
          N= 4, new10(A,B,I,O), new13(A,B,C,D,E,F,G,H).
new12(A,B,C,D,A,B,C,D) :- E= 0,  0>=F+ 1, G= 0, H=F* 4+I, I>= 0, I+ 1=< 4, H=D, 
          J= 4, new9(A,B,E,K).
new12(A,B,C,D,E,F,G,H) :- I= 0,  0>=J+ 1, K= 0, L=J* 4+M, M>= 0, M+ 1=< 4, L=D, 
          N= 4, new10(A,B,I,O), new13(A,B,C,D,E,F,G,H).
new11(A,B,C,D,A,B,C,D) :- E= 1, F+ 1=<G, F=D, G=A, new9(A,B,E,H).
new11(A,B,C,D,E,F,G,H) :- I= 1, J+ 1=<K, J=D, K=A, new10(A,B,I,L), 
          new12(A,B,C,D,E,F,G,H).
new11(A,B,C,D,A,B,C,D) :- E= 0, F>=G, F=D, G=A, new9(A,B,E,H).
new11(A,B,C,D,E,F,G,H) :- I= 0, J>=K, J=D, K=A, new10(A,B,I,L), 
          new12(A,B,C,D,E,F,G,H).
new10(A,B,C,D) :- E>= 1, E=C, F= 0, new15(A,B,C,D).
new10(A,B,C,D) :- E+ 1=< 0, E=C, F= 0, new15(A,B,C,D).
new9(A,B,C,D) :- E= 0, E=C, F= 0, new16(A,B,C,D).
new8(A,B,C,D,A,B,C,D) :- E= 1,  0=<F, G= 0, F=D, new9(A,B,E,H).
new8(A,B,C,D,E,F,G,H) :- I= 1,  0=<J, K= 0, J=D, new10(A,B,I,L), 
          new11(A,B,C,D,E,F,G,H).
new8(A,B,C,D,A,B,C,D) :- E= 0,  0>=F+ 1, G= 0, F=D, new9(A,B,E,H).
new8(A,B,C,D,E,F,G,H) :- I= 0,  0>=J+ 1, K= 0, J=D, new10(A,B,I,L), 
          new11(A,B,C,D,E,F,G,H).
new6(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=D, J=A, new8(A,B,C,D,E,F,G,H).
new5(A,B,C,D,E,F,G,H) :- I+ 1=<J, I=D, K=J* 8+L, L>= 0, L+ 1=< 8, K=C, M= 8, 
          new6(A,B,C,D,E,F,G,H).
new4(A,B,C,D,E,F,G,H) :- new5(A,B,C,D,E,F,G,H).
new3(A,B,C,D,E,F,G,H) :- I= 0, new4(A,B,J,I,E,F,G,H).
new2(A,B,C,D,E,F,G,H) :- new3(A,B,C,D,E,F,G,H).
new1(A,B,C,D) :- new2(A,B,E,F,C,D,G,H).
init(A,B).
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
