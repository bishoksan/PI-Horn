new32(A,B,C,D,E,F,G,A,B,C,D,E,F,G).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=:=1, P=:=Q, P=:=A, A>=0, Q=:=B, B>=0, 
          new14(A,B,O,R), new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=:=0, P>=Q+1, P=:=A, A>=0, Q=:=B, B>=0,
          new14(A,B,O,R), new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=:=0, P+1=<Q, P=:=A, A>=0, Q=:=B, B>=0,
          new14(A,B,O,R), new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P+1, O=:=E, P=:=0,
          new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+1=<P, O=:=E, P=:=0,
          new31(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=:=P, O=:=E, P=:=0, 
          new32(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new28(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=:=P, O=:=G, P=:=0, 
          new30(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new28(A,B,C,D,E,F,O,H,I,J,K,L,M,N).
new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new27(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new25(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new26(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P+1, O=:=E, P=:=0, Q=:=0,
          new25(A,B,Q,D,E,F,G,H,I,J,K,L,M,N).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+1=<P, O=:=E, P=:=0, Q=:=0,
          new25(A,B,Q,D,E,F,G,H,I,J,K,L,M,N).
new24(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=:=P, O=:=E, P=:=0, Q=:=1, 
          new25(A,B,Q,D,E,F,G,H,I,J,K,L,M,N).
new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=:=P, Q=:=1, 
          new24(A,B,C,Q,O,P,G,H,I,J,K,L,M,N).
new22(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new23(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new20(A,B,C,D) :- new22(A,B,E,F,G,H,I,C,D,J,K,L,M,N).
safe :- init(A,B), new20(A,B,C,D).
new19(A,B).
new17(A,B,C,C) :- new19(A,B).
new16(A,B,C,C).
new14(A,B,C,D) :- E>=F+1, E=:=C, F=:=0, new16(A,B,C,D).
new14(A,B,C,D) :- E+1=<F, E=:=C, F=:=0, new16(A,B,C,D).
new13(A,B,C,D) :- E=:=F, E=:=C, F=:=0, new17(A,B,C,D).
new11(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H=:=1, I=:=J, I=:=A, A>=0, J=:=B, B>=0, 
          new13(A,B,H,K).
new11(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H=:=0, I>=J+1, I=:=A, A>=0, J=:=B, B>=0,
          new13(A,B,H,K).
new11(A,B,C,D,E,F,G,A,B,C,D,E,F,G) :- H=:=0, I+1=<J, I=:=A, A>=0, J=:=B, B>=0,
          new13(A,B,H,K).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P+1, O=:=E, P=:=0,
          new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+1=<P, O=:=E, P=:=0,
            new11(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new8(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=:=P, O=:=G, P=:=0,
          new10(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new8(A,B,C,D,E,F,O,H,I,J,K,L,M,N).
new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new7(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new5(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new6(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O>=P+1, O=:=E, P=:=0, Q=:=0,
          new5(A,B,Q,D,E,F,G,H,I,J,K,L,M,N).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O+1=<P, O=:=E, P=:=0, Q=:=0,
          new5(A,B,Q,D,E,F,G,H,I,J,K,L,M,N).
new4(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=:=P, O=:=E, P=:=0, Q=:=1, 
          new5(A,B,Q,D,E,F,G,H,I,J,K,L,M,N).
new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- O=:=P, Q=:=1, 
          new4(A,B,C,Q,O,P,G,H,I,J,K,L,M,N).
new2(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- new3(A,B,C,D,E,F,G,H,I,J,K,L,M,N).
new1(A,B,C,D) :- new2(A,B,E,F,G,H,I,C,D,J,K,L,M,N).
init(A,B). % :- A=:=0, B=:=0.
false :- init(A,B), new1(A,B,C,D).
spec :- false.
spec :- safe.
