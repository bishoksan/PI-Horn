% Product timed automaton supplied by Peter Jensen, Aalborg Univ.
% Fischer protocol with two processes
 

'a1_C2'(A,B,Eps,Id1,X11,X21) :-
    Id1=0,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'c1_C2'(A,B,Eps,Id,X1,X2).
'a1_W2'(A,B,Eps,Id1,X11,X21) :-
    Id1=0,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'c1_W2'(A,B,Eps,Id,X1,X2).
'a1_R2'(A,B,Eps,Id1,X11,X21) :-
    X21=<(A + Eps),Id1=0,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'c1_R2'(A,B,Eps,Id,X1,X2).
'r1_C2'(A,B,Eps,Id,X111,X21) :-
    X111=<(A + Eps),Id=0,X11=0,
    X111=X11+Delta,X21=X2+Delta,Delta>=0,
    'w1_C2'(A,B,Eps,Id,X1,X2).
'r1_W2'(A,B,Eps,Id,X111,X21) :-
    X111=<(A + Eps),Id=0,X11=0,
    X111=X11+Delta,X21=X2+Delta,Delta>=0,
    'w1_W2'(A,B,Eps,Id,X1,X2).
'r1_R2'(A,B,Eps,Id,X111,X21) :-
    X111=<(A + Eps),X2=<(A + Eps),Id=0,X11=0,
    X111=X11+Delta,X21=X2+Delta,Delta>=0,
    'w1_R2'(A,B,Eps,Id,X1,X2).
'c1_C2'(A,B,Eps,Id,X11,X21) :-
    X1>=(B - Eps), Id=1,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'w1_C2'(A,B,Eps,Id,X1,X2).
'w1_C2'(A,B,Eps,Id1,X111,X21) :-
    X1=<(A + Eps),X11=0, Id1=1,
    X111=X11+Delta,X21=X2+Delta,Delta>=0,
    'r1_C2'(A,B,Eps,Id,X1,X2).
'r1_C2'(A,B,Eps,Id,X111,X21) :-
    X111=<(A + Eps),Id=0,X11=0,
    X111=X11+Delta,X21=X2+Delta,Delta>=0,
    'a1_C2'(A,B,Eps,Id,X1,X2).
'c1_W2'(A,B,Eps,Id,X11,X21) :-
    X1>=(B - Eps), Id=1,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'w1_W2'(A,B,Eps,Id,X1,X2).
'w1_W2'(A,B,Eps,Id1,X111,X21) :-
    X1=<(A + Eps),X11=0,Id1=1,
    X111=X11+Delta,X21=X2+Delta,Delta>=0,
    'r1_W2'(A,B,Eps,Id,X1,X2).
'r1_W2'(A,B,Eps,Id,X111,X21) :-
    X111=<(A + Eps),Id=0,X11=0,
    X111=X11+Delta,X21=X2+Delta,Delta>=0,
    'a1_W2'(A,B,Eps,Id,X1,X2).
'c1_R2'(A,B,Eps,Id,X11,X21) :-
    X21=<(A + Eps),X1>=(B - Eps), Id=1,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'w1_R2'(A,B,Eps,Id,X1,X2).
'w1_R2'(A,B,Eps,Id1,X111,X21) :-
    X21=<(A + Eps),X1=<(A + Eps),X11=0, Id1=1,
    X111=X11+Delta,X21=X2+Delta,Delta>=0,
    'r1_R2'(A,B,Eps,Id,X1,X2).
'r1_R2'(A,B,Eps,Id,X111,X21) :-
    X111=<(A + Eps),X2=<(A + Eps),Id=0,X11=0,
    X111=X11+Delta,X21=X2+Delta,Delta>=0,
    'a1_R2'(A,B,Eps,Id,X1,X2).
'c1_A2'(A,B,Eps,Id1,X11,X21) :-
    Id1=0,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'c1_C2'(A,B,Eps,Id,X1,X2).
'w1_A2'(A,B,Eps,Id1,X11,X21) :-
    Id1=0,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'w1_C2'(A,B,Eps,Id,X1,X2).
'r1_A2'(A,B,Eps,Id1,X11,X21) :-
    X11=<(A + Eps),Id1=0,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'r1_C2'(A,B,Eps,Id,X1,X2).
'c1_R2'(A,B,Eps,Id,X11,X211) :-
    X211=<(A + Eps),Id=0,X21=0,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'c1_W2'(A,B,Eps,Id,X1,X2).
'w1_R2'(A,B,Eps,Id,X11,X21) :-
    X21=<(A + Eps),Id=0,X21=0,
    X11=X1+Delta,X211=X21+Delta,Delta>=0,
    'w1_W2'(A,B,Eps,Id,X1,X2).
'r1_R2'(A,B,Eps,Id,X11,X211) :-
    X11=<(A + Eps),X2=<(A + Eps),Id=0,X21=0,
    X11=X1+Delta,X211=X21+Delta,Delta>=0,
    'r1_W2'(A,B,Eps,Id,X1,X2).
'r1_C2'(A,B,Eps,Id,X11,X21) :-
    X11=<(A + Eps),X2>=(B - Eps), Id=2,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'r1_W2'(A,B,Eps,Id,X1,X2).
'r1_W2'(A,B,Eps,Id1,X11,X211) :-
    X11=<(A + Eps),X2=<(A + Eps),X21=0,Id1=2,
    X11=X1+Delta,X211=X21+Delta,Delta>=0,
    'r1_R2'(A,B,Eps,Id,X1,X2).
'w1_C2'(A,B,Eps,Id,X11,X21) :-
    X2>=(B - Eps), Id=2,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'w1_W2'(A,B,Eps,Id,X1,X2).
'w1_W2'(A,B,Eps,Id1,X11,X211) :-
    X2=<(A + Eps),X21=0,Id1=2,
    X11=X1+Delta,X211=X21+Delta,Delta>=0,
    'w1_R2'(A,B,Eps,Id,X1,X2).
'c1_C2'(A,B,Eps,Id,X11,X21) :-
    X2>=(B - Eps), Id=2,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'c1_W2'(A,B,Eps,Id,X1,X2).
'c1_W2'(A,B,Eps,Id1,X11,X211) :-
    X2=<(A + Eps),X21=0,Id1=2,
    X11=X1+Delta,X211=X21+Delta,Delta>=0,
    'c1_R2'(A,B,Eps,Id,X1,X2).
'c1_R2'(A,B,Eps,Id,X11,X211) :-
    X211=<(A + Eps),Id=0,X21=0,
    X11=X1+Delta,X211=X21+Delta,Delta>=0,
    'c1_A2'(A,B,Eps,Id,X1,X2).
'w1_R2'(A,B,Eps,Id,X11,X211) :-
    X211=<(A + Eps),Id=0,X21=0,
    X11=X1+Delta,X211=X21+Delta,Delta>=0,
    'w1_A2'(A,B,Eps,Id,X1,X2).
'r1_R2'(A,B,Eps,Id,X11,X211) :-
    X11=<(A + Eps),X2=<(A + Eps),Id=0,X21=0,
    X11=X1+Delta,X211=X21+Delta,Delta>=0,
    'r1_A2'(A,B,Eps,Id,X1,X2).
'a1_A2'(A,B,Eps,Id,X11,X21) :-
    A>=0,B>=0,Eps>0,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'i1_I2'(A,B,Eps,Id,X1,X2).
'a1_A2'(A,B,Eps,Id1,X11,X21) :-
    Id1=0,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'a1_C2'(A,B,Eps,Id,X1,X2).
'a1_C2'(A,B,Eps,Id,X11,X21) :-
    X2>=(B - Eps), Id=2,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'a1_W2'(A,B,Eps,Id,X1,X2).
'a1_R2'(A,B,Eps,Id,X11,X211) :-
    X211=<(A + Eps),Id=0,X21=0,
    X11=X1+Delta,X211=X21+Delta,Delta>=0,
    'a1_W2'(A,B,Eps,Id,X1,X2).
'a1_W2'(A,B,Eps,Id1,X11,X211) :-
    X2=<(A + Eps),X21=0,Id1=2,
    X11=X1+Delta,X211=X21+Delta,Delta>=0,
    'a1_R2'(A,B,Eps,Id,X1,X2).
'a1_R2'(A,B,Eps,Id,X11,X211) :-
    X211=<(A + Eps),Id=0,X21=0,
    X11=X1+Delta,X211=X21+Delta,Delta>=0,
    'a1_A2'(A,B,Eps,Id,X1,X2).
'a1_A2'(A,B,Eps,Id1,X11,X21) :-
    Id1=0,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'c1_A2'(A,B,Eps,Id,X1,X2).
'c1_A2'(A,B,Eps,Id,X11,X21) :-
    X1>=(B - Eps), Id=1,
    X11=X1+Delta,X21=X2+Delta,Delta>=0,
    'w1_A2'(A,B,Eps,Id,X1,X2).
'r1_A2'(A,B,Eps,Id,X111,X21) :-
    X111=<(A + Eps),Id=0,X11=0,
    X111=X11+Delta,X21=X2+Delta,Delta>=0,
    'w1_A2'(A,B,Eps,Id,X1,X2).
'w1_A2'(A,B,Eps,Id1,X111,X21) :-
    X1=<(A + Eps),X11=0, Id1=1,
    X111=X11+Delta,X21=X2+Delta,Delta>=0,
    'r1_A2'(A,B,Eps,Id,X1,X2).
'r1_A2'(A,B,Eps,Id,X111,X21) :-
    X111=<(A + Eps),Id=0,X11=0,
    X111=X11+Delta,X21=X2+Delta,Delta>=0,
    'a1_A2'(A,B,Eps,Id,X1,X2).

    
'i1_I2'(A,B,Eps,Id,X1,X2) :-
	init(A,B,Eps).

init(A,B,Eps).

	
% Expected precondition neg([A -B+2*Eps>=0,A+Eps>=0]), i.e.
% I.e. A-B+2*Eps < 0  \/ A+Eps < 0.


	
false :- 'c1_C2'(A,B,Eps,Id,X1,X2).
spec :- false. 
spec :- safe.