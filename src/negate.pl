:- module(negate,_).

:- use_module(library(lists)).
:- use_module(chclibs(ppl_ops)).
:- use_module(library(read_from_string), [read_from_atom/2]).



% negate(+DNF, -DNF).  
% DNF ::= Conj (; Conj)*
% Conj ::= [C] | [C|Conj]
% C ::= X = Y | X >= Y | X =< Y | X > Y | X < Y |

main([C]) :-
	convertString(C,C1),
	numbervars(C1,0,_),
	negate(C1,NC1),
	start_ppl,
	simplify(NC1,NC),
	end_ppl,
	write(NC),
	nl.
	
convertString(Q,Q1) :-
	read_from_atom(Q,Q1).
	

negate([C1], NegC1) :-
	!,
	negateConstraint(C1,NegC1).
negate([C1|C2], NegC1C2) :-
	!,
	negateConstraint(C1,NegC1),
	negate(C2,NegC2),
	disjunct(NegC1,NegC2,NegC1C2).
negate((D1;D2), NegD1D2) :-
	!,
	negate(D1,NegD1),
	negate(D2,NegD2),
	conjunct(NegD1,NegD2,NegD1D2).
negate([], [1=0]) :-
	!.

negateConstraint(X = Y,([X > Y]; [X < Y])).
negateConstraint(X > Y, [X =< Y]).
negateConstraint(X >= Y, [X < Y]).
negateConstraint(X < Y, [X >= Y]).
negateConstraint(X =< Y, [X > Y]).

disjunct((C1;D2),D,(C1;D3)) :-
	!,
	disjunct(D2,D,D3).
disjunct(C1,D1,(C1;D1)).

conjunct((C1;D2),D,D5) :-
	!,
	distribute(D,C1,D3),
	conjunct(D2,D,D4),
	disjunct(D3,D4,D5).
conjunct(C1,D1,D2) :-
	distribute(D1,C1,D2).
	
distribute((C1;D1),Cs,(C2;D2)) :-
	!,
	append(Cs,C1,C2),
	distribute(D1,Cs,D2).
distribute(C1,Cs,C2) :-
	!,
	append(Cs,C1,C2).
	
simplify((D1;D2),D5) :-
	makePolyhedron(D1,H),
	getConstraint(H,D3),
	(D3=[0=1] -> D5=D4; D5=(D3;D4)),
	simplify(D2,D4).
simplify(D1,D3) :-
	makePolyhedron(D1,H),
	getConstraint(H,D3),
	!.
simplify(_,false).	
