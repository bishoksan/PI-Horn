:- module(dnf,_).

:- use_module(library(lists)).
:- use_module(chclibs(ppl_ops)).
:- use_module(library(read_from_string), [read_from_atom/2]).

%interface with yices
:- use_module(chclibs(yices2_sat)).
:- use_module(ciao_yices(ciao_yices_2)).


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

%given two lists of lists (representing disj) L1 and L2 such that L1 /\ L2 and a set of variables ocurring in them, get a single dnf formula
%equivalent to L1 /\ L2 as a disjunctive sequence F
dnf(L1, L2, _, ([])):-
%empty case
    L1=[[]],
    L2=[[]], !.
%both having single element case
dnf(L1, L2, _, (C)):-
%single elem case
    length(L1, 1),
    length(L2, 1),
    L1 = [A],
    L2 = [B],
    append(A,B,C), !.

dnf(L1, L2, Vs, F):-
    simplify_disj_2_seq(L1, Vs, S1),
    simplify_disj_2_seq(L2, Vs, S2),
    dnf2(S1, S2, F1),
    disj_seq_2_list_list(F1, F2),
    simplify_disj_2_seq(F2, Vs, F).

%L represents list of lists representing disj

list_of_list_2_seq([A], (A)):-!.
list_of_list_2_seq([A,B], (A;B)):-
    !.
list_of_list_2_seq([A|R], (A;R1)):-
    !,
    list_of_list_2_seq(R, R1).
list_of_list_2_seq([], [false]).


dnf2((C1;C2), (C3;C4), (R1;R2)):-
    !,
    dnf2(C1, (C3;C4), R1),
    dnf2(C2, (C3;C4), R2).
dnf2((C1;C2), C3, (R1;R2)):-
    !,
    dnf2(C1, C3, R1),
    dnf2(C2, C3, R2).
dnf2( C3, (C1;C2),(R1;R2)):-
    !,
    dnf2(C3, C1, R1),
    dnf2(C3, C2, R2).
dnf2(A,B, C):-
    !,
    append(A,B,C).
dnf2([],_, [false]). %empty list is false
dnf2(_,[], [false]).
dnf2([false],_, [false]).
dnf2(_,[false], [false]).


%simplification of disjuncts

%simplifies a dnf formula L to another dnf L1

simplify_dnf_2_dnf(L, Vs, L1):-
   % yices_init,
    simplify_disj(L, Vs, Seq),
    (Seq=false->L1=[false]
    ;
        disj_seq_2_list_list(Seq, L1)
    ).
   %yices_exit.


% simplify_disj(L,_,  L1): L is a list of list representing disjuncts, L1 is
%a seq of disjuncts

simplify_disj(L, Vs, Seq):-
    (L=[]->Seq=(false)
    ;
        filter_trivial_true_false(L, L1),
        simplify_disj_2_seq(L1, Vs, Seq)
    ).

filter_trivial_true_false([X], [X]):-!.
filter_trivial_true_false([C|L], L1):-
    (C=[false]; C=[0=1]; C=[1=0]),
    !,
    filter_trivial_true_false(L, L1).
filter_trivial_true_false([C|_], [[]]):-
    C=[],
    !.
filter_trivial_true_false([C|L], [C|L1]):-
    filter_trivial_true_false(L, L1).

 filter_trivial_true_false([], []).


simplify_disj_2_seq([C],Vs, (false)):-
    unsat(C, Vs), !.
simplify_disj_2_seq([C],_, (C)):-
    !.
simplify_disj_2_seq([], _, (true)).
simplify_disj_2_seq(L,Vs, (false)):-
    listofList2YicesDisj(L, DisjYices),
    unsat(DisjYices, Vs),
    !.
simplify_disj_2_seq([C1, C2|L],Vs, L1):-
    (implies(C1, C2, Vs) ->
        Tmp= C2
    ;
    (implies(C2, C1, Vs) -> Tmp= C1; Tmp=(C1; C2))
    ),
    simplify_disj_2_seq([Tmp|L],Vs, L1).

disj_seq_2_list_list((S1; S2), L):-
    disj_seq_2_list_list(S1, L1),
    disj_seq_2_list_list(S2, L2),
    append(L1, L2, L).
disj_seq_2_list_list(S1, [S1]):-!.


implies(F1, F2, Vs):-
    unsat((F1, neg(F2)), Vs).

unsat(Formula, Vs):-
	yices_vars(Vs, int, VInts),
	yices_unsat(Formula,VInts).

sat(Formula, Vs):-
	yices_vars(Vs, int, VInts),
	yices_sat(Formula,VInts).

tautology(Formula, Vs):-
	yices_vars(Vs, int, VInts),
	yices_unsat(neg(Formula),VInts).

listofList2YicesDisj([A], [A]):- !.
listofList2YicesDisj([A|R], (A;R1)):- !,
	listofList2YicesDisj(R, R1).
listofList2YicesDisj([], [false]). %meaning false


is_list(X) :-
        var(X), !,
        fail.
is_list([]).
is_list([_|T]) :-
        is_list(T).

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

