:- module(precond, [main/1, precond/2, initCall/1, isIndentical/1], [dynamic, datafacts]).

:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(streams)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(common)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(linearize)).
:- include(chclibs(get_options)).
:- include(chclibs(messages)).

:- use_module(library(lists)).
:- use_module(library(terms_vars)).

:- use_module(chclibs(yices2_sat)).
:- use_module(ciao_yices(ciao_yices_2)).

:- use_module(dnf).

:- data flag/1.
:- dynamic(extractInitHead/0). %extract precond from clauses whose head is init (use -init option to get this option)

:- dynamic(yices_precond/1).

go(F) :-
    precond:main(['-prg',F]).
	
main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutFile),
	precond(File,Ls),
	(OutFile=user_output -> OutS=user_output; open(OutFile,write,OutS)),
	writeNodes(Ls,OutS),
	close(OutS).
	
precond(File,Ls) :-
    retractall(yices_precond(_)), %do once for each file
	load_file(File),
    yices_init,
	start_ppl,
	leafNodes(Ls),
    yices_exit,
	end_ppl.

leafNodes(Ls) :-
	findall((P,Cs),
		(leafNode(P,Cs),\+(Cs=[false])),
		Ls).
		
leafNode(P,Cs) :-
	my_clause(A,B,_),
    (extractInitHead-> separate_constraints(B,Cs1,[])
    ;
        separate_constraints(B,Cs1,_)
    ),
	checkNoLeaf(A,P,Cs1,Cs).


checkNoLeaf(A,A,Cs1,Cs) :-
	initCall(A),
    !,
	A =.. [_|Xs],
	projectOnto(Cs1,Xs,Cs).
checkNoLeaf(_,P,Cs1,Cs) :-
    (extractInitHead ->
        fail
    ;
        append(_,[(P=Q)|_],Cs1),
        isIndentical(P=Q),
        initCall(P),
        P =.. [_|Xs],
        projectOnto(Cs1,Xs,Cs)
    ).

initCall(A) :-
	functor(A,F,_),
	atom(F),
	atom_concat(init,_,F).

	
notIndenticals([]).
notIndenticals([C|Cs]) :-
	notIndentical(C),
	notIndenticals(Cs).

notIndentical(C) :-
	\+ isIndentical(C).
	
isIndentical(X = Y) :- %X and Y are identical
	X == Y.

/*
apply projection only if the current constraints involving init predicate is not covered by the already obtained precond
*/
projectOnto(Ds1,Zs,Cs) :-
    linearize(Ds1,Ds2),
    varset(Ds1, V1),
	varset(Ds2,Ws),
	setdiff(Ws,Zs,Ys),
	numbervars((Zs,Ds1),0,_),
    setunion(Zs, V1, Zs1),
    (covered(Ds2, Zs1)->
        Cs=[false]
    ;
        makePolyhedron(Ds2,H),
        project(H,Ys,H1),
        getConstraint(H1,Cs),
        (yices_precond(P)->
            retractall(yices_precond(P)),
            assert(yices_precond((P;Cs)))
        ;
            assert(yices_precond(Cs))
        )
    ).

covered(F1, Vs):-
    yices_precond(F2),
    %write(F1), write(' => '), write(F2), nl,
    implies(F1, F2, Vs).

writeNodes([],_).
writeNodes([(P,Cs)|Ls],S) :-
	writeq(S,P),
	write(S,':-'),
	write(S,Cs),
	write(S,'.'),
	nl(S),
	writeNodes(Ls,S).
	
setOptions(ArgV,File,OutFile) :-
	get_options(ArgV,Options,_),
	(member(programO(File),Options); 
			write(user_output,'No input file given.'),nl(user_output),fail),
	(member(outputFile(OutFile),Options); 
			OutFile=user_output),
	(member(extractInitHead,Options), assert(extractInitHead);
			true).

recognised_option('-prg',  programO(R),[R]).
recognised_option('-o',    outputFile(R),[R]).
recognised_option('-v', verbose, []).
recognised_option('-init', extractInitHead,[]).

cleanup :-
	retractall(my_clause(_,_,_)),
    retractall(extractInitHead),
    retractall(yices_precond(_)).
		
