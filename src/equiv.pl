:- module(equiv, [equiv/3, main/1], [dynamic]).
/*
Given a program P, its partially evaluated program P', and their respective models M and M', this program checks if M and M' are equivalent.
*/

:- dynamic pred_map/2. % map between logen predicates and the original ones
:- dynamic orig_inv/1.
:- dynamic pe_inv/1.

:- use_module(library(lists)).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(aggregates)).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(yices2_sat)).
:- use_module(ciao_yices(ciao_yices_2)).
:- use_module(library(terms_vars)).

:- use_module(library(process), [process_call/3]).

:- use_module(chclibs(common)).
:- include(chclibs(get_options)).
:- include(chclibs(messages)).

:- data flag/1.

main(ArgV) :-
	cleanup,
	setOptions(ArgV,PeFile,CHAOFile, CHAPeFile),
    (equiv(PeFile, CHAOFile, CHAPeFile)->  halt(0); halt(1)). %if equivalent, exits with 0 otherwise 1.

setOptions(ArgV,PeFile,CHAOFile, CHAPeFile) :-
	get_options(ArgV,Options,_),
	(member(programO(PeFile),Options);
			write(user_output,'No input file given.'),nl(user_output),fail),
	(member(chaFileOrig(CHAOFile),Options);
			write(user_output,'No input file given.'),nl(user_output),fail),
    (member(chaFilePe(CHAPeFile),Options);
			write(user_output,'No input file given.'),nl(user_output),fail).


recognised_option('-prg',  programO(R),[R]).
recognised_option('-chaO',    chaFileOrig(R),[R]).
recognised_option('-peO',    chaFilePe(R),[R]).
recognised_option('-v', verbose, []).

/* P_pe is always assumed to be more precise than the P_orig. So  equiv means P_orig => P_pe*/

equiv(P_pe, M_orig, M_pe):-
    %get the map between PE program's predicates and the original predicates
    get_predicateMap(P_pe),
    implies(M_orig, M_pe).


 %assume that a model contains at most one constrained facts for each atom

implies(Mo, Mpe):-
    open(Mo, read, Ho),
    open(Mpe, read, Hpe),
    read(Ho, Co),
    save_orig_inv(Ho, Co),
    read(Hpe, Cpe),
    save_pe_inv(Hpe, Cpe),
    close(Ho),
    close(Hpe),
    orig_invs(Ls),
    construct_formula(Ls, [], Formula), % [] = true
    !,
    check_unsat(neg(Formula)).




construct_formula([], F, F).
construct_formula([(H,C)|Is],F, Formula):-
 %   get_neg_formula(C, Cneg),
    pe_predicates(H, PE_preds),
    H =.. [_|Xs],
    get_pe_formula(PE_preds, Xs, PeFormula),
    append([(neg(C); PeFormula)], F, Fs),
construct_formula(Is, Fs, Formula).


%get_neg_formula([F], [neg(H)]).

orig_invs(L):-
    findall((H,B), orig_inv((H,B)), L).


save_pe_inv(_, end_of_file):-
	!.
save_pe_inv(S,(H:-B)):-
	assert(pe_inv((H,B))),
	read(S,C),
	save_pe_inv(S, C).

save_orig_inv(_, end_of_file):-
	!.
save_orig_inv(S,(H:-B)):-
	assert(orig_inv((H,B))),
	read(S,C),
	save_orig_inv(S, C).

get_pe_formula([], _, [false]).
get_pe_formula([P/_|Ps], Xs, [(C;PeFormula)]):-
    H =.. [P|Xs],
    pe_inv((H, C)),
    get_pe_formula(Ps, Xs, PeFormula).


check_unsat(Formula):-
    varset(Formula, Vs),
	numbervars(Formula, 0, _),
    write('Is unsat: '), write(Formula), nl,
	yices_vars(Vs, int, VInts),
	yices_init,
	yices_unsat(Formula,VInts),
	yices_exit.


/*gives a set of pe predicates corresponding to a original predicate*/

pe_predicates(Pred, PE_preds):-
    functor(Pred, Name, Arity),
    setof(P, pred_map(P, Name/Arity), PE_preds), !.
pe_predicates(_, []). %if setof fails

%produces  a map between PE program's predicates using logen and the original predicates

get_predicateMap_logen(F):-
    cleanup,
    load_file(F),
    F1='comments.pl',
    parse_comments_logen(F, F1),
    load_file(F1),
    process_call(path('rm'), [F1],[]),
    predicateMap_logen.
    %printMap.

 /*
sed -n 's/^\/\*  \(.*\)\. \*\//\1./p' $1 > logen_renames.pl
in prolog '\' has to be replaced by '\\'

*/


parse_comments_logen(F, F1):-
    process_call(path('sed'), ['-n', 's/^\\/\\*  \\(.*\\)\\. \\*\\//\\1./p', F],
	             [stdout(file(F1))]).


/*
map pred_map(PE_predicates, Original_predicates)

search for clauses whose:
     -body contains at most one non-constraint atom
    - the head takes 3 arguments
    - the first argument is a single element list


*/
predicateMap_logen:-
    %look for only one atom in the body
    my_clause(H, [B], _),
    H=..[_|[[A1], _, _]],
    functor(A1, P1,N1),
    functor(B, P,N),
    assert(pred_map(P/N, P1/N1)),
    fail.
predicateMap_logen.


get_predicateMap(F):-
    retractall(pred_map(_,_)),
    load_file(F),
    predicateMap.
    %printMap.

/*collect all head predicates and get renaming undone*/
predicateMap:-
    my_clause(H1, _, _),
    functor(H1, P1,N1),
    get_original_predicate_name(P1, P),
    save_pred_map(P1, P, N1),
    fail.
predicateMap.

save_pred_map(P1, P, N1):-
    (pred_map(P1/N1, _) ->
        true
    ;
        assert(pred_map(P1/N1, P/N1))).

/* remove the last ___Version from P1 to get the original predicate P*/

get_original_predicate_name(P1, P):-
    name(P1, P1N),
    findall(PN, append(PN, [95, 95,95|_], P1N),L),
    (L=[]-> 
    	P=P1
    ;  
    	last_elem(L, Last),  name(P, Last)
    ).

%requires the list to be non-empty    
last_elem([X], X).
last_elem([_|L], Y):-
    	last_elem(L,Y).


printMap:-
    pred_map(O,PE),
    write(O), write(' -- '), write(PE), nl,
    fail.
printMap.

cleanup:-
    retractall(pred_map(_,_)),
    retractall(orig_inv(_)),
    retractall(pe_inv(_)).





