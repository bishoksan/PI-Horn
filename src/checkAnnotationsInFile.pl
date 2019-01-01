:- module(checkAnnotationsInFile,[checkForAnnotations/2], []).

:- use_module(chclibs(program_loader)).

:- use_module(library(streams)).
:- use_module(library(write)).

% Sanity Check: check the presence of spec:- false, spec:- safe and init():-   clauses, if  present returns
% 0 (file has both safe, init and unsafe predicates) otherwise 1 (sanity check failed).
checkForAnnotations(F, Result):-
	load_file(F),
( my_clause(spec, [false], _), my_clause(spec, [safe], _), my_clause(A,_,_),functor(A, init, _)->
	    Result=0
        ; Result=1
          ,open('failed_sanity.txt', append, SP),  write(SP, F), nl(SP), close(SP)
	).

