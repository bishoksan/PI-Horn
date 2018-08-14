:- module(counterExampleYices, [checkCounterExample/3, counterExample/2, readCex/2, separateLinearConstraints/3], []).

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(lists)).

:- use_module(chclibs(linearize)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(program_loader)).
:- use_module(chclibs(common)).
:- use_module(chclibs(yices2_sat)).
:- use_module(ciao_yices(ciao_yices_2)).
 :- use_module(library(terms_vars)).

:- include(chclibs(get_options)).
:- include(chclibs(messages)).



:- data flag/1.
recognised_option('-v', verbose, []).

% Given a file containing possibly an abstract trace (abstract
% counterexample) and program P, it checks whether the counterexample
% is feasible or not with respect to P. It returns "safe" is the file
% containts no abstract trace; "unsafe" if the counterexample is
% feasible or "unknown" otherwise.

% NOTE: TraceF is the outcome of cpascc.pl with -cex option

counterExample(ArgV, Result) :-
	get_options(ArgV,Options,Args0),
	retractall_fact(flag(verbose)),
	( member(verbose, Options) ->
	    assertz_fact(flag(verbose))
	; true
	),
	%
	Args0 = [F, TraceF],
	unsafe(F, TraceF, Result).
	
unsafe(F,PFile, Result) :-
	readCex(PFile,Cex),
	checkCounterExample(Cex,F, Result).
	
readCex(PFile,Cex) :-
	open(PFile,read,S),
	read(S,C),
	existsCex(S,C,Cex),
	close(S).

existsCex(_,end_of_file,no) :-
	!.
existsCex(_,(cex(Cex)),Cex) :-
	!.
existsCex(_,(counterexample(Cex)),Cex) :-
	!.
existsCex(S,_,Cex) :-
	read(S,C1),
	existsCex(S,C1,Cex).
	
checkCounterExample(no, _, Result) :-
	!,
	Result=safe.
checkCounterExample(Cex, F, Result) :-
    write('Counter example: '), write(Cex), nl,
	verbose_message(['Counter example: ', Cex]),
	%
	load_file(F),
	start_ppl,
	( ( checkTrace([false],[],[Cex])
	  ; checkTrace([false_ans],[],[Cex])
	  ) ->
	    Result0=unsafe
	; Result0=unknown
	),
	end_ppl,
	Result = Result0.

%if succeeds then the program is unsafe
checkTrace([],Cs2,_):-
    check_sat_yices(Cs2).
checkTrace([B|Bs],Cs,[T|Ts]) :-
	T =..[C|Ts1],
	my_clause(B,Bs1,C),
	separate_constraints(Bs1,Cs1,Bs2),
	append(Bs2,Bs,Bs3),
	append(Cs1,Cs,Cs2),
    separateLinearConstraints(Cs2, CsL, _),
    %check_sat_yices(Cs2),
	append(Ts1,Ts,Ts2),
	checkTrace(Bs3,CsL,Ts2).


check_sat_yices(Cs):-
    %filterOutTargetAndGoalVarConstr(Cs1,Cs),
    yices_init,
    varset(Cs, Vars),
    numbervars(Cs,0,_),
	yices_vars(Vars, int, YicesVars),
    (yices_sat(Cs, YicesVars)-> yices_exit, true; yices_exit, !, fail).

separateLinearConstraints([],[],[]).
separateLinearConstraints([C|Cs],[C|Cs1],Cs2) :-
    %write('before '),write(C), nl,
	linear_constraint(C),
	!,
    %write('after '),write(C), nl,
	separateLinearConstraints(Cs,Cs1,Cs2).
separateLinearConstraints([C|Cs],Cs1,[C|Cs2]) :-
	separateLinearConstraints(Cs,Cs1,Cs2).

filterOutTargetAndGoalVarConstr([], []).
filterOutTargetAndGoalVarConstr([(_=targetvars(_))|Cs], Cs1):-
    !,
    filterOutTargetAndGoalVarConstr(Cs, Cs1).
filterOutTargetAndGoalVarConstr([(_=goalvars(_))|Cs], Cs1):-
    !,
    filterOutTargetAndGoalVarConstr(Cs, Cs1).
filterOutTargetAndGoalVarConstr([C|Cs], [C|Cs1]):-
    !,
    filterOutTargetAndGoalVarConstr(Cs, Cs1).
