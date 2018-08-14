:- module(constrainInitStates, _).

:- use_module(chclibs(load_simple)).
:- use_module(chclibs(common)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(linearize)).

:- use_module(library(lists)).
:- use_module(library(terms_vars)).
:- use_module(negate).
:- use_module(precond).

:- use_module(chclibs(yices2_sat)).
:- use_module(ciao_yices(ciao_yices_2)).

:- dynamic(prop/2).
:- dynamic(all_cls/0). %the prop. is injected in all clauses if it is the head and is init cls or init pred appears in the body

/*
Limitations: assume property file to contain single version of init predicate

Given a program P and a file Prop with constraints on initial states (specified via "initXXX" predicates), it replaces all the initial states of P with that of Prop

P:
init_1(X):-X>0.
init(X):-X>0.

p(X):-init_1(X).
p(X):-init(X).

Prop:

init(X):-X>0.
init(X):-X<0.


Result:
init(X):-X>0.
init(X):-X<0.

init_1(X):-X>0.
init_1(X):-X<0.

p(X):-init_1(X).
p(X):-init(X).

-all_cls Options also constraints initial states of other clauses (for now linear where initXXX(.)=initXXX(.) appears at most once in the body)

P:
init_1(X):-X>0.

p(X, Y):-init_1(X)=init_1(X), Y=0.
q(X):- p(X,_).

Prop:

init(X):-X>0.
init(X):-X<0.


Result:

init_1(X):-X>0.
init_1(X):-X<0.

p(X, Y):-init_1(X)=init_1(X),X>0, Y=0.
p(X, Y):-init_1(X)=init_1(X), X<0, Y=0.
q(X):- p(X,_).


*/


go(F,FP) :-
	constrainInitStates:main(['-prg',F,'-pre',FP]).
	
main(ArgV) :-
	constrainInitStates:cleanup,
	setOptions(ArgV,File,PFile,OutS),
	readPropFile(PFile),
    load_file(File),
    yices_init,
    %be careful there may not be any init facts
    replaceInitCls(OutS),
    genConstrainedCls(OutS),
    yices_exit,
	close(OutS).

genConstrainedCls(OutS):-
    my_clause(A,B,_),
    %B can contain init=init or none
    (all_cls->
        (containInitPred(B, Xs)->
            writeInitBodyClauses(A,B, Xs,OutS)
        ;
            writeSatCls(A,B,OutS)
        )
    ;
        writeSatCls(A,B,OutS)
    ),
    fail.
genConstrainedCls(_).

%Xs is the set of vars of init predicates
containInitPred(B, Xs):-
        separate_constraints(B,Cs1,_),
        append(_,[(P=Q)|_],Cs1),
        initCall(P),
        isIndentical(P=Q),
        P =..[_|Xs].

replaceInitCls(S):-
    findall((A, C), (my_clause(A,_,C), initCall(A)), Ls),
    getIds(Ls, Ids),
    getUniqueAtoms(Ls, Atoms),
    retractallIds(Ids),
    writeCls(Atoms, [], S).

getUniqueAtoms(Ls, Atoms):-
    %setof can fail
    (setof(P/N, (member((A,_), Ls), functor(A, P,N)), Atoms)-> true
    ;
        Atoms=[]
    ).


getIds([],  []).
getIds([(_, C)|Ls],  [C|Lc]):-
    getIds(Ls,  Lc).


retractallIds([]).
retractallIds([Id|Ids]):-
    retractall(my_clause(_,_,Id)),
    retractallIds(Ids).

writeCls([], _, _).
writeCls([P/N|L], B, S):-
    functor(A,P,N),
    writeInitClauses(A,B,S),
    writeCls(L, B, S).

writeInitClauses(A,B,S):-
    prop(A1,C),
    A1=..[_|Xs],  A=..[_|Xs], %unifying vars
    append(B,C, B1),
    writeSatCls(A,B1,S),
    fail.
writeInitClauses(_,_,_).

writeInitBodyClauses(A,B,Xs, S):-
    prop(A1,C),
    A1=..[_|Xs],
    append(C,B, B1),
    writeSatCls(A,B1,S),
    fail.
writeInitBodyClauses(_,_,_,_).



	
% get_options/3 provided by Michael Leuschel
get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (constrainInitStates:recognised_option(X,Opt,Values) ->
	  ( append(Values, Rest, T),
	    RT = Rest,
	    Options = [Opt|OT], Args = AT
	  )
   ;
	  (
	    Options = OT,	Args = [X|AT],
	    RT = T
	  )
   ),
   constrainInitStates:get_options(RT,OT,AT).
   
setOptions(ArgV,File,PFile,OutS) :-
	constrainInitStates:get_options(ArgV,Options,_),
	(member(programO(File),Options); 
			write(user_output,'No input file given.'),nl(user_output),fail),
	(member(precondfile(PFile),Options); 
			write(user_output,'No precondition file given.'),nl(user_output),fail),
    (member(all_cls,Options)-> assert(all_cls);true),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output).

	

recognised_option('-prg',  programO(R),[R]).
recognised_option('-o',    outputFile(R),[R]).
recognised_option('-pre',  precondfile(R),[R]).
recognised_option('-all-cls',  all_cls,[]).
cleanup :-
	retractall(my_clause(_,_,_)),
	retractall(prop(_,_)),
    retractall(all_cls).
		
readPropFile(PFile) :-
	open(PFile,read,S),
	read(S,C),
	readPropFacts(S,C),
	close(S).
	
readPropFacts(_,end_of_file) :-
	!.
readPropFacts(S,(H:-C)) :-
	assert(prop(H,C)),
	read(S,C1),
	readPropFacts(S,C1).

writeSatCls(H,B,S):-
    separate_constraints(B, Cs, _),
    linearize(Cs,Cs1),
    varset(Cs1, Vs),
    numbervars((H,B), 0, _),
    sat(Cs1, Vs),
    !,
    writeClause(H,B,S).
writeSatCls(_,_,_).

sat(Formula, Vs):-
	yices_vars(Vs, int, VInts),
	yices_sat(Formula,VInts).

writeClause(H,B,S) :-
	writeq(S,H),
	write(S,' :-'),
	nl(S),
	writeBodyAtoms(S,B),
	write(S,'.'),
	nl(S).
	
writeClauses([(H:-B)|Rs],S) :-
	writeClause(H,B,S),
	writeClauses(Rs,S).
writeClauses([],_).
	
writeBodyAtoms(S,[]) :-
	!,
	write(S,'   '),
	write(S,true).
writeBodyAtoms(S,[B]) :-
	!,
	write(S,'   '),
	writeq(S,B).
writeBodyAtoms(S,[B1,B2|Bs]) :-
	write(S,'   '),
	writeq(S,B1),
	write(S,','),
	nl(S),
	writeBodyAtoms(S,[B2|Bs]).
