:- module(insertPrecond, _, [dynamic]).

:- use_module(chclibs(load_simple)).
:- use_module(chclibs(common)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(linearize)).

:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(terms_vars)).
:- use_module(negate).
:- use_module(precond).

:- dynamic(prop/2).
:- dynamic(neg/0). %controls either to insert negation of the precond or not

/*
The original file is modified to insert both the negation of precond as well as the precond into the initial states

need fixing
*/


go(F,FP) :-
	insertPrecond:main(['-prg',F,'-pre',FP]).
	
main(ArgV) :-
	insertPrecond:cleanup,
	setOptions(ArgV,File,PFile,OutS),
	precond(File,Ls), 	% given initial conditions
	readPropFile(PFile),  
	start_ppl,
	proplist(L),		% inferred preconditions
	insertPreconds(L,Ls,PLs),
	end_ppl,
	writeInitClauses(PLs,OutS),
	writeNonInitClauses(Ls,OutS),
	close(OutS).

	
% get_options/3 provided by Michael Leuschel
get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (insertPrecond:recognised_option(X,Opt,Values) ->
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
   insertPrecond:get_options(RT,OT,AT).
   
setOptions(ArgV,File,PFile,OutS) :-
	insertPrecond:get_options(ArgV,Options,_),
	(member(programO(File),Options); 
			write(user_output,'No input file given.'),nl(user_output),fail),
	(member(precondfile(PFile),Options); 
			write(user_output,'No precondition file given.'),nl(user_output),fail),
    (member(neg,Options)-> assert(neg);true),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output).

	


recognised_option('-prg',  programO(R),[R]).
recognised_option('-o',    outputFile(R),[R]).
recognised_option('-pre',  precondfile(R),[R]).
recognised_option('-neg',  neg,[]).

cleanup :-
	retractall(my_clause(_,_,_)),
	retractall(prop(_,_)),
    retractall(neg).
		
readPropFile(PFile) :-
	open(PFile,read,S),
	read(S,C),
	readPropFacts(S,C),
	close(S).
	
readPropFacts(_,end_of_file) :-
	!.
readPropFacts(S,(H:-C)) :-
	varset(H,Xs),
	dummyCList(Xs,DCL),
	append(C,DCL,CsL),
	assert(prop(H,CsL)),
	read(S,C1),
	readPropFacts(S,C1).
	
proplist(LD) :-
	findall((A,C),(
		prop(A,C),
		numbervars((A,C),0,_)),
	Ps),
	makePropList(Ps,0,_,[],L),
	negDisjuncts(L,LD).
	
makePropList([],K,K,L,L).
makePropList([(A,C)|Ps],K0,K2,L0,L2) :-
	makePolyhedron(C,H),
	functor(A,P,N),
	N>0,
	!,
	addProperty(P/N,H,C,K0,K1,L0,L1),
	makePropList(Ps,K1,K2,L1,L2).
makePropList([_|Ps],K0,K1,L0,L1) :-
	makePropList(Ps,K0,K1,L0,L1).

addProperty(P/N,Hp,_,K0,K1,[pred(P/N,Props)|Ps0],[pred(P/N,Props1)|Ps0]) :-
	!,
	addPredProps(Props,Hp,K0,K1,Props1).
addProperty(P/N,Hp,C,K0,K1,[PredProps|Ps0],[PredProps|Ps1]) :-
	addProperty(P/N,Hp,C,K0,K1,Ps0,Ps1).
addProperty(P/N,Hp,_,K0,K1,[],[pred(P/N,[Hp-K0])]) :-
	K1 is K0+1.

addPredProps([],Hp,K0,K1,[Hp-K0]) :-
	K1 is K0+1.
addPredProps([H-Id|Props],Hp,K,K,[H-Id|Props]) :-
	equivalent(H,Hp),
	!.
addPredProps([H-Id|Props],Hp,K0,K1,[H-Id|Props1]) :-
	addPredProps(Props,Hp,K0,K1,Props1).
	
negDisjuncts([pred(P/N,Props)|L],[pred(P/N,NDisj1)|L1]) :-
	makeDisj(Props,Disj),
    (neg->
        negate(Disj,NDisj),
        simplify(NDisj,NDisj1)
    ;
        NDisj1=Disj
    ),
	negDisjuncts(L,L1).
negDisjuncts([],[]).

makeDisj([H-_],D1) :-
	!,
	getConstraint(H,D1).
makeDisj([H-_|Ps],(D1;D2)) :-
	getConstraint(H,D1),
	makeDisj(Ps,D2).
	
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

% Compute the refined preconditions by forming the conjunction of the 
% original preconditions with the inferred safety preconditions.


insertPreconds(InferredCond,OrigCond,PLs) :-
    write('inferred '), write(InferredCond), nl, write('orig '), write(OrigCond), nl,
	convertToDisjuncts(OrigCond,LDs),
    write('orig disj '), write(LDs), nl,
	gatherPreconds(LDs,InferredCond,LDs1),
    write('gathered precond '),write(LDs1), nl,
	generatePreconds(LDs1,PLs,[]), write(PLs), nl.
	
convertToDisjuncts([(A,C)|Ls],LDs) :-
	convertToDisjuncts(Ls,LDs1),
	insertDisj(LDs1,A,C,LDs).
convertToDisjuncts([],[]).

insertDisj([(A :- D)|LDs],A,C,[(A :- (C;D))|LDs]) :-
	!.
insertDisj([(A1 :- D)|LDs],A,C,[(A1 :- D)|LDs1]) :-
	insertDisj(LDs,A,C,LDs1).
insertDisj([],A,C,[(A :- C)]).

gatherPreconds([(A :- Disj)|LDs],L,[(A :- Es)|LDs1]) :-
     %write('mapping preds '), write(L), write(' ' ), nl, write(L1), nl,
	mapPreds(L,L1,A,Ds),
    write('mapping preds '), write(L), nl, write(' useless list ' ), write(L1), nl,
    write(Ds), nl,
	conjunct(Disj,Ds,Es),
	gatherPreconds(LDs,L1,LDs1).
gatherPreconds([],L,[]) :-
	(L = [_|_] -> 
		%write('Unmapped preconditions: '), %no idea why we need it
		%write(L),
		nl;
	true).

mapPreds([pred(P/N,Ds)],[],A,Ds) :-
	mapsTo(P/N,A),
	!.
mapPreds([pred(P/N,Ds)],[pred(P/N,Ds)],_,Ds) :-
	!.
mapPreds([pred(P/N,Ds)|L],L1,A,Ds0) :-
   % write(P/N),nl,
	mapsTo(P/N,A),
	!,
	mapPreds(L,L1,A,Ds1),
	disjunct(Ds,Ds1,Ds0).
mapPreds([pred(P/N,Ds)|L],[pred(P/N,Ds)|L1],A,Ds0) :-
    !,
	mapPreds(L,L1,A,Ds0).
mapPreds([],[],_,[]):-write('this case popping up'), nl.

mapsTo(P/M,A) :-
	functor(A,P1,M),
	atom_concat(P,_,P1).
/* johns version
mapsTo(P/M,A) :-
	functor(A,P1,M),
	atom_concat(P1,_,P).
*/
	
	
generatePreconds([(A :- Es)|LDs],PLs0,PLs2) :-
	generatePrecondClauses(Es,A,PLs0,PLs1),
	generatePreconds(LDs,PLs1,PLs2).
generatePreconds([],PLs,PLs).

generatePrecondClauses((E1;E2),A,[(A :- E1)|PLs0],PLs1) :-
	satisfiable(E1,_),
	!,
	generatePrecondClauses(E2,A,PLs0,PLs1).
generatePrecondClauses((_;E2),A,PLs0,PLs1) :-
	!,
	generatePrecondClauses(E2,A,PLs0,PLs1).
generatePrecondClauses(E,A,[(A :- E)|PLs0],PLs0) :-
%	satisfiable(E,_),
	!.
generatePrecondClauses(_,_,PLs0,PLs0).
	
% write definitions of the initial predicates

writeInitClauses([(A :- E)|PLs],S) :-
	writeClause(A,E,S),
	writeInitClauses(PLs,S).
writeInitClauses([],_).

% write original clauses, omitting the initial predicates

writeNonInitClauses(Ls,S) :-
	my_clause(A,B,_),
	\+ initPred(A,Ls),
	numbervars((A,B),0,_),
	writeClause(A,B,S),
	fail.
writeNonInitClauses(_,_).

initPred(A,Ls) :-
	member((A,_),Ls),
	!.
	
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
