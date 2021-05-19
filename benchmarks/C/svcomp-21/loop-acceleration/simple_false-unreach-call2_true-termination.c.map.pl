% function declarations
fun(
 map__VERIFIER_assert,
 [id(cond,[type(int)])],
 [],
 addr(3)
). %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@3
fun(
 main,
 [],
 [],
 addr(6)
). %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@12
fun(
 map__VERIFIER_nondet_int,
 [],
 [id(val,[type(int)])],
 addr(15)
). %utils.c@1
fun(
 map__VERIFIER_nondet_long,
 [],
 [id(val,[type(long)])],
 addr(16)
). %utils.c@6
fun(
 map__VERIFIER_nondet_uint,
 [],
 [id(val,[type(uint)])],
 addr(17)
). %utils.c@11
fun(
 map__VERIFIER_nondet_bool,
 [],
 [id(val,[type(bool)])],
 addr(18)
). %utils.c@16
fun(
 map__VERIFIER_assume,
 [id(expression,[type(int)])],
 [],
 addr(19)
). %utils.c@21
fun(
 error,
 [],
 [],
 addr(22)
). %utils.c@33
fun(
 errorFn,
 [],
 [],
 addr(23)
). %utils.c@38
fun(
 map__VERIFIER_error,
 [],
 [],
 addr(24)
). %utils.c@43
% function definitions
% __VeriMAP_ep
at(0,inst,[],call(main,[],none,addr(1))).
at(1,inst,[],halt).
% __VeriMAP_abort
at(2,inst,[],abort).
% __VERIFIER_assert
at(3,ifte,[],ite(bexp(not(aexp(id(cond,[scope(loc),type(int)])))),addr(4),addr(5))). %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@4
at(4,inst,[],call(map__VERIFIER_error,[],none,addr(5))). %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@5
at(5,ret,[],ret(map__VERIFIER_assert,none)). %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@7
% main
at(6,block,[],goto(addr(7))). %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@15 (block)
at(7,inst,[],goto(addr(8))). %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@15 (loop)
at(8,inst,['while_continue'],goto(addr(9))).
at(9,ifte,[],ite(bexp(lt(aexp(id(x,[scope(glb),type(uint)])),aexp(const(268435455,uint)))),addr(11),addr(10))). %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@15
at(10,goto,[],goto(addr(12))). %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@15 (goto)
at(11,inst,[],asgn(id(x,[scope(glb),type(uint)]),aexp(plus(aexp(id(x,[scope(glb),type(uint)])),aexp(const(1,uint)))),addr(8))). %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@16
at(12,inst,['while_break'],goto(addr(13))).
at(13,inst,[],call(map__VERIFIER_assert,[bexp(gt(aexp(id(x,[scope(glb),type(uint)])),aexp(const(268435455,uint))))],none,addr(14))). %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@19
at(14,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@20
% __VERIFIER_nondet_int
at(15,ret,[],ret(map__VERIFIER_nondet_int,aexp(id(val,[scope(loc),type(int)])))). %utils.c@3
% __VERIFIER_nondet_long
at(16,ret,[],ret(map__VERIFIER_nondet_long,aexp(id(val,[scope(loc),type(long)])))). %utils.c@8
% __VERIFIER_nondet_uint
at(17,ret,[],ret(map__VERIFIER_nondet_uint,aexp(id(val,[scope(loc),type(uint)])))). %utils.c@13
% __VERIFIER_nondet_bool
at(18,ret,[],ret(map__VERIFIER_nondet_bool,aexp(id(val,[scope(loc),type(bool)])))). %utils.c@18
% __VERIFIER_assume
at(19,ifte,[],ite(bexp(not(aexp(id(expression,[scope(loc),type(int)])))),addr(20),addr(21))). %utils.c@22
at(20,goto,['LOOP'],goto(addr(20))). %utils.c@22 (goto)
at(21,ret,[],ret(map__VERIFIER_assume,none)). %utils.c@23
% error
at(22,goto,['ERROR'],goto(addr(22))). %utils.c@35 (goto)
% errorFn
at(23,goto,['ERROR'],goto(addr(23))). %utils.c@40 (goto)
% __VERIFIER_error
at(24,goto,['ERROR'],goto(addr(24))). %utils.c@45 (goto)
% global variables
gvars([
  id(x,[type(uint)]) %/media/sf_share_vm//loop-acceleration/simple_false-unreach-call2_true-termination.c@10
]).
:- assert(undef_funs([])).
preds(0,[]).
succs(0,[1]).
preds(1,[0]).
succs(1,[]).
preds(2,[]).
succs(2,[]).
preds(3,[]).
succs(3,[5,4]).
preds(4,[3]).
succs(4,[5]).
preds(5,[3,4]).
succs(5,[]).
preds(6,[]).
succs(6,[7]).
preds(7,[6]).
succs(7,[8]).
preds(8,[11,7]).
succs(8,[9]).
preds(9,[8]).
succs(9,[10,11]).
preds(10,[9]).
succs(10,[12]).
preds(11,[9]).
succs(11,[8]).
preds(12,[10]).
succs(12,[13]).
preds(13,[12]).
succs(13,[14]).
preds(14,[13]).
succs(14,[]).
preds(15,[]).
succs(15,[]).
preds(16,[]).
succs(16,[]).
preds(17,[]).
succs(17,[]).
preds(18,[]).
succs(18,[]).
preds(19,[]).
succs(19,[21,20]).
preds(20,[20,19]).
succs(20,[20]).
preds(21,[19]).
succs(21,[]).
preds(22,[22]).
succs(22,[22]).
preds(23,[23]).
succs(23,[23]).
preds(24,[24]).
succs(24,[24]).
:- assert(data_types([bool,uint,long,int])).
