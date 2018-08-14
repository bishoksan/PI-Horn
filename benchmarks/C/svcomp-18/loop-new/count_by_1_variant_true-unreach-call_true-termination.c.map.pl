% function declarations
fun(
 map__VERIFIER_assert,
 [id(cond,[type(int)])],
 [],
 addr(3)
). %/media/sf_share_vm//loop-new/assert.h@3
fun(
 main,
 [],
 [],
 addr(6)
). %/media/sf_share_vm//loop-new/count_by_1_variant_true-unreach-call_true-termination.c@3
fun(
 map__VERIFIER_nondet_int,
 [],
 [id(val,[type(int)])],
 addr(17)
). %utils.c@1
fun(
 map__VERIFIER_nondet_long,
 [],
 [id(val,[type(long)])],
 addr(18)
). %utils.c@6
fun(
 map__VERIFIER_nondet_uint,
 [],
 [id(val,[type(uint)])],
 addr(19)
). %utils.c@11
fun(
 map__VERIFIER_nondet_bool,
 [],
 [id(val,[type(bool)])],
 addr(20)
). %utils.c@16
fun(
 map__VERIFIER_assume,
 [id(expression,[type(int)])],
 [],
 addr(21)
). %utils.c@21
fun(
 error,
 [],
 [],
 addr(24)
). %utils.c@33
fun(
 errorFn,
 [],
 [],
 addr(25)
). %utils.c@38
fun(
 map__VERIFIER_error,
 [],
 [],
 addr(26)
). %utils.c@43
% function definitions
% __VeriMAP_ep
at(0,inst,[],call(main,[],none,addr(1))).
at(1,inst,[],halt).
% __VeriMAP_abort
at(2,inst,[],abort).
% __VERIFIER_assert
at(3,ifte,[],ite(bexp(not(aexp(id(cond,[scope(loc),type(int)])))),addr(4),addr(5))). %/media/sf_share_vm//loop-new/assert.h@4
at(4,inst,[],call(map__VERIFIER_error,[],none,addr(5))). %/media/sf_share_vm//loop-new/assert.h@5
at(5,ret,[],ret(map__VERIFIER_assert,none)). %/media/sf_share_vm//loop-new/assert.h@7
% main
at(6,inst,[],asgn(id(i,[scope(glb),type(int)]),aexp(const(0,int)),addr(7))). %/media/sf_share_vm//loop-new/count_by_1_variant_true-unreach-call_true-termination.c@5
at(7,block,[],goto(addr(8))). %/media/sf_share_vm//loop-new/count_by_1_variant_true-unreach-call_true-termination.c@5 (block)
at(8,inst,[],goto(addr(9))). %/media/sf_share_vm//loop-new/count_by_1_variant_true-unreach-call_true-termination.c@5 (loop)
at(9,inst,['while_continue'],goto(addr(10))).
at(10,ifte,[],ite(bexp(neq(aexp(id(i,[scope(glb),type(int)])),aexp(const(1000000,int)))),addr(12),addr(11))). %/media/sf_share_vm//loop-new/count_by_1_variant_true-unreach-call_true-termination.c@5
at(11,goto,[],goto(addr(15))). %/media/sf_share_vm//loop-new/count_by_1_variant_true-unreach-call_true-termination.c@5 (goto)
at(12,block,[],goto(addr(13))). %/media/sf_share_vm//loop-new/count_by_1_variant_true-unreach-call_true-termination.c@6 (block)
at(13,inst,[],call(map__VERIFIER_assert,[bexp(lte(aexp(id(i,[scope(glb),type(int)])),aexp(const(1000000,int))))],none,addr(14))). %/media/sf_share_vm//loop-new/count_by_1_variant_true-unreach-call_true-termination.c@6
at(14,inst,[],asgn(id(i,[scope(glb),type(int)]),aexp(plus(aexp(id(i,[scope(glb),type(int)])),aexp(const(1,int)))),addr(9))). %/media/sf_share_vm//loop-new/count_by_1_variant_true-unreach-call_true-termination.c@5
at(15,inst,['while_break'],goto(addr(16))).
at(16,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-new/count_by_1_variant_true-unreach-call_true-termination.c@8
% __VERIFIER_nondet_int
at(17,ret,[],ret(map__VERIFIER_nondet_int,aexp(id(val,[scope(loc),type(int)])))). %utils.c@3
% __VERIFIER_nondet_long
at(18,ret,[],ret(map__VERIFIER_nondet_long,aexp(id(val,[scope(loc),type(long)])))). %utils.c@8
% __VERIFIER_nondet_uint
at(19,ret,[],ret(map__VERIFIER_nondet_uint,aexp(id(val,[scope(loc),type(uint)])))). %utils.c@13
% __VERIFIER_nondet_bool
at(20,ret,[],ret(map__VERIFIER_nondet_bool,aexp(id(val,[scope(loc),type(bool)])))). %utils.c@18
% __VERIFIER_assume
at(21,ifte,[],ite(bexp(not(aexp(id(expression,[scope(loc),type(int)])))),addr(22),addr(23))). %utils.c@22
at(22,goto,['LOOP'],goto(addr(22))). %utils.c@22 (goto)
at(23,ret,[],ret(map__VERIFIER_assume,none)). %utils.c@23
% error
at(24,goto,['ERROR'],goto(addr(24))). %utils.c@35 (goto)
% errorFn
at(25,goto,['ERROR'],goto(addr(25))). %utils.c@40 (goto)
% __VERIFIER_error
at(26,goto,['ERROR'],goto(addr(26))). %utils.c@45 (goto)
% global variables
gvars([
  id(i,[type(int)]) %/media/sf_share_vm//loop-new/count_by_1_variant_true-unreach-call_true-termination.c@2
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
preds(8,[7]).
succs(8,[9]).
preds(9,[14,8]).
succs(9,[10]).
preds(10,[9]).
succs(10,[11,12]).
preds(11,[10]).
succs(11,[15]).
preds(12,[10]).
succs(12,[13]).
preds(13,[12]).
succs(13,[14]).
preds(14,[13]).
succs(14,[9]).
preds(15,[11]).
succs(15,[16]).
preds(16,[15]).
succs(16,[]).
preds(17,[]).
succs(17,[]).
preds(18,[]).
succs(18,[]).
preds(19,[]).
succs(19,[]).
preds(20,[]).
succs(20,[]).
preds(21,[]).
succs(21,[23,22]).
preds(22,[22,21]).
succs(22,[22]).
preds(23,[21]).
succs(23,[]).
preds(24,[24]).
succs(24,[24]).
preds(25,[25]).
succs(25,[25]).
preds(26,[26]).
succs(26,[26]).
:- assert(data_types([bool,uint,long,int])).
