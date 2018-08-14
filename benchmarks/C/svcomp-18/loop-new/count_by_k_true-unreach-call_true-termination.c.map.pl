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
 [id(i,[type(int)])],
 addr(6)
). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@4
fun(
 map__VERIFIER_nondet_int,
 [],
 [id(val,[type(int)])],
 addr(20)
). %utils.c@1
fun(
 map__VERIFIER_nondet_long,
 [],
 [id(val,[type(long)])],
 addr(21)
). %utils.c@6
fun(
 map__VERIFIER_nondet_uint,
 [],
 [id(val,[type(uint)])],
 addr(22)
). %utils.c@11
fun(
 map__VERIFIER_nondet_bool,
 [],
 [id(val,[type(bool)])],
 addr(23)
). %utils.c@16
fun(
 map__VERIFIER_assume,
 [id(expression,[type(int)])],
 [],
 addr(24)
). %utils.c@21
fun(
 error,
 [],
 [],
 addr(27)
). %utils.c@33
fun(
 errorFn,
 [],
 [],
 addr(28)
). %utils.c@38
fun(
 map__VERIFIER_error,
 [],
 [],
 addr(29)
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
at(6,ifte,[],ite(bexp(lte(aexp(const(0,int)),aexp(id(k,[scope(glb),type(int)])))),addr(7),addr(9))). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@7
at(7,ifte,[],ite(bexp(lte(aexp(id(k,[scope(glb),type(int)])),aexp(const(10,int)))),addr(10),addr(8))). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@7
at(8,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@7
at(9,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@7
at(10,inst,[],asgn(id(i,[scope(loc),type(int)]),aexp(const(0,int)),addr(11))). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@8
at(11,block,[],goto(addr(12))). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@8 (block)
at(12,inst,[],goto(addr(13))). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@8 (loop)
at(13,inst,['while_continue'],goto(addr(14))).
at(14,ifte,[],ite(bexp(lt(aexp(id(i,[scope(loc),type(int)])),aexp(mult(aexp(const(1000000,int)),aexp(id(k,[scope(glb),type(int)])))))),addr(16),addr(15))). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@8
at(15,goto,[],goto(addr(17))). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@8 (goto)
at(16,inst,[],asgn(id(i,[scope(loc),type(int)]),aexp(plus(aexp(id(i,[scope(loc),type(int)])),aexp(id(k,[scope(glb),type(int)])))),addr(13))). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@8
at(17,inst,['while_break'],goto(addr(18))).
at(18,inst,[],call(map__VERIFIER_assert,[bexp(eq(aexp(id(i,[scope(loc),type(int)])),aexp(mult(aexp(const(1000000,int)),aexp(id(k,[scope(glb),type(int)]))))))],none,addr(19))). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@9
at(19,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@10
% __VERIFIER_nondet_int
at(20,ret,[],ret(map__VERIFIER_nondet_int,aexp(id(val,[scope(loc),type(int)])))). %utils.c@3
% __VERIFIER_nondet_long
at(21,ret,[],ret(map__VERIFIER_nondet_long,aexp(id(val,[scope(loc),type(long)])))). %utils.c@8
% __VERIFIER_nondet_uint
at(22,ret,[],ret(map__VERIFIER_nondet_uint,aexp(id(val,[scope(loc),type(uint)])))). %utils.c@13
% __VERIFIER_nondet_bool
at(23,ret,[],ret(map__VERIFIER_nondet_bool,aexp(id(val,[scope(loc),type(bool)])))). %utils.c@18
% __VERIFIER_assume
at(24,ifte,[],ite(bexp(not(aexp(id(expression,[scope(loc),type(int)])))),addr(25),addr(26))). %utils.c@22
at(25,goto,['LOOP'],goto(addr(25))). %utils.c@22 (goto)
at(26,ret,[],ret(map__VERIFIER_assume,none)). %utils.c@23
% error
at(27,goto,['ERROR'],goto(addr(27))). %utils.c@35 (goto)
% errorFn
at(28,goto,['ERROR'],goto(addr(28))). %utils.c@40 (goto)
% __VERIFIER_error
at(29,goto,['ERROR'],goto(addr(29))). %utils.c@45 (goto)
% global variables
gvars([
  id(k,[type(int)]) %/media/sf_share_vm//loop-new/count_by_k_true-unreach-call_true-termination.c@3
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
succs(6,[9,7]).
preds(7,[6]).
succs(7,[8,10]).
preds(8,[7]).
succs(8,[]).
preds(9,[6]).
succs(9,[]).
preds(10,[7]).
succs(10,[11]).
preds(11,[10]).
succs(11,[12]).
preds(12,[11]).
succs(12,[13]).
preds(13,[16,12]).
succs(13,[14]).
preds(14,[13]).
succs(14,[15,16]).
preds(15,[14]).
succs(15,[17]).
preds(16,[14]).
succs(16,[13]).
preds(17,[15]).
succs(17,[18]).
preds(18,[17]).
succs(18,[19]).
preds(19,[18]).
succs(19,[]).
preds(20,[]).
succs(20,[]).
preds(21,[]).
succs(21,[]).
preds(22,[]).
succs(22,[]).
preds(23,[]).
succs(23,[]).
preds(24,[]).
succs(24,[26,25]).
preds(25,[25,24]).
succs(25,[25]).
preds(26,[24]).
succs(26,[]).
preds(27,[27]).
succs(27,[27]).
preds(28,[28]).
succs(28,[28]).
preds(29,[29]).
succs(29,[29]).
:- assert(data_types([bool,uint,long,int])).
