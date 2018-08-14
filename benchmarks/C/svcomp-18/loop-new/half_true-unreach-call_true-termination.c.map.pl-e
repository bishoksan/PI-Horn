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
 [id(i,[type(int)]),id(tmp___0,[type(int)])],
 addr(6)
). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@3
fun(
 map__VERIFIER_nondet_int,
 [],
 [id(val,[type(int)])],
 addr(28)
). %utils.c@1
fun(
 map__VERIFIER_nondet_long,
 [],
 [id(val,[type(long)])],
 addr(29)
). %utils.c@6
fun(
 map__VERIFIER_nondet_uint,
 [],
 [id(val,[type(uint)])],
 addr(30)
). %utils.c@11
fun(
 map__VERIFIER_nondet_bool,
 [],
 [id(val,[type(bool)])],
 addr(31)
). %utils.c@16
fun(
 map__VERIFIER_assume,
 [id(expression,[type(int)])],
 [],
 addr(32)
). %utils.c@21
fun(
 error,
 [],
 [],
 addr(35)
). %utils.c@33
fun(
 errorFn,
 [],
 [],
 addr(36)
). %utils.c@38
fun(
 map__VERIFIER_error,
 [],
 [],
 addr(37)
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
at(6,inst,[],asgn(id(i,[scope(loc),type(int)]),aexp(const(0,int)),addr(7))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@4
at(7,ifte,[],ite(bexp(lte(aexp(id(k,[scope(glb),type(int)])),aexp(const(1000000,int)))),addr(8),addr(10))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@6
at(8,ifte,[],ite(bexp(gte(aexp(id(k,[scope(glb),type(int)])),aexp(const(-1000000,int)))),addr(11),addr(9))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@6
at(9,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@6
at(10,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@6
at(11,inst,[],asgn(id(i,[scope(loc),type(int)]),aexp(const(0,int)),addr(12))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@7
at(12,block,[],goto(addr(13))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@7 (block)
at(13,inst,[],goto(addr(14))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@7 (loop)
at(14,inst,['while_continue'],goto(addr(15))).
at(15,ifte,[],ite(bexp(lt(aexp(id(i,[scope(loc),type(int)])),aexp(mult(aexp(const(2,int)),aexp(id(k,[scope(glb),type(int)])))))),addr(17),addr(16))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@7
at(16,goto,[],goto(addr(20))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@7 (goto)
at(17,ifte,[],ite(bexp(eq(aexp(mod(aexp(id(i,[scope(loc),type(int)])),aexp(const(2,int)))),aexp(const(0,int)))),addr(18),addr(19))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@8
at(18,inst,[],asgn(id(n,[scope(glb),type(int)]),aexp(plus(aexp(id(n,[scope(glb),type(int)])),aexp(const(1,int)))),addr(19))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@9
at(19,inst,[],asgn(id(i,[scope(loc),type(int)]),aexp(plus(aexp(id(i,[scope(loc),type(int)])),aexp(const(1,int)))),addr(14))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@7
at(20,inst,['while_break'],goto(addr(21))).
at(21,ifte,[],ite(bexp(lt(aexp(id(k,[scope(glb),type(int)])),aexp(const(0,int)))),addr(22),addr(23))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@12
at(22,inst,[],asgn(id(tmp___0,[scope(loc),type(int)]),aexp(const(1,int)),addr(26))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@12
at(23,ifte,[],ite(bexp(eq(aexp(id(n,[scope(glb),type(int)])),aexp(id(k,[scope(glb),type(int)])))),addr(24),addr(25))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@12
at(24,inst,[],asgn(id(tmp___0,[scope(loc),type(int)]),aexp(const(1,int)),addr(26))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@12
at(25,inst,[],asgn(id(tmp___0,[scope(loc),type(int)]),aexp(const(0,int)),addr(26))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@12
at(26,inst,[],call(map__VERIFIER_assert,[aexp(id(tmp___0,[scope(loc),type(int)]))],none,addr(27))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@12
at(27,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@13
% __VERIFIER_nondet_int
at(28,ret,[],ret(map__VERIFIER_nondet_int,aexp(id(val,[scope(loc),type(int)])))). %utils.c@3
% __VERIFIER_nondet_long
at(29,ret,[],ret(map__VERIFIER_nondet_long,aexp(id(val,[scope(loc),type(long)])))). %utils.c@8
% __VERIFIER_nondet_uint
at(30,ret,[],ret(map__VERIFIER_nondet_uint,aexp(id(val,[scope(loc),type(uint)])))). %utils.c@13
% __VERIFIER_nondet_bool
at(31,ret,[],ret(map__VERIFIER_nondet_bool,aexp(id(val,[scope(loc),type(bool)])))). %utils.c@18
% __VERIFIER_assume
at(32,ifte,[],ite(bexp(not(aexp(id(expression,[scope(loc),type(int)])))),addr(33),addr(34))). %utils.c@22
at(33,goto,['LOOP'],goto(addr(33))). %utils.c@22 (goto)
at(34,ret,[],ret(map__VERIFIER_assume,none)). %utils.c@23
% error
at(35,goto,['ERROR'],goto(addr(35))). %utils.c@35 (goto)
% errorFn
at(36,goto,['ERROR'],goto(addr(36))). %utils.c@40 (goto)
% __VERIFIER_error
at(37,goto,['ERROR'],goto(addr(37))). %utils.c@45 (goto)
% global variables
gvars([
  id(k,[type(int)]), %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@2
  id(n,[type(int),init(aexp(const(0,int)))]) %/media/sf_share_vm//loop-new/half_true-unreach-call_true-termination.c@2
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
succs(7,[10,8]).
preds(8,[7]).
succs(8,[9,11]).
preds(9,[8]).
succs(9,[]).
preds(10,[7]).
succs(10,[]).
preds(11,[8]).
succs(11,[12]).
preds(12,[11]).
succs(12,[13]).
preds(13,[12]).
succs(13,[14]).
preds(14,[19,13]).
succs(14,[15]).
preds(15,[14]).
succs(15,[16,17]).
preds(16,[15]).
succs(16,[20]).
preds(17,[15]).
succs(17,[19,18]).
preds(18,[17]).
succs(18,[19]).
preds(19,[17,18]).
succs(19,[14]).
preds(20,[16]).
succs(20,[21]).
preds(21,[20]).
succs(21,[23,22]).
preds(22,[21]).
succs(22,[26]).
preds(23,[21]).
succs(23,[25,24]).
preds(24,[23]).
succs(24,[26]).
preds(25,[23]).
succs(25,[26]).
preds(26,[25,24,22]).
succs(26,[27]).
preds(27,[26]).
succs(27,[]).
preds(28,[]).
succs(28,[]).
preds(29,[]).
succs(29,[]).
preds(30,[]).
succs(30,[]).
preds(31,[]).
succs(31,[]).
preds(32,[]).
succs(32,[34,33]).
preds(33,[33,32]).
succs(33,[33]).
preds(34,[32]).
succs(34,[]).
preds(35,[35]).
succs(35,[35]).
preds(36,[36]).
succs(36,[36]).
preds(37,[37]).
succs(37,[37]).
:- assert(data_types([bool,uint,long,int])).
