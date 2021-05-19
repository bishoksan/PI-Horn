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
 [id(i,[type(int)]),id(j,[type(int)])],
 addr(6)
). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@3
fun(
 map__VERIFIER_nondet_int,
 [],
 [id(val,[type(int)])],
 addr(34)
). %utils.c@1
fun(
 map__VERIFIER_nondet_long,
 [],
 [id(val,[type(long)])],
 addr(35)
). %utils.c@6
fun(
 map__VERIFIER_nondet_uint,
 [],
 [id(val,[type(uint)])],
 addr(36)
). %utils.c@11
fun(
 map__VERIFIER_nondet_bool,
 [],
 [id(val,[type(bool)])],
 addr(37)
). %utils.c@16
fun(
 map__VERIFIER_assume,
 [id(expression,[type(int)])],
 [],
 addr(38)
). %utils.c@21
fun(
 error,
 [],
 [],
 addr(41)
). %utils.c@33
fun(
 errorFn,
 [],
 [],
 addr(42)
). %utils.c@38
fun(
 map__VERIFIER_error,
 [],
 [],
 addr(43)
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
at(6,ifte,[],ite(bexp(lte(aexp(const(10,int)),aexp(id(n,[scope(glb),type(int)])))),addr(7),addr(9))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@7
at(7,ifte,[],ite(bexp(lte(aexp(id(n,[scope(glb),type(int)])),aexp(const(10000,int)))),addr(10),addr(8))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@7
at(8,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@7
at(9,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@7
at(10,ifte,[],ite(bexp(lte(aexp(const(10,int)),aexp(id(m,[scope(glb),type(int)])))),addr(11),addr(13))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@8
at(11,ifte,[],ite(bexp(lte(aexp(id(m,[scope(glb),type(int)])),aexp(const(10000,int)))),addr(14),addr(12))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@8
at(12,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@8
at(13,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@8
at(14,inst,[],asgn(id(i,[scope(loc),type(int)]),aexp(const(0,int)),addr(15))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@9
at(15,block,[],goto(addr(16))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@9 (block)
at(16,inst,[],goto(addr(17))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@9 (loop)
at(17,inst,['while_continue'],goto(addr(18))).
at(18,ifte,[],ite(bexp(lt(aexp(id(i,[scope(loc),type(int)])),aexp(id(n,[scope(glb),type(int)])))),addr(20),addr(19))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@9
at(19,goto,[],goto(addr(31))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@9 (goto)
at(20,inst,[],asgn(id(j,[scope(loc),type(int)]),aexp(const(0,int)),addr(21))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@10
at(21,block,[],goto(addr(22))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@10 (block)
at(22,inst,[],goto(addr(23))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@10 (loop)
at(23,inst,['while_continue___0'],goto(addr(24))).
at(24,ifte,[],ite(bexp(lt(aexp(id(j,[scope(loc),type(int)])),aexp(id(m,[scope(glb),type(int)])))),addr(26),addr(25))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@10
at(25,goto,[],goto(addr(29))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@10 (goto)
at(26,block,[],goto(addr(27))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@11 (block)
at(27,inst,[],asgn(id(k,[scope(glb),type(int)]),aexp(plus(aexp(id(k,[scope(glb),type(int)])),aexp(const(1,int)))),addr(28))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@11
at(28,inst,[],asgn(id(j,[scope(loc),type(int)]),aexp(plus(aexp(id(j,[scope(loc),type(int)])),aexp(const(1,int)))),addr(23))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@10
at(29,inst,['while_break___0'],goto(addr(30))).
at(30,inst,[],asgn(id(i,[scope(loc),type(int)]),aexp(plus(aexp(id(i,[scope(loc),type(int)])),aexp(const(1,int)))),addr(17))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@9
at(31,inst,['while_break'],goto(addr(32))).
at(32,inst,[],call(map__VERIFIER_assert,[bexp(gte(aexp(id(k,[scope(glb),type(int)])),aexp(const(100,int))))],none,addr(33))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@14
at(33,ret,[],ret(main,aexp(const(0,int)))). %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@15
% __VERIFIER_nondet_int
at(34,ret,[],ret(map__VERIFIER_nondet_int,aexp(id(val,[scope(loc),type(int)])))). %utils.c@3
% __VERIFIER_nondet_long
at(35,ret,[],ret(map__VERIFIER_nondet_long,aexp(id(val,[scope(loc),type(long)])))). %utils.c@8
% __VERIFIER_nondet_uint
at(36,ret,[],ret(map__VERIFIER_nondet_uint,aexp(id(val,[scope(loc),type(uint)])))). %utils.c@13
% __VERIFIER_nondet_bool
at(37,ret,[],ret(map__VERIFIER_nondet_bool,aexp(id(val,[scope(loc),type(bool)])))). %utils.c@18
% __VERIFIER_assume
at(38,ifte,[],ite(bexp(not(aexp(id(expression,[scope(loc),type(int)])))),addr(39),addr(40))). %utils.c@22
at(39,goto,['LOOP'],goto(addr(39))). %utils.c@22 (goto)
at(40,ret,[],ret(map__VERIFIER_assume,none)). %utils.c@23
% error
at(41,goto,['ERROR'],goto(addr(41))). %utils.c@35 (goto)
% errorFn
at(42,goto,['ERROR'],goto(addr(42))). %utils.c@40 (goto)
% __VERIFIER_error
at(43,goto,['ERROR'],goto(addr(43))). %utils.c@45 (goto)
% global variables
gvars([
  id(k,[type(int)]), %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@2
  id(n,[type(int)]), %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@2
  id(m,[type(int)]) %/media/sf_share_vm//loop-new/nested_true-unreach-call_true-termination.c@2
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
succs(10,[13,11]).
preds(11,[10]).
succs(11,[12,14]).
preds(12,[11]).
succs(12,[]).
preds(13,[10]).
succs(13,[]).
preds(14,[11]).
succs(14,[15]).
preds(15,[14]).
succs(15,[16]).
preds(16,[15]).
succs(16,[17]).
preds(17,[30,16]).
succs(17,[18]).
preds(18,[17]).
succs(18,[19,20]).
preds(19,[18]).
succs(19,[31]).
preds(20,[18]).
succs(20,[21]).
preds(21,[20]).
succs(21,[22]).
preds(22,[21]).
succs(22,[23]).
preds(23,[28,22]).
succs(23,[24]).
preds(24,[23]).
succs(24,[25,26]).
preds(25,[24]).
succs(25,[29]).
preds(26,[24]).
succs(26,[27]).
preds(27,[26]).
succs(27,[28]).
preds(28,[27]).
succs(28,[23]).
preds(29,[25]).
succs(29,[30]).
preds(30,[29]).
succs(30,[17]).
preds(31,[19]).
succs(31,[32]).
preds(32,[31]).
succs(32,[33]).
preds(33,[32]).
succs(33,[]).
preds(34,[]).
succs(34,[]).
preds(35,[]).
succs(35,[]).
preds(36,[]).
succs(36,[]).
preds(37,[]).
succs(37,[]).
preds(38,[]).
succs(38,[40,39]).
preds(39,[39,38]).
succs(39,[39]).
preds(40,[38]).
succs(40,[]).
preds(41,[41]).
succs(41,[41]).
preds(42,[42]).
succs(42,[42]).
preds(43,[43]).
succs(43,[43]).
:- assert(data_types([bool,uint,long,int])).
