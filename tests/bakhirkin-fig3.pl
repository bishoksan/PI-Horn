 /*
 taken from: Backward Analysis via Over-Approximate Abstraction
and Under-Approximate Subtraction (https://www.microsoft.com/en-us/research/wp-content/uploads/2014/06/2014-SAS-Backward-Analysis-via-Over-Approximate-Abstraction-and-Under-Approximate-Subtraction-TR.pdf)

// safe: x[1,99] => y>=1 /\ x=<0 => y<>0

while x >= 1 {
if x =< 99 {
    if y =< 0 {assert 0}
    if * {x =-1}
}
x=x+1
}
 assert y <> 0

 */
 


init(A,B).
while(A,B):- init(A,B).
while(A,B):-  inc_exit(A, B).
if1(A,B):- A>=1,while(A,B).
if1_exit(A,B):- if1(A,B), A>=100.
if2(A,B):- A=<99, if1(A,B).
if2_exit(A,B):- if2(A,B), B>=1.
false:- if2(A,B), B=<0.
if3(A,B):- if2_exit(A,B).
if3_exit(A1,B):- if3(A,B),A1= -1.
if3_exit(A,B):- if3(A,B).
inc(A,B):- if1_exit(A,B).
inc_exit(A1,B):- inc(A,B),A1=A+1.
false:- B=0, while(A,B), A=< 0.
safe:- B>=1, while(A,B), A=< 0.
safe:- B=< -1, while(A,B), A=< 0.
spec :- false. 
spec :- safe.
