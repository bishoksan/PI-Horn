 /*
 taken from: Backward Analysis via Over-Approximate Abstraction
and Under-Approximate Subtraction (https://www.microsoft.com/en-us/research/wp-content/uploads/2014/06/2014-SAS-Backward-Analysis-via-Over-Approximate-Abstraction-and-Under-Approximate-Subtraction-TR.pdf)

// safe: x=[1;60] or x>=100

 while x >= 1{
	 if x = 60 { x :=50 }
 x :=x + 1
 if x = 100 { x:= 0 }
 }
 assert 0 //or false = means the assertion never reaches to this point
 */
 


init(A).
while(A):- 
	init(A).
while(A):-  
	if2_exit(A).
if1(A):- 
	A>=1,
	while(A).
inc(B):- 
	if1(A),
	A=60, 
	B=50.
inc(A):- 
	if1(A), 
	A>=61.
inc(A):- 
	if1(A), 
	A=<59.
if2(B):- 
	inc(A), 
	B=A+1.
if2_exit(B):- 
	if2(A), 
	A=100, 
	B=0.
if2_exit(A):- 
	if2(A), 
	A>=101.
if2_exit(A):- 
	if2(A), 
	A=<99.
false:- 
	A=<0,while(A).
safe:-
    0=1,while(A).
spec :- false. 
spec :- safe.