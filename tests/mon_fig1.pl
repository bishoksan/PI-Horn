/* Combining Forward and Backward Abstract
Interpretation of Horn Clauses, by Alexey Bakhirkin and David Monniaux, SAS-17 */

%init(A,B) :- A = 0 , B = 0.
init(A,B).
p(A, B) :- init(A,B).
p(A , B) :- C >= 0, A=C+1, B=D+1, p(C, D).
p(A , B) :- C =< -1, A=C+1, p(C, B).
false :- A>=B+1, p(A, B).
false :- A=<B-1, p(A, B).
safe :- A=B, p(A, B).
spec :- false. 
spec :- safe.