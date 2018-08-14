/* Combining Forward and Backward Abstract
Interpretation of Horn Clauses, by Alexey Bakhirkin and David Monniaux, SAS-17 */

%init(A, B) :- A = 0 , B = 0.
init(A, B).
p(A, B) :- init(A,B).
p(A1, B1) :- f(A, B, A1, B1), p(A, B).
false :- A >= B+1, p(A, B).
false :- A +1=< B, p(A, B).
safe :- A = B, p(A, B).
f(A, B, A + 1, B + 1) :- A >= 0, fc(A, B).
f(A, B, A + 1, B) :- A < 0, fc(A, B).
fc(A, B).
spec :- false. 
spec :- safe.