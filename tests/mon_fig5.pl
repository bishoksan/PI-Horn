/* Combining Forward and Backward Abstract
Interpretation of Horn Clauses, by Alexey Bakhirkin and David Monniaux, SAS-17 */
/*
a = 0; b = *;
 while (*)
 a += b ;
 if ( a > 0) {
     while (*)
         b += a ;
     assert ( b ≥ 0) ;
     }

*/

%init(A, B) :- A = 0.

init(A, B).
w_out(A, B):- init(A,B).
w_out(A,B):- if_exit(A,B).

w_out_exit(A, B):- init(A,B).

assign_first(A1,B):-w_out(A,B), A1=A+B.
if_exit(A,B):- assign_first(A,B), A=<0.
if_exit(A,B):- w_in_exit(A,B).

w_in(A,B):-assign_first(A,B), A>=1.
w_in(A,B1):- B1=A+B,w_in(A,B).
w_in_exit(A,B):-assign_first(A,B), A>=0.
false:- B=< -1, w_in_exit(A,B).
safe:- B>= 0, w_in_exit(A,B).


spec :- false. 
spec :- safe.