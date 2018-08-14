/*

while(a>=1){
    a=a-1;
    b=b-2;}
if(b<0){error}
*/


% example used in ICLP paper
/*
while(A,B):-   init(A,B)=init(A,B).
while(A1,B1) :-  A>=1, A1=A-1, B1=B-1, while(A,B).
false:- A=<0,  while(A,B), B<0. %unsafe
%false:- A=<0,  while(A,B), B>=0.%safe

in 0 iterations we get
approx safe= B>=0,A=<0; A>=1,B>=A
approx unsafe= B<0,A=<0; A>=1,A>B
*/

init(A,B).
while(A,B):-   init(A,B).
while(A1,B1) :-  A>=1, A1=A-1, B1=B-1, while(A,B).
false:- A=<0,  while(A,B), B<0. %unsafe
safe:- A=<0,  while(A,B), B>=0.%safe
%false:- A=<0,  while(A,B), B>=0.%safe
spec :- false. 
spec :- safe.