/*
int a,b;
if(a<=100){
    a=100-a;
}else{
    a=a-100; }
while(a>=1){
    a=a-1;
    b=b-2;}
assert(b!=0)
*/


% an example to be used in the paper
init(A,B):-true.
if(A1,B) :- init(A,B), A=<100, A1=100-A.
if(A1,B) :- init(A,B), A>=101, A1=A-100.
while(A,B):-   if(A,B).
while(A1,B1) :-  A>=1, A1=A-1, B1=B-2, while(A,B).
false:- A=<0,  while(A,B), B=0.
safe:- A=<0,  while(A,B), B>=1.
safe:- A=<0,  while(A,B), B=< -1.
spec :- false. 
spec :- safe.