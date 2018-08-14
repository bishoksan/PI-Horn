/*
Provided by Harald: subject4.txt
public void Subject4WithAbiInt(int x)
{
    int x0 = x;
    
    int c = 0, p = 0;
    
    while (true)
    {
        
        PexAssume.AreEqual(x0, x + c);
        
        if (x <= 0) break;
        
        if (c == 50)
        {
            
            PexAssume.IsTrue(c==50);
            PexAssume.IsTrue((x0==x+c));
            
            abort();
        }
        c = c + 1;
        p = p + c;
        x = x - 1;
        
    }
    
    if (c == 30)
    {
        PexAssume.IsTrue(c == 30);
        PexAssume.IsTrue((x0 == x + c));
        abort();
    }
}

*/


init(X).
while_entry(X,X0, C,P):-
    X0=X, C=0, P=0, init(X).
while(X,X0, C,P):-
    while_entry(X,X0, C,P).
while_exit(X,X0, C,P):- %due to break statement
    while(X,X0, C,P), X=<0.

if2_entry(X,X0, C,P):-
    while(X,X0, C,P), X>= -1.
false:- if2_entry(X,X0, C,P), C=50.

if2_exit(X,X0, C,P):- if2_entry(X,X0, C,P), C>=51.
if2_exit(X,X0, C,P):- if2_entry(X,X0, C,P), C=<49.


while(X1,X0, C1,P1):- if2_exit(X,X0, C,P), C1=C+1, P1=P+C, X1=X-1.
false:- while_exit(X,X0, C,P), C=30, X0=X+C.
safe:- while_exit(X,X0, C,P), C>30.
safe:- while_exit(X,X0, C,P), C<30.




spec :- false. 
spec :- safe.