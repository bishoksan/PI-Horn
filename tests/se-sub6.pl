/*
Provided by Harald: subject6.txt
public void twoLoops(int k)
{
    int x = 0;
    int y = 0;
    int z = 0;
    
    
    while (x <= k)
    {
        x = x++;
        y = y++;
        z = z - 2;
    }
    
    while (x >= k)
    {
        x = x--;
        y = y - 3;
        z = z + 2;
        
    }
    
    PexAssert.IsTrue((x <= 0) && (z >= 0) && (y <= 0));
    // abort();
    
}

*/


init(K).
while1_entry(K,X,Y,Z):-
     X=0, Y=0, Z=0, init(K).
while1(K,X,Y,Z):-
    while1_entry(K,X,Y,Z), X=<K.
while1(K,X1,Y1,Z1):-
    while1(K,X,Y,Z), X1=X+1, Y1=Y+1, Z1=Z-2.
while1_exit(K,X,Y,Z):-
    while1(K,X,Y,Z), X>=K+1.

while2_entry(K,X,Y,Z):- while1_exit(K,X,Y,Z).
while2(K,X,Y,Z):-
    while2_entry(K,X,Y,Z), X>=K.
while2(K,X1,Y1,Z1):-
    while2(K,X,Y,Z), X1=X-1, Y1=Y-1, Z1=Z+2.
while2_exit(K,X,Y,Z):-
    while2(K,X,Y,Z), X=<K-1.

false:- while2_exit(K,X,Y,Z), X>= 1.
false:- while2_exit(K,X,Y,Z), Z=< -1.
false:- while2_exit(K,X,Y,Z), Y>= 1.
safe:- while2_exit(K,X,Y,Z), X=<0, Z>=0, Y=<0.





spec :- false. 
spec :- safe.