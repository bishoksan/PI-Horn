/*
Provided by Harald: subject2.txt
internal  int  inputDependentLoopExample2(

int n,
int m
)
{
    int k = 0;
    int i = 0;
    int j = 0;
    int limit = 5000;
    
    
    while (i < n)
    {
        j = 0;
        while (j < m)
        {
            k = k + 5;
            j = j + 1;
        }
        i = i + 1;
    }
    
    
    if (k == limit) // loop dependent branch
        abort();
    return k;
}

*/


init(N,M).
while_out_entry(N,M, K,I,J, Limit):-
    init(N,M), K=0, I=0, J=0, Limit=5000.
while_out(N,M, K,I,J, Limit):-
    while_out_entry(N,M, K,I,J, Limit), I=< N-1.
while_out(N,M, K,I1,J, Limit):-
    while_in_exit(N,M, K,I,J, Limit), I1=I+1.
while_out_exit(N,M, K,I,J, Limit):-
    while_out(N,M, K,I,J, Limit), I>=N.
while_in_entry(N,M, K,I,J, Limit):-
    while_out(N,M, K,I,J, Limit), J=0.
while_in(N,M, K,I,J, Limit):-
    while_in_entry(N,M, K,I,J, Limit), J=<M-1.
while_in(N,M, K1,I,J1, Limit):-
    while_in(N,M, K,I,J, Limit), K1=K+5, J1=J+1.
while_in_exit(N,M, K,I,J, Limit):-
     while_in(N,M, K,I,J, Limit), J>=M.
false:-
    while_out(N,M, K,I,J, Limit), K=Limit.
safe:-
    while_out(N,M, K,I,J, Limit), K>Limit.
safe:-
    while_out(N,M, K,I,J, Limit), K<Limit.





spec :- false. 
spec :- safe.