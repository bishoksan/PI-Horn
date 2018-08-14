%sufficient pre-condition to produce the error 0=<J<=5.

/*


integer j = [0;10];
integer i = 0;

void main()
{
    while (i < 100) {
        i++;
        j = j + [0;1];
    }
    assert(j <= 105);
}

*/

%init(I,J).
init(I,J):-I=0, 0=<J, J=<10.

while(I,J):- init(I,J).
while(I1,J1):- I=<99, I1=I+1, J1=J, while(I,J).
while(I1,J1):- I=<99, I1=I+1, J1=J+1, while(I,J).


while_exit(I,J):-while(I,J), I>=100.

false:- J>=106, while_exit(I,J).
safe:- J=<105, while_exit(I,J).
spec :- false. 
spec :- safe.