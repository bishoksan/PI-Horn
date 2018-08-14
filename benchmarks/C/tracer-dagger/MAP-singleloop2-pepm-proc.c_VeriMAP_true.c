int n;

int main() {
int x=0;
int y=0;
//int n;

  //__VERIFIER_assume(n>=1);

	while(x < 2*n){
	   x = x + 1;

	   if ( x > n )
		  y = y - 1;
	   else
		  y = y + 2;
	}

	if(x != y)
		goto ERROR;

	return 0;
ERROR:
	return -1;
}
