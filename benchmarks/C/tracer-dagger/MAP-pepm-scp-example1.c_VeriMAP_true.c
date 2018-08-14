int n,k;
int main(){

	int i=0;
	//int k=0;
	//int n;

	__VERIFIER_assume(n>=0);

	while (i < n) {
	   i = i + 1;
	   k = k + 2;
	}

	while (i > 0) {
	   i = i - 1;
	   k = k - 1;
	}

	if(k < n)
       goto ERROR;

	return 0;
ERROR:
	return -1;
}
