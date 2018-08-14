int n, y;
int main(){

	int x=0;
	//int y=0;
	//int n;

	//__VERIFIER_assume(n>=0);

	while (x < n) {
	   x = x + 1;
	   y = y + 1;
	}

	while (x > 0) {
	   x = x - 1;
	   y = y - 1;
	}

	if(y > x)
       goto ERROR;

	return 0;
ERROR:
	return -1;
}
