int y;
int main(){
//int y;
int x=0;

  //  __VERIFIER_assume(y>=0);

	while ( x < 10000) {
		y = y + 1;
		x = x + 1;
	}

	if( y + x < 10000)		
		goto ERROR;

	return 0;
ERROR:
	return -1;
}
