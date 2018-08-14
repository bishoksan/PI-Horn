int x=0;
int y=0;

int main(){

	//int x=0;
	//int y=0;

	while (__VERIFIER_nondet_int()) {
		if (__VERIFIER_nondet_int()) {
			x = x+1; 
			y = y+2;
		} else if (__VERIFIER_nondet_int()) {
			if (x >= 4) {
			    x = x+1; 
			    y = y+3; 
			}
		} 
	}

    if(3*x < y)
		goto ERROR;
	
	return 0;
ERROR:
	return -1;
}
