// int x = 0;
//int y = 0;
int n;
int k;

int incr(int int z)
{ 
  return k + z;
}

void main() {
    int x, y;
  while (x < n) {
    x = x + 1; 
    y=incr(x);
  }

  __VERIFIER_assert( x<=y );

}
