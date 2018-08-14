extern void __VERIFIER_error() __attribute__ ((__noreturn__));

void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: __VERIFIER_error();
  }
  return;
}
unsigned int x = 10;

int main(void) {
  

  while (x >= 10) {
    x += 2;
  }

  __VERIFIER_assert(!(x % 2));
}
