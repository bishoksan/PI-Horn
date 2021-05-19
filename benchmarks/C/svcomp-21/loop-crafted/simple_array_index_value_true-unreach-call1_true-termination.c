/* Benchmark used to verify Chimdyalwar, Bharti, et al. "VeriAbs: Verification by abstraction (competition contribution)." 
International Conference on Tools and Algorithms for the Construction and Analysis of Systems. Springer, Berlin, Heidelberg, 2017.*/

#define SIZE 10000
extern void __VERIFIER_error() __attribute__ ((__noreturn__));
void __VERIFIER_assert(int cond)
{
  if (!(cond)) {
    ERROR: __VERIFIER_error();
  }
  return;
}

unsigned int index;

int main()
{
  unsigned int array[SIZE];

  for (index = 0; index < SIZE; index++) {
    array[index] = (index % 2);
  }

  for (index = 0; index < SIZE; index++) {
    if (index % 2 == 0) {
      __VERIFIER_assert(array[index] == 0);
    } else {
      __VERIFIER_assert(array[index] != 0);
    }
  }

}



