/* Benchmark used to verify Chimdyalwar, Bharti, et al. "VeriAbs: Verification by abstraction (competition contribution)." 
International Conference on Tools and Algorithms for the Construction and Analysis of Systems. Springer, Berlin, Heidelberg, 2017.*/

#define SIZE 100000
extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern void __VERIFIER_assume(int);
void __VERIFIER_assert(int cond)
{
  if (!(cond)) {
    ERROR: __VERIFIER_error();
  }
  return;
}

unsigned int __VERIFIER_nondet_uint();

unsigned int index1 = 0;
unsigned int index2 = 0;

int main()
{
  unsigned int array[SIZE];
  unsigned int loop_entered = 0;

  index1 =  __VERIFIER_nondet_uint();
  __VERIFIER_assume(index1 < SIZE);
  index2 =  __VERIFIER_nondet_uint();
  __VERIFIER_assume(index2 < SIZE);
  
  while (index1 < index2) {
    __VERIFIER_assert((index1 < SIZE) && (index2 < SIZE));
    __VERIFIER_assume(array[index1] == array[index2]);
    index1++;
    index2--;
    loop_entered = 1;
  }

  if (loop_entered) {
    while (index2 < index1) {
      __VERIFIER_assert(array[index1] == array[index2]);
      index2++;
      index1--;
    }
  }
    
}
