# 1 "MAP/SAFE-exbench/DAGGER-p2-ok.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/DAGGER-p2-ok.tmp.c"
# 20 "MAP/SAFE-exbench/DAGGER-p2-ok.tmp.c"
int nondet(void){
int i;
int __BLAST_NONDET;
i = __BLAST_NONDET;
return i;
}

int id;
void mime7to8()
{
  int c1 ;
  int c2 ;
  int c3 ;
  int c4 ;
  int tmp___6 ;
  //int id;

  {
  if (tmp___6 == 0) {

    id = 0;

    while (1) {
      c1 = nondet();
      if (! (c1 != -1)) {
        break;
      }
      if (nondet()) {
        if (nondet()) {
          continue;
        }
      }
      c2 = nondet();
      while (1) {
        c2 = nondet();
        if (nondet()) {
          if (nondet()) {
            break;
          }
        } else {
          break;
        }
      }
      if (c2 == -1) {
        break;
      }
      c3 = nondet();
      while (1) {
        c3 = nondet();
        if (nondet()) {
          if (nondet()) {
            break;
          }
        } else {
          break;
        }
      }
      if (c3 == -1) {
        break;
      }
      c4 = nondet();
      while (1) {
        c4 = nondet();
        if (nondet()) {
          if (nondet()) {
            break;
          }
        } else {
          break;
        }
      }
      if (c4 >= -1) {
 if (c4 <= -1) {
        break;
 }
      }
      if (c1 >= 61) {
 if (c1 <= 61)
        continue;
      } else {
        if (c2 >= 61) {
 if (c2 <= 61)
          continue;
        }
      }
      if (c1 < 0) {
        c1 = -1;
      } else {
        if (c1 > 127) {
          c1 = -1;
        } else {
          c1 = nondet();
        }
      }
      if (c2 < 0) {
        c2 = -1;
      } else {
        if (c2 > 127) {
          c2 = -1;
        } else {
          c2 = nondet();
        }
      }
      if (id >= 1000) {
 __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
        id = 0;
      } else {
 __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
      }
      id++;
      if (nondet()) {
        goto MY_L;
      } else {
        if (id >= 1000) {
          MY_L:
          if (id > 1 || id < 1) {
            id--;
  __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
            if (nondet() ) {
              id++;
            } else {
              id--;
  __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
              if (nondet()) {
                id++;
              }
            }
          }

          id = 0;
        }
      }
      if (c3 >= 61) {
 if (c3 <= 61)
        continue;
      }

      if (c3 < 0) {
        c3 = -1;
      } else {
        if (c3 > 127) {
          c3 = -1;
        } else {
          c3 = nondet();
        }
      }
      if (id >= 1000) {
  __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
        id = 0;
      } else {
  __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
      }
      id++;
      if (nondet()) {
        goto MY_L___0;
      } else {
        if (id >= 1000) {
          MY_L___0:
          if (id > 1 || id < 1) {
            id--;
  __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
            if (nondet()) {
              id++;
            } else {
              id--;
  __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
              if (nondet()) {
                id++;
              }
            }
          }
  __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );

          id = 0;
        }
      }
      if (c4 >= 61) {
 if (c4 <= 61)
        continue;
      }

      if (c4 < 0) {
        c4 = -1;
      } else {
        if (c4 > 127) {
          c4 = -1;
        } else {
          c4 = nondet();
        }
      }
      if (id >= 1000) {
  __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
        id = 0;
      } else {
  __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
      }
      id++;
      if (nondet()) {
        goto MY_L___1;
      } else {
        if (id >= 1000) {
          MY_L___1:
          if (id > 1 || id < 1) {
            id--;
  __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
            if (nondet()) {
              id++;
            } else {
              id--;
  __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
              if (nondet() ) {
                id++;
              }
            }
          }

          id = 0;
        }
      }
    }
    if (id >= 0) {
      __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id <= 1000 );
    }
  } else {

  }
  return;
}
}

int main() {
mime7to8();
return 0;
}
