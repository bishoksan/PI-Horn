# 1 "MAP/SAFE-exbench/DAGGER-p3-ok.tmp.c"
# 1 "<command-line>"
# 1 "MAP/SAFE-exbench/DAGGER-p3-ok.tmp.c"
# 20 "MAP/SAFE-exbench/DAGGER-p3-ok.tmp.c"
int nondet(void)
{int i; return i;}

//void prescan(int delim , int pvpbsize)
int id, pvpbsize;
void main()
{
  register int c ;
  int delim;
  //int pvpbsize;
  unsigned int bslashmode ;
  unsigned int route_syntax ;
  int cmntcnt ;
  int anglecnt ;
  int state ;
  int errno ;
  unsigned int tmp ;
  //int id = 0;
  int nondetvar;

  {
  errno = 0;
  bslashmode = 0;
  route_syntax = 0;
  cmntcnt = 0;
  anglecnt = 0;
  state = 1;
  c = -1;
  while (1) {
    while (1) {
      if (c != -1) {
        if (! bslashmode) {
          if (id >= pvpbsize - 5) {
            tmp = nondet();
            if (tmp > 40) {
            }
            returnnull:
            return ;
          }
          __VERIFIER_assert( id >= 0 );
   __VERIFIER_assert( id < pvpbsize );
          id++;
        }
      }
      c = nondet();
      if (c == 0) {
        if (state == 2) {
          c = '\"';
        } else {
          if (cmntcnt > 0) {
            c = ')';
          } else {
            if (anglecnt > 0) {
              c = '>';
            } else {
              break;
            }
          }
        }
      } else {
        if (c == delim) {
          if (cmntcnt <= 0) {
            if (state != 2) {
              if (anglecnt <= 0) {
                break;
              }
              if (delim == 44) {
                if (! route_syntax) {
                  c = '>';
                }
              }
            }
          }
        }
      }
      if (id >= pvpbsize - 5) {
        return ;
      } else {
        __VERIFIER_assert( id >= 0 );
 __VERIFIER_assert( id < pvpbsize );
      }
      if (bslashmode) {
        bslashmode = 0;
        if (cmntcnt > 0) {
          c = -1;
          goto M__Cont;
        } else {
          if (c != 33) {
            goto M_L;
          } else {
            if (state == 2) {
              M_L:
              if (id >= pvpbsize - 5) {
                return ;
              } else {
                __VERIFIER_assert( id >= 0 ); __VERIFIER_assert( id < pvpbsize );
                id++;
              }
              goto M__Cont;
            }
          }
        }
      }
      if (c == 92) {
        bslashmode = 1;
      } else {
        if (! (state == 2)) {
          if (c == 40) {
     nondetvar = nondet();
            if (nondetvar) {
              cmntcnt ++;
              c = -1;
            } else {
              goto M_L___1;
            }
          } else {
            M_L___1:
            if (c == 41) {
     nondetvar = nondet();
              if (nondetvar) {
                if (cmntcnt <= 0) {
                  c = -1;
                } else {
                  cmntcnt --;
                }
              } else {
                goto M_L___0;
              }
            } else {
              M_L___0:
              if (cmntcnt > 0) {
                c = -1;
              } else {
                if (c == 60) {
                  anglecnt ++;
                  while (1) {
      nondetvar = nondet();
                    if (nondetvar) {
      nondetvar = nondet();
                      if (nondetvar) {
                        break;
                      }
                    } else {
                      break;
                    }
                  }
      nondetvar = nondet();
                  if (nondetvar) {
                    route_syntax = 1;
                  }
                } else {
                  if (c == 62) {
                    if (anglecnt <= 0) {
                      c = -1;
                    } else {
                      anglecnt --;
                    }
                    route_syntax = 0;
                  } else {
                    if (delim == 32) {
       nondetvar = nondet();
                      if (nondetvar) {
       nondetvar = nondet();
                        if (nondetvar) {
                          c = ' ';
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      if (c == -1) {
        goto M__Cont;
      }
      if (c == delim) {
        if (anglecnt <= 0) {
          if (state != 2) {
            break;
          }
        }
      }
      state = nondet();
      if (state == 5) {
     nondetvar = nondet();
        if (nondetvar) {
          if (nondetvar) {
          } else {
          }
        } else {
        }
      }
     nondetvar = nondet();
      if (nondetvar) {
        c = -1;
      }
     nondetvar = nondet();
      if (nondetvar) {
        break;
      }
      M__Cont: ;
    }
     nondetvar = nondet();
    if (nondetvar) {
      if (id >= pvpbsize - 5) {
        return ;
      } else {
        __VERIFIER_assert( id >= 0 );
 __VERIFIER_assert( id < pvpbsize );
        id++;
      }
     nondetvar = nondet();
      if (nondetvar) {
        goto returnnull;
      }
    }
    if (c != 0) {
      if (! (c != delim)) {
        if (! (anglecnt > 0)) {
          break;
        }
      }
    } else {
      break;
    }
  }
  return ;
}
}
