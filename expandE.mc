#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <setjmp.h>
#include <time.h>
#include <dlfcn.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <fcntl.h>
#include <string.h>
#include "mc.h"

int main(int argc, char **argv){
  if(argc != 3){fprintf(stdout,"wrong number of arguments to expandE"); return -1;}
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcD2_init();
  expandE_init(); //defines install_value_proerties macro
  install_base();
  in_expand = 1;
  catch_all{mcexpand(argv[1],argv[2]);}{return -1;};
}
