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

#include "mc.h"

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  in_expand = 1;
  catch_error({mcexpand(argv[1], argv[2]);});
  return error_flg[0];
}
