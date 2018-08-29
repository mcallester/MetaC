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
  catch_error(
	      mcA_init();
	      mcB_init();
	      mcC_init();
	      mcD_init();
	      mcE_init1();
	      mcE_init2();
	      in_expand = 1;
	      mcexpand(argv[1], argv[2]));
  return error_flg;
}
