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
  if(argc != 4){fprintf(stdout,"wrong number of arguments to expandE"); return 1;}
  MetaC_directory = argv[1];
  catch_error(
	      mcA_init();
	      mcB_init();
	      mcC_init();
	      mcD_init();
	      mcE_init1();
	      mcE_init2();
	      in_expand = 1;
	      mcexpand(argv[2], argv[3]));
  return error_flg;
}
