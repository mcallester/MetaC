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

expptr load(expptr forms);

void IDE_pprint(expptr e, FILE * f, int level);

void MC_doit(expptr e){
  pprint(load(append(preamble,append(init_forms,cons(e,nil)))),stdout,rep_column);
  fprintf(stdout,"}result}");
  fflush(stdout);
}

void IDE_loop(){
  while(1){
    catch_error({
	fprintf(stdout, "}IDE}");
	preamble = nil;
	init_forms = nil;
	expptr e = read_from_ide();
	MC_doit(macroexpand(e));  })
      }
}

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcE_init1();
  mcE_init2();
  in_ide = 1;
  
  catch_error(insert_base())
  if(error_flg != 0)return error_flg;

  IDE_loop();
  return 0;
}
