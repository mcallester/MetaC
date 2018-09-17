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

voidptr symbol_value[SYMBOL_DIM];

void eval_exp(expptr);

void read_eval_print(){
  while(1){
    push_memory_frame();
    fprintf(stdout, "MC>");
    in_doit = 0;
    catch_error({
	expptr e = read_from_repl();
	if(!e || e == nil)continue;
	ucase{e;
	  {quit}:{break;}
	  {$any}:{eval_exp(e);}
	}});
    pop_memory_frame();
  }
}

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcE_init1();
  mcE_init2();
  in_repl = 1;
  
  catch_error(insert_base())
  if(error_flg != 0)return error_flg;

  read_eval_print();
  return 0;
}
