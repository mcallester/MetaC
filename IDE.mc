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

void IDE_loop(){
  while(1){
    push_memory_frame();
    catch_error({
        expptr e=read_from_ide();
	fprintf(stdout, "processing:\n");
	pprint(e,stdout,0);
        send_emacs_tag(print_tag);
	breakpt("IDE break");
	
	eval_exp(e);
	send_emacs_tag(result_tag);
      });
      
    pop_memory_frame();
  }
}

int main(int argc, char **argv){
  in_ide = 1;
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcE_init1();
  mcE_init2();
  
  catch_error(insert_base())
  if(error_flg[0] != 0)return error_flg[0];

  send_emacs_tag(ignore_tag); //blocks unwanted output from gdb initialization 'when the start-up prints.
  
  IDE_loop();
  return 0;
}
