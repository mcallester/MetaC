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

voidptr symbol_value[STRING_DIM];

expptr eval_exp(expptr);

void IDE_loop(){
  
  send_emacs_tag(running_tag);
  
  while(1){
    push_memory_frame(); //stack memory
    
    catch_all{
      catch{NIDE()}{
	expptr e=read_from_ide();
	fprintf(stdout, "processing:\n");
	pprint(e,stdout,0);
	send_emacs_tag(print_tag);
	
	expptr result = eval_exp(e);
	pprint(result,stdout,0);
	send_emacs_tag(result_tag);
	}{}
      }{
      send_emacs_tag(uncaught_throw_tag);};
    
    pop_memory_frame();}
  }


int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcE_init1();
  mcE_init2();
  in_ide = 1;
  insert_base();
  IDE_loop();
}
