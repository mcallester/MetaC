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

expptr simple_eval(explist exps);

void REPL_loop(){
  
  while(1){
    push_memory_frame(); //stack memory
    fprintf(stdout, "MC>");    
    
    catch_all{
      catch(NIDE()){
	char* s = fgets(ephemeral_buffer,EPHEMERAL_DIM,stdin);
	expptr e = mcread(s);
	if(e == `quit)break;
	expptr result = simple_eval(expcons(mcread(s),NULL));
	pprint(result,stdout);
	}{}
      }{
      fprintf(stdout,"\n uncought throw\n");}
    pop_memory_frame();}
  }

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcF_init();
  install_value_properties();
  NIDE_init();
  in_repl=1;
  REPL_loop();
  return 0;
}
