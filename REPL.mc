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

void read_eval_print(){
  while(1){
    push_memory_frame();
    fprintf(stdout, "MC>");
    catch_all{
      fgets(ephemeral_buffer,EPHEMERAL_DIM,stdin);
      expptr e = mcread(ephemeral_buffer);
      expptr value = NULL;
      if(!e || e == nil)continue;
      ucase(e){
	{quit}:{break;};
	{$any}:{value = eval_exp(e);};}
      fprintf(stdout,"%s\n",exp_string(value));}
    {};
    pop_memory_frame();
    }
  }

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  expandE_init();
  install_value_properties();
  NIDE_init();
  in_ide =0;
  read_eval_print();
  return 0;
}
