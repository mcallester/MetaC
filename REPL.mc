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

void MC_doit(expptr e){
  fputc('\n',stdout);
  pprint(load(append(preamble,append(init_forms,cons(e,NULL)))),stdout,0);
}

int rep_column;

void read_eval_print(){
  rep_column += 3;
  while(1){
    int i;
    indent(rep_column);
    fprintf(stdout, "MC>");
    catch_error({
	preamble = NULL;
	init_forms = NULL;
	expptr e = macroexpand(read_from_terminal());
	ucase{e;
	  {quit}:{if(rep_column == 0)break; else throw_error();}
	  {continue}:{if(rep_column != 0)break;}
	  {describe(?sym)}:{
	    indent(rep_column);
	    pprint(getprop(sym,`{declaration},NULL),stdout,rep_column);}
	  {!s;}:{MC_doit(e);}
	  {?type ?f(!args){!body}}:{MC_doit(e);}
	  {{!s}}:{MC_doit(e);}
	  {!e}:{MC_doit(`{return ${e};})}
	}})
      }
  rep_column -=3;
}

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcE_init1();
  mcE_init2();
  rep_column = -3;
  
  catch_error(insert_base_values())
  if(error_flg != 0)return error_flg;

  read_eval_print();
  return 0;
}
