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

expptr load(expptr forms, expptr value);

expptr eval(expptr statement);

//the following are used in the expansion of set_base_values()

expptr preamble;
expptr env_syms;
void preprocess(expptr);
int rep_column;

void eval_statement(expptr s){
  load(append(preamble,append(init_forms,cons(s,NULL))),NULL);
  fprintf(stdout,"\ndone\n\n");
}

void eval_expression(expptr e){
  expptr val = load(append(preamble,init_forms),e);
  fputc('\n',stdout);
  pprint(val,stdout,rep_column);}

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
	  {!s;}:{eval_statement(e);}
	  {?type ?f(!args){!body}}:{eval_statement(e);}
	  {{!s}}:{eval_statement(e);}
	  {!e}:{eval_expression(e);}
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

  catch_error(insert_base_values())
  if(error_flg != 0)return error_flg;
  
  rep_column = -3;
  read_eval_print();
  return 0;
}
