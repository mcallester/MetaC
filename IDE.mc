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
  if(!in_repl){fprintf(stdout,"}return}");}
}

int rep_column;

void indent(int column){
  for(int i = 0; i< column;i++)fputc(' ',stdout);
}

void read_eval_print(){
  rep_column += 3;
  while(1){
    catch_error({
	indent(rep_column);
	fprintf(stdout, "MC>");
	preamble = nil;
	init_forms = nil;
	expptr e = macroexpand(read_from_terminal());
	if(!e || e == nil)continue;
	ucase{e;
	  {REPL}:{in_repl = 1;}
	  {IDE}:{in_repl = 0;}
	  {quit}:{break;}
	  {continue}:{if(rep_column != 0)break;}
	  {describe($sym)}:{
	    indent(rep_column);
	    pprint(getprop(sym,`{declaration},NULL),stdout,rep_column);}
	  {$s;}:{MC_doit(e);}
	  {{$s}}:{MC_doit(e);}
	  {$type $f($args){$body}}:{MC_doit(e);}
	  {$e}:{MC_doit(`{return $e;});}
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
  in_repl = 0;
  
  catch_error(insert_base())
  if(error_flg != 0)return error_flg;

  read_eval_print();
  return 0;
}
