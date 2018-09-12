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
  expptr res = load(append(preamble,append(init_forms,cons(e,nil))));
  pprint(res,stdout,rep_column);
}

void indent(int column){
  for(int i = 0; i< column;i++)fputc(' ',stdout);
}

void read_eval_print(){
  rep_column += 3;
  while(1){
    push_memory_frame();
    catch_error({
	indent(rep_column);
	fprintf(stdout, "MC>");
	preamble = nil;
	init_forms = nil;
	expptr e = macroexpand(read_from_repl());
	if(!e || e == nil)continue;
	ucase{e;
	  {quit}:{break;}
	  {continue}:{if(rep_column != 0)break;}
	  {$e}:{MC_doit(e);}
	}})
      pop_memory_frame();
      }
  rep_column -=3;
}

int main(int argc, char **argv){
  if(argc != 2){fprintf(stdout,"wrong number of arguments to MC"); return 1;}
  MetaC_directory = argv[1];
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcE_init1();
  mcE_init2();
  rep_column = -3;
  in_repl = 1;
  
  catch_error(insert_base())
  if(error_flg != 0)return error_flg;

  read_eval_print();
  return 0;
}
