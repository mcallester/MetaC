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

expptr load(expptr forms);

void MC_doit(expptr e){
  fputc('\n',stdout);
  pprint(load(append(preamble,append(init_forms,cons(e,nil)))),stdout,0);
}

void read_eval_print(){
  dolist(e,file_expressions("testREPLdata.mc")){
    if(e == nil)continue;
    fprintf(stdout, "\nMC>");
    pprint(e,stdout,0);
    preamble = nil;
    init_forms = nil;
    expptr e2 = macroexpand(e);
    ucase{e2;
      {$s;}:{MC_doit(e2);}
      {{$s}}:{MC_doit(e2);}
      {$type $f($args){$body}}:{MC_doit(e2);}
      {$e}:{MC_doit(`{return $e2;});}}}
}

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcE_init1();
  mcE_init2();
  rep_column = -3;
  
  catch_error(insert_base())
  if(error_flg[0] != 0)return error_flg[0];

  read_eval_print();
  return 0;
}
