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

int in_require=0;

void MC_doit(expptr e){
  pprint(load(append(preamble,append(init_forms,cons(e,nil)))),stdout,rep_column);
  send_emacs_tag(in_require ? ignore_tag : result_tag);
  fflush(stdout);
}

char *strip_quotes(char *input){
  int len=strlen(input);

  if (input[0]=='"' && input[len-1]=='"'){
    char * buffer = (char *) stack_alloc(len-1);
    strncpy(buffer,input+1,len-2);
    buffer[len-2]=0;
    return buffer; 
  }
  else return input;
}

void eval_exp(expptr exp){
  MC_doit(macroexpand(exp));}

void IDE_loop(){
  while(1){
    catch_error({
	send_emacs_tag(ide_tag);
	in_doit = 0;
	preamble = nil;
	init_forms = nil;
        in_require=0;

        expptr e=read_from_ide();

	fprintf(stdout, "processing:\n");
	pprint(e,stdout,0);
        send_emacs_tag(print_tag);

        char *require_file;
        expptr exps_to_eval;
        ucase{e;
          {#require($sym)} : {
            if (!atomp(sym)) uerror(`{Require argument "$sym" must be a symbol});
            require_file=sformat("%s.mc",strip_quotes(atom_string(sym)));
            in_require=1;
            exps_to_eval=file_expressions(require_file);}
          {$any} : {
            exps_to_eval=cons(e,nil);}}

        mapc(eval_exp,exps_to_eval);

        if (in_require) {
          fprintf(stdout,"%s Provided ",require_file); 
          send_emacs_tag(result_tag);
          in_require=0;}
      })}
}

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcE_init1();
  mcE_init2();
  in_ide = 1;
  
  catch_error(insert_base())
  if(error_flg != 0)return error_flg;

  send_emacs_tag(ignore_tag);
  IDE_loop();
  return 0;
}
