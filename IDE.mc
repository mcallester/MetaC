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

int in_require=0;

void MC_doit(expptr e){

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

expptr simple_eval(expptr e){
  return load(append(preamble,append(init_forms,cons(e,nil))));
}

void simple_eval_noval(expptr e){
  load(append(preamble,append(init_forms,cons(e,nil))));
}

void eval_exp(expptr exp){
  preamble= nil;
  init_forms = nil;
  expptr e = macroexpand(exp);
  ucase{e;
    {#require($sym)}.(atomp(sym)) : {
      char * require_file=sformat("%s.mc",strip_quotes(atom_string(sym)));
      mapc(simple_eval_noval,file_expressions(require_file));
      fprintf(stdout,"%s Provided ",require_file); }
    {$any} : {pprint(simple_eval(e),stdout,0);}}
}

void IDE_loop(){
  while(1){
    push_memory_frame();

    catch_error({
	send_emacs_tag(request_input_tag);
        expptr e=read_from_ide();

	fprintf(stdout, "processing:\n");
	pprint(e,stdout,0);
        send_emacs_tag(print_tag);

	eval_exp(e);
	send_emacs_tag(result_tag);
      });
      
    pop_memory_frame();
  }
}

int main(int argc, char **argv){
  if(argc != 2){fprintf(stdout,"wrong number of arguments to NIDE"); return 1;}
  MetaC_directory = argv[1];
    
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
