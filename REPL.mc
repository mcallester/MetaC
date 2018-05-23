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

void load(expptr forms);
expptr eval(expptr statement);

//the following are used in the expansion of set_base_values()

expptr preamble;
expptr env_syms;
void preprocess(expptr);
int rep_column;

expptr strip_body(expptr decl){
  ucase{decl;
    {?type ?f(!args){!body}}:{return `{${type} ${f}(${args})};}
    {!x}:{return decl}}
  return NULL;}


void read_eval_print(){
  rep_column += 3;
  while(1){
    int i;
    writestrm = stdout;
    indent(rep_column);
    fprintf(stdout, "MC>");
    catch_error({
	expptr e = read_from_terminal();
	ucase{e;
	  {quit}:{if(rep_column == 0)break; else throw_error();}
	  {continue}:{if(rep_column != 0)break;}
	  {load(!forms)}:{load(forms);}
	  {describe(!sym)}:{
	    indent(rep_column);
	    pprint(strip_body(getprop(sym,`{declaration},NULL)),stdout,rep_column);}
	  {definition(!sym)}:{
	    indent(rep_column);
	    pprint(getprop(sym,`{declaration},NULL),stdout,rep_column);}}
      })
      }
  rep_column -=3;
}

void load_base(){
  //this emulates loading base_decls.h
  expptr forms = file_expressions(`{base_decls.h});
  mapc(install,forms);
  //the base forms have already been declared in mc.h and defined in various mc files
  //hence declarations, extractions and definitions are not needed.
  insert_base_values();  //This is a macro defined in mcE.  It initializes the symbol_values array.
  // initialization statements are handled by the initialization procedures
  dolist(sym, env_syms){setprop(sym,`{new},`{false});};
}

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcE_init1();
  mcE_init2();

  catch_error(load_base());
  if(error_flg != 0)return error_flg;
  
  rep_column = -3;
  read_eval_print();
  return 0;
}
