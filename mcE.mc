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

expptr file_preamble;
expptr env_syms;
expptr statements;
int compilecount;
int symbol_count;

int newp(expptr sym);
int installedp(expptr form);
expptr args_variables(expptr args);
expptr args_types(expptr args);
void install(expptr decl);
voidptr compile_load_file(charptr fstring);
void install_preamble(expptr);
void install_pointer(expptr,expptr,expptr);
void install_var(expptr,expptr,expptr);
void install_proc(expptr,expptr,expptr,expptr);
void install_decl(expptr,expptr);
int symbol_index(expptr);
void unrecognized_statement(expptr form);
void write_decl(expptr);
void write_proc(expptr);
expptr proc_def(expptr type, expptr f, expptr args);
expptr insertions();
expptr extractions();

/** ========================================================================
The load function is given a list of fully macro-expanded expressions.
The patterns for expressions accepted by load is defined in the install function.
======================================================================== **/

expptr load(expptr forms, expptr value){ // forms and value must both be fully macro expanded.

  statements = NULL;
  mapc(install,forms);

  compilecount++; //avoids argument duplication problem with sformat
  char * s = sformat("TEMP%d.c",compilecount);
  open_output_file(s);

  dolist(form,file_preamble){print_line(form,fileout);}
  pprint(`{void * * symbol_value_copy;},fileout,0);
  mapc(write_decl,env_syms);
  mapc(write_proc,env_syms);

  pprint(`{
      expptr _mc_doit(voidptr * symbol_value){
	symbol_value_copy = symbol_value;
	${insertions()}
	${extractions()}
	${statements}
	${((value != NULL)? `{return ${value};} : `{return string_symbol("void");})}}},
    fileout,0);

  dolist(var, env_syms){setprop(var,`{new},NULL);}
  
  fclose(fileout);
  
  void * header = compile_load_file(sformat("TEMP%d",compilecount));
  expptr (* _mc_doit)(voidptr *);
  _mc_doit = dlsym(header,"_mc_doit");
  expptr result = (*_mc_doit)(symbol_value);
  return result;
}

void install(expptr form){ //only the following patterns are allowed.
  ucase{form;
    {typedef !def;}:{install_preamble(form);}
    {typedef !def1,!def2;}:{install_preamble(form);}
    {#define !def}:{install_preamble(form);}
    {#include < !file >}:{install_preamble(form);}
    {#include !x}:{install_preamble(form);}
    {?type * ?var;}:{install_pointer(type,var,`{NULL});}
    {?type ?X[?dim];}:{
      install_pointer(type,X,`{malloc(${dim}*sizeof(${type}))});}
    {?type ?var;}:{install_var(type,var,`{malloc(sizeof(${type}))})}
    {?type ?var = !e;}:{install_var(type,var,e);}
    {?type ?f(!args){!body}}:{install_proc(type, f, args, body);}
    {?type ?f(!args);}:{install_proc(type,f,args,NULL);} //this is needed for base_decls.h
    {?type !e;}:{unrecognized_statement(form);}
    {?type * !e;}:{unrecognized_statement(form);}
    {!e;}:{push(form,statements)}
    {{!statement}}:{push(form,statements)}
    {!e}:{unrecognized_statement(form)}}
}

void install_preamble(expptr e){
  if(!installedp(e)){
    file_preamble = append(file_preamble,(cons(e,NULL)));
    setprop(e,`{installed},`{true});}
}

void install_var(expptr type, expptr var, expptr initval){
  install_decl(var,`{${type} ${var};});
  if(newp(var)){
    expptr pointer = gensym(var);
    setprop(var,`{symbol_macro_expansion},`{(* ${pointer})});
    install_pointer(type, pointer, initval);}
}

void install_pointer(expptr type, expptr var, expptr initval){
  install_decl(var,`{${type} * ${var};});
  if(newp(var) || (initval != NULL && initval != getprop(var,`{initval},NULL))){
    setprop(var,`{initval},initval);
    setprop(var,`{new},`{true});}
}

void install_proc(expptr type, expptr f, expptr args, expptr body){
  install_decl(f,`{${type} ${f}(${args});});
  if(newp(f) || (body != NULL && body != getprop(f,`{body},NULL))){
    setprop(f,`{body},body);
    setprop(f,`{new},`{true});}
}

void install_decl(expptr var, expptr decl){
  expptr old_decl = getprop(var,`{declaration}, NULL);
  if(old_decl == NULL){
    push(var,env_syms);
    setprop(var,`{declaration},decl);
    setprop(var,`{new},`{true});
    symbol_index(var);} //this syncronizes base_decls.h indeces between expandE REPL.
  else if(old_decl != decl){
    push_dbg_expression(decl);
    berror("attempt to change global type declaration");}
}

void unrecognized_statement(expptr form){
  fprintf(stderr,"unrecognized statement %s\n", exp_string(form));
  fprintf(stderr,"you may need to define single-symbol types\n");
  berror("");
}

int symbol_index(expptr sym){
  int index = (int) getprop_int(sym, `{index}, -1);
  if(index == -1){
    if(symbol_count == SYMBOL_DIM){berror("Mc symbol table exhausted");}
    index = symbol_count++;
    setprop_int(sym,`{index}, index);
  }
  return index;
}

int newp(expptr sym){
  return getprop(sym,`{new},NULL) == `{true};
}

int installedp(expptr sym){
  return getprop(sym,`{installed},NULL) == `{true};
}

void write_decl(expptr sym){  //this generates both old and new declarations
  expptr decl = getprop(sym,`{declaration},NULL);
  ucase{decl;
    {?type ?var}:{}
    {!e}:{pprint(decl,fileout,0)}}
}

void write_proc(expptr sym){
  ucase{getprop(sym,`{declaration},NULL);
    {?type ?f(!args);}:{
      if(newp(f)){pprint(`{${type} ${f}(${args}){${getprop(f,`{body},NULL)}}},fileout,0);}
      else
	pprint(
	       `{${type} ${f}(${args}){
		   ${type} (* _mc_f)(${args});
		   _mc_f = symbol_value_copy[${int_exp(symbol_index(f))}];
		   ${(type == `{void} ? `{(* _mc_f)(${args_variables(args)});} : `{return (* _mc_f)(${args_variables(args)});})}}},
	       fileout, 0);}
    {!e}:{}}
}

expptr insertions(){
  expptr result = NULL;
  dolist(sym,env_syms){
    if(newp(sym)){
      ucase{getprop(sym,`{declaration},NULL); 
	{?type ?f(!args);}:{push(`{symbol_value[${int_exp(symbol_index(f))}] = ${f};}, result);}
	{?type * ?var;}:{push(`{symbol_value[${int_exp(symbol_index(var))}] = ${getprop(var,`{initval},NULL)};}, result)}
	{!e}:{}}}}
  return result;
}
	  
expptr extractions(){
  expptr result = NULL;
  dolist(sym,env_syms){
    ucase{getprop(sym,`{declaration},NULL);
      {?type * ?var;}:{
	push(`{${var} = (${type} *) symbol_value[${int_exp(symbol_index(var))}];}, result);}
      {!x}:{}}}
  return result;
}

voidptr compile_load_file(charptr fstring){
  int flg;
  char * s1 = sformat("cc -g -fPIC -Wall -c -Werror %s.c -o %s.o",fstring,fstring);
  flg = system(s1);
  if(flg != 0){
    fprintf(stderr,"\n evaluation aborted\n\n");
    throw_error();}
  char * s2 = sformat("cc -g -fPIC -shared -lm -Werror %s.o -o %s.so",fstring,fstring);
  flg = system(s2);
  if(flg != 0){
    fprintf(stderr,"\n evaluation aborted\n\n");
    throw_error();}
  char * s3 = sformat("./%s.so",fstring);
  voidptr header = dlopen(s3, RTLD_LAZY|RTLD_GLOBAL);
  if(header == NULL){
    fprintf(stderr,"unable to open shared library %s with error %i\n", s3, (*dlerror()));
    throw_error();}
  return header;
}

 /** ========================================================================
The macro insert__base_values() is used for initializing symbol_values.
See main in REPL.c (the C file).
======================================================================== **/

umacro{insert_base_values()}{
  expptr result = NULL;
  dolist(sym,env_syms){
    ucase{getprop(sym,`{declaration},NULL); 
      {?type ?f(!args);}:{
	push(`{symbol_value[${int_exp(symbol_index(f))}] = ${f};}, result);}
      {?type * ?var;}:{
	push(`{symbol_value[${int_exp(symbol_index(var))}] = ${var};}, result);}}}
  return result;
}

init_fun(mcE_init1)

void mcE_init2(){
  env_syms = NULL;
  file_preamble = NULL;
  compilecount = 0;
  symbol_count = 0;
  mapc(install, file_expressions(`{base_decls.h}));
  dolist(sym,env_syms){setprop(sym,`{new},NULL);}
}
