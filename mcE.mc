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

expptr decl_symbol(expptr decl);
int newp(expptr sym);
int installedp(expptr form);
expptr args_variables(expptr args);
expptr args_types(expptr args);

void install(expptr decl);
void write_new_definitions();
expptr hybernations();
voidptr compile_load_file(charptr fname);

void write_declarations();
expptr array_awakenings();

/** ========================================================================
Intuitively the REPL can perform the different actions of declaring data variables, defining procedures, and executing statements.

Each iteration of the REPL writes a C file, compiles the file into a DLL, loads the DLL and extracts
a main procedure and applies the main procedure to the array symbol_value.

However, in every invocation of REPL the expressions involved are macro expanded.  This means that every invocation must be treated as
performing a sequence of actions of any kind.  So the general case, which is equivalent to loading a file, is used in every invocation of the REPL.
This is done with the procedure load.
======================================================================== **/

/** ========================================================================
The REPL state includes C preprocessor #define statements, type definitions,
and an assignment of values to data variables and procedure names.

The #define statements and type definitions are both stored in a variable called file_preamble.  This is relatively straightforward
and will not be discussed futher in the documentation.

The set of procedure names and data variables is stored in the variable env_syms.

The value of each variable in env_syms is stored in the array symbol_value where we have
that symbol_value[symbol_index(x)] is the value of x.
======================================================================== **/


/** ========================================================================
The load function is given a list of fully macro-expanded expressions.  The types of expressions accepted by
load is defined in the install function.

Arrays are the only supported data variables at this time.  However, note that <type> X[1] is equivalent to <type> * X.
======================================================================== **/

void install_preamble(expptr);
void add_new_symbol(expptr,expptr);
void install_var(expptr,expptr);
void install_array(expptr,expptr);
void install_proc(expptr,expptr,expptr,expptr);
int symbol_index(expptr);
int newp(expptr);
int installedp(expptr);

void install(expptr form){ //only the following patterns are allowed.
  ucase{form;
    {typedef !def;}:{install_preamble(form);}
    {typedef !def1,!def2;}:{install_preamble(form);}
    {#define !def}:{install_preamble(form);}
    {#include < !file >}:{install_preamble(form);}
    {#include !x}:{install_preamble(form);}
    {?type ?var;}:{install_var(type,var);}
    {?type ?var = !e;}:{install_var(type,var);}
    {?type ?X[?dim];}:{install_array(X,form);}
    {?type ?f(!args){!body}}:{install_proc(type, f, args, form);}
    {?type ?f(!args);}:{install_proc(type,f,args,form);} //this is needed for base_decls.h
    {!e;}:{}
    {{!statement}}:{}}
  
}

void install_preamble(expptr e){
  if(!installedp(e)){
    file_preamble = append(file_preamble,(cons(e,NULL)));
    setprop(e,`{installed},`{true});}
}

void add_new_symbol(expptr x, expptr decl){
  push(x,env_syms);
  symbol_index(x); //this is needed to syncronize the base_decls.h indeces between the expandE process and the REPL process.
  setprop(x,`{declaration},decl);
  setprop(x,`{new},`{true});
}

void install_var(expptr type, expptr var){
  expptr decl = `{${type} ${var};};
  expptr old_decl = getprop(var,`{declaration}, NULL);
  if(old_decl == NULL){
    expptr var_pointer = gensym(var);
    add_new_symbol(var,decl);
    setprop(var,`{new},NULL);
    setprop(var,`{symbol_macro_expansion},`{(* ${var_pointer})});
    add_new_symbol(var_pointer,`{${type} ${var_pointer}[1];});
    return;}
  if(old_decl != decl){
    push_dbg_expression(decl);
    berror("attempt to change array declaration");}
}
  

  

void install_array(expptr X, expptr decl){
  expptr old_decl = getprop(X,`{declaration},NULL);
  if(old_decl == NULL){
    add_new_symbol(X,decl);
    return;}
  if(old_decl != decl){
    push_dbg_expression(decl);
    berror("attempt to change array declaration");}
}

void install_proc(expptr newtype, expptr f, expptr newargs, expptr newdecl){
  expptr olddecl = getprop(f,`{declaration},NULL);
  if(olddecl == NULL){
    add_new_symbol(f,newdecl);
    return;}
  ucase{olddecl;
    {?oldtype ?g(!oldargs){!oldbody}}:{
      if(newtype != oldtype || newargs!= oldargs){
	push_dbg_expression(newdecl);
	berror("attempt to change procedure signature");}
      setprop(f,`{declaration}, newdecl);
      setprop(f,`{new},`{true});}
    {!x}:{
      push_dbg_expression(olddecl);
      berror("attempt to redefine base procedure");
    }}
}

int symbol_count;

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

/** ========================================================================
load
======================================================================== **/

int compilecount;
expptr declarations();
expptr new_proc_defs();
expptr old_proc_defs();
expptr proc_def(expptr type, expptr f, expptr args);
expptr array_value_extractions();
expptr value_insertions();
expptr statements(expptr forms);

void pprint_out(expptr exp){
  pprint(exp,fileout,0);
}

expptr load(expptr forms, expptr value){ //the forms and the value expression must both be fully macro expanded.
  
  mapc(install,forms);

  compilecount++; //avoid argument duplication problem with sformat
  char * s = sformat("TEMP%d.c",compilecount);
  open_output_file(s);

  fprintf(fileout, "/* preamble */\n\n");
  dolist(form,file_preamble){print_line(form,fileout);}

  fprintf(fileout,"\n/** declarations (both old and new) **/\n\n");
  mapc(pprint_out,declarations());

  fprintf(fileout,"/** declaring symbol_value_copy **/\n\n");
  pprint(`{void * * symbol_value_copy;},fileout,0);

  fprintf(fileout,"/** old procedure definitions **/\n\n");  
  mapc(pprint_out,old_proc_defs());
  
  fprintf(fileout,"/** new procedure definitions **/\n\n");
  mapc(pprint_out,new_proc_defs());

  fprintf(fileout,"/** doit **/");
  pprint(`{
      expptr _mc_doload(voidptr * symbol_value){
	symbol_value_copy = symbol_value;
	${array_value_extractions()}
	${value_insertions()}
	${statements(forms)}
	${((value != NULL)? `{return ${value};} : `{return string_symbol("void");})}}},
    fileout,0);
  fclose(fileout);
  
  void * header = compile_load_file(sformat("TEMP%d",compilecount));
  expptr (* _mc_doload)(voidptr *);
  _mc_doload = dlsym(header,"_mc_doload");
  expptr result = (*_mc_doload)(symbol_value);
  dolist(sym, env_syms){setprop(sym,`{new},NULL);};
  return result;
}

expptr declarations(){  //this generates both old and new declarations
  expptr result = NULL;
  dolist(sym, env_syms){
    expptr decl = getprop(sym,`{declaration},NULL);
    ucase{decl;
      {?type ?f(!args){!body}}:{push(`{${type} ${f}(${args});},result);}
      {?type ?f(!args);}:{push(`{${type} ${f}(${args});},result);}
      {?type ?var[?dimension];}:{
	if(!newp(var)){push(`{${type} * ${var};},result);}
	else{push(decl,result);}}
      {!x}:{}}}
  return result;
}

expptr new_proc_defs(){
  expptr result = NULL;
  dolist(sym, env_syms){
    expptr decl = getprop(sym,`{declaration},NULL);
    ucase{decl;
      {?type ?f(!args){!body}}:{
	if(newp(f)){push(`{${type} ${f}(${args}){${body}}},result);}}
      {!x}:{}}}
  return result;
}

expptr old_proc_defs(){
  expptr result = NULL;
  dolist(sym, env_syms){
    if(!newp(sym)){
      ucase{getprop(sym,`{declaration},NULL);
	{?type ?f(!args){!body}}:{push(proc_def(type,f,args), result);}
    	{?type ?f(!args);}:{push(proc_def(type,f,args), result);}
	{!x}:{}}}}
  return result;
}

expptr proc_def(expptr type, expptr f, expptr args){
  expptr g = gensym(`{f});
  return
    `{${type} ${f}(${args}){
      ${type} (* ${g})(${args});
      ${g} = symbol_value_copy[${int_exp(symbol_index(f))}];
      ${(type == `{void} ? `{(* ${g})(${args_variables(args)});} : `{return (* ${g})(${args_variables(args)});})}}};
}


expptr array_value_extractions(){
  expptr result = NULL;
  dolist(sym, env_syms){
    if(!newp(sym)){
      ucase{getprop(sym,`{declaration},NULL);
	{?type ?var[?dimension];}:{
	  push(`{${sym} = (${type} *) symbol_value_copy[${int_exp(symbol_index(sym))}];}, result);}
	{!x}:{}}}}
  return result;
}

expptr value_insertions(){
  expptr result = NULL;
  dolist(sym, env_syms){
    if(newp(sym)){
      push(`{symbol_value[${int_exp(symbol_index(sym))}] = (voidptr) ${sym};},
	   result);}}
  return result;
  }

expptr statements(expptr forms){
  expptr result = NULL;
  dolist(form, forms){
    ucase{form;
      {{!statement}}:{push(form,result);}
      {typedef !def;}:{}
      {typedef !def1,!def2;}:{}
      {?type ?var;}:{}
      {?type ?X[?dim];}:{}
      {?type ?f(!args);}:{}
      {?type ?var = !e;}:{push(`{${macroexpand(var)} = ${e};},result)}
      {!e;}:{push(form,result);}      
      {!x}:{}}}
  return result;
}

/** ========================================================================

The macro set_base_values() is used for initializing the base environment.  See the procedure load_basetype
in REPL.mc

======================================================================== **/

umacro{insert_base_values()}{return value_insertions();}

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
  char * s3 = sformat("%s.so",fstring);
  voidptr header = dlopen(s3, RTLD_LAZY|RTLD_GLOBAL);
  if(header == NULL)throw_error();
  return header;
}

init_fun(mcE_init1)

void mcE_init2(){
  env_syms = NULL;
  file_preamble = NULL;
  compilecount = 0;
  symbol_count = 0;
}
