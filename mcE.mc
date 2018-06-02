#include "mc.h"

expptr file_preamble;
expptr declarations;
expptr procedures;
expptr base_datavars; //used only to communicate between install_base and insert_base_values.
expptr file_statements;
expptr doit_statements;

int compilecount;
int symbol_count;

expptr args_variables(expptr args);
void install(expptr decl);
voidptr compile_load_file(charptr fstring);
void install_preamble(expptr);
void install_var(expptr,expptr,expptr);
void install_proc(expptr,expptr,expptr,expptr);
int symbol_index(expptr);
expptr symbol_index_exp(expptr);
void unrecognized_statement(expptr form);
void write_sig(expptr);
void write_proc(expptr);
expptr proc_def(expptr f);
expptr symbol_expand(expptr e);
void install_base();

void mcE_init2(){
  file_preamble = NULL;
  declarations = NULL;
  procedures = NULL;
  symbol_count = 0;
  install_base();
  compilecount = 0;
}

void install_base(){
  base_datavars = NULL;
  dolist(decl,file_expressions(`{base_decls.h})){
    ucase{decl;
      {?type ?f(!args);}:{
	setprop(f,`{signature},decl);
	push(f,procedures);
	push(decl,declarations);
	symbol_index(f);} // establish base indeces
      {?type ?x;}:{
	setprop(x,`{symbol_macro_expansion},
		`{(* (${type} *) symbol_value_copy[${symbol_index_exp(x)}])});
	push(decl,declarations);
	push(x,base_datavars);
	symbol_index(x);} // establish base indeces
      {!e}:{push(e,file_preamble);}}}
}

/** ========================================================================
The macro insert_base_values() appears at the beginning of main in REPL.mc.
It is expanded by expandE after expandE calls mcE_init2().
======================================================================== **/
  
umacro{insert_base_values()}{
  expptr result = NULL;
  dolist(f,procedures){
    push(`{symbol_value[${symbol_index_exp(f)}] = ${f};}, result);}
  dolist(x,base_datavars){
    push(`{symbol_value[${symbol_index_exp(x)}] = & ${x};}, result);}
  return result;
}

init_fun(mcE_init1)  //this just installs the init-form for the above macro.

/** ========================================================================
The load function is given a list of fully macro-expanded expressions.
======================================================================== **/

expptr load(expptr forms){ // forms must both be fully macro expanded.

  compilecount ++; //avoids argument duplication problem with sformat
  char * s = sformat("TEMP%d.c",compilecount);
  open_output_file(s);

  file_statements = NULL;
  doit_statements = NULL;

  mapc(install,forms);
  dolist(form,reverse(file_preamble)){print_line(form,fileout);}
  fputc('\n',fileout);
  pprint(`{void * * symbol_value_copy;},fileout,0);
  dolist(form,reverse(declarations)){pprint(form,fileout,0);}
  dolist(f,procedures){pprint(symbol_expand(proc_def(f)),fileout,0);}
  dolist(form,reverse(file_statements)){pprint(symbol_expand(form),fileout,0);}

  pprint(`{
      expptr _mc_doit(voidptr * symbol_value){
	symbol_value_copy = symbol_value;
	${symbol_expand(reverse(doit_statements))}
	return `{done};}},
    fileout,0);
  fclose(fileout);
  
  void * header = compile_load_file(sformat("TEMP%d",compilecount));
  expptr (* _mc_doit)(voidptr *);
  _mc_doit = dlsym(header,"_mc_doit");
  return (*_mc_doit)(symbol_value);
}

void install(expptr statement){ //only the following patterns are allowed.
  ucase{statement;
    {typedef !def;}:{install_preamble(statement);}
    {typedef !def1,!def2;}:{install_preamble(statement);}
    {#define !def}:{install_preamble(statement);}
    {#include < !file >}:{install_preamble(statement);}
    {#include !x}:{install_preamble(statement);}
    {?type ?var;}:{install_var(type,var,NULL);}
    {?type ?var = !e;}:{install_var(type,var,e);}
    {?type ?X[?dim];}:{
      install_var(`{${type} *},X,`{malloc(${dim}*sizeof(${type}))})}
    {?type ?f(!args){!body}}:{install_proc(type, f, args, body);}
    {return !e;}:{push(statement,doit_statements);}
    {?type !e;}:{unrecognized_statement(statement);}
    {?type * !e;}:{unrecognized_statement(statement);}
    {!e;}:{push(statement,doit_statements)}
    {{!statement}}:{push(statement,doit_statements)}
    {!e}:{unrecognized_statement(statement)}}
}

void install_preamble(expptr e){
  if(!getprop(e,`{installed},NULL))
    file_preamble = append(file_preamble,(cons(e,NULL)));
    setprop(e,`{installed},`{true});}
}

void install_var(expptr type, expptr var, expptr initval){
  expptr oldtype = getprop(var,`{type},NULL);
  expptr replacement = `{(* (${type} *) symbol_value_copy[${symbol_index_exp(var)}])};
  if(oldtype != NULL && type != oldtype)berror("attempt to change variable type");
  if(oldtype == NULL){
    setprop(var,`{type},type);
    //the variable does not need to be declared --- it does not exist in the dll.
    setprop(var,`{symbol_macro_expansion}, replacement);
    push(`{symbol_value[${symbol_index_exp(var)}] = malloc(sizeof(${type}))}, doit_statements);}
  expptr oldinit = getprop(var,`{initval},NULL);
  if(oldinit != NULL && initval != oldinit){
    setprop(var,`{initval},initval);
    push(`{${replacement} = ${initval};}, doit_statements);}
}

void install_proc(expptr type, expptr f, expptr args, expptr newbody){
  expptr oldsig = getprop(f,`{signature}, NULL);
  expptr newsig = `{${type} ${f}(${args})};
  if(oldsig != NULL && newsig != oldsig)berror("attempt to change procedure signature");
  if(oldsig == NULL){
    setprop(f,`{signature},newsig);
    push(newsig, declarations);}
  expptr oldbody = getprop(f,`{body},NULL);
  if(newbody != NULL && newbody != oldbody) setprop(f,`{body}, newbody);
  if(oldsig == NULL || newbody != oldbody){
    push(`{symbol_value[${symbol_index_exp(f)}] = ${f};}, doit_statements);
    setprop(f,`{new},`{true});}
}

expptr symbol_expand(expptr e){
  if(e == NULL)return e;
  if(constructor(e) == 'a'){
    expptr expansion = getprop(e,`{symbol_macro_expansion},NULL);
    if(expansion != NULL)return expansion;}
  if(atomp(e))return e;
  return intern_exp(constructor(e),symbol_expand(arg1(e)),symbol_expand(arg2(e)));
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

expptr symbol_index_exp(expptr sym){
  return int_exp(symbol_index(sym));
}

expptr proc_def(expptr f){
  ucase{getprop(f,`{signature},NULL);
    {?type ?f(!args);}:{
      if(f == `{cons})cbreak();
      if(getprop(f,`{new},NULL)){
	setprop(f,`{new},NULL);
	return `{${type} ${f}(${args}){${getprop(f,`{body},NULL)}}};}
      else {return
	  `{${type} ${f}(${args}){
	    ${type} (* _mc_f)(${args});
	    _mc_f = symbol_value_copy[${symbol_index_exp(f)}];
	    ${(type == `{void} ?
	       `{(* _mc_f)(${args_variables(args)});}
	       : `{return (* _mc_f)(${args_variables(args)});})}}};}}}
  return NULL;
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
