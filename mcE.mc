#include "mc.h"

/** ========================================================================
In this implementation all global data variables must be arrays.
We can replace a declaration

    <type>  x;

by

   <type> x[1];

and replace x by x[0] throughout the code.

An array variable x can be dynamically linked into the DLL code in such a way that assignments
to x[i] in the DLL code do the right thing.  This is not true of assignments
to x as opposed to assignments to x[i]. To support assignment to x (as opposed to x[i])
we need to be able to identify the FREE occurances of x in the code. This implementation
does not support that level of analysis of the C code.

MetaC is designed to create new languages.  The implementation of a new language will support
better code analysis of the new langauge.
======================================================================== **/

expptr file_preamble;
expptr procedures;
expptr arrays;

expptr new_procedures;
expptr new_arrays;
expptr new_statements;

int compilecount;
int symbol_count;

expptr args_variables(expptr args);
void install(expptr sig);
voidptr compile_load_file(charptr fstring);
void install_preamble(expptr);
void install_var(expptr,expptr,expptr);
void install_proc(expptr,expptr,expptr,expptr);
int symbol_index(expptr);
expptr symbol_index_exp(expptr);
void unrecognized_statement(expptr form);
expptr proc_def(expptr f);
void install_base();

void mcE_init2(){
  file_preamble = nil;
  arrays = nil;
  procedures = nil;
  symbol_count = 0;
  install_base();  //eval-when compile and load (run in both expandE and REPL).
  compilecount = 0;
}

void install_base(){
  dolist(sig,file_expressions("base_decls.h")){
    ucase{sig;
      {$type $f($args);}:{
	symbol_index(f);  //synchoronizes compile and load indeces.
	push(f,procedures);
	setprop(f,`{signature},sig);}
      {$type $x[$dim];}:{
	symbol_index(x);  //synchoronizes compile and load indeces.
	push(x,arrays);
	setprop(x,`{signature},sig);}
      {$e}:{push(e,file_preamble);}}}
}

/** ========================================================================
The REPL must start with the base symbols inserted.  The following is run at "load time".
The macro insert_base() appears at the beginning of main in REPL.mc.
It is expanded during the compilation of REPL (by expandE) after running install_base().

New procedures must be defined one at a time so that calls to other procedures go through
the dynamic linking table.
======================================================================== **/

umacro{insert_base()}{
  expptr result = nil;
  dolist(f,procedures){
    push(`{symbol_value[${symbol_index_exp(f)}] = $f;}, result);}
  dolist(X,arrays){
    push(`{symbol_value[${symbol_index_exp(X)}] = $X;}, result);}
  return result;
}

init_fun(mcE_init1)  //mcE_init1 just installs the above macro.

expptr new_procedure_insertion (expptr f){
  return `{symbol_value[${symbol_index_exp(f)}] = $f;};
}

expptr new_array_insertion (expptr x){
  ucase{getprop(x,`{signature},nil);
    {$type $x[$dim];}:{return `{symbol_value[${symbol_index_exp(x)}] = malloc($dim*sizeof($type));};}}
  return nil; //avoids compiler warning
}

expptr array_extraction (expptr x){
  return `{$x = symbol_value[${symbol_index_exp(x)}];};
}

/** ========================================================================
The load function is given a list of fully macro-expanded expressions.
======================================================================== **/

expptr load(expptr forms){ // forms must be fully macro expanded.

  compilecount ++; //should not be inside sformat --- sformat duplicates.
  char * s = sformat("/tmp/TEMP%d.c",compilecount);
  fileout = fopen(s, "w");

  new_procedures = nil;
  new_arrays = nil;
  new_statements = nil;
  
  mapc(install,forms);
  dolist(form,reverse(file_preamble)){pprint(form,fileout,rep_column);}
  fputc('\n',fileout);
  pprint(`{void * * symbol_value_copy;},fileout,0);

  //variable declarations
  dolist(f,procedures){pprint(getprop(f,`{signature},NULL),fileout,0);}
  dolist(x,arrays){
    ucase{getprop(x,`{signature},NULL);
      {$type $x[$dim];}:{pprint(`{$type * $x;},fileout,0);}}}

  //procedure value extractions.  array extractions are done in doit.
  dolist(f,procedures){pprint(proc_def(f),fileout,0);} 

  pprint(`{
      expptr _mc_doit(voidptr * symbol_value){
	symbol_value_copy = symbol_value;
	${mapcar(new_procedure_insertion, new_procedures)}
	${mapcar(new_array_insertion, new_arrays)}
	${mapcar(array_extraction, arrays)} // procedure extractions are done by procdefs above
	${reverse(new_statements)}
	return string_atom("done");}},
    fileout,0);
  fclose(fileout);
  
  void * header = compile_load_file(sformat("/tmp/TEMP%d",compilecount));
  if(!in_repl){fprintf(stdout, "}ignore}");}
  in_doit = 1;
  expptr (* _mc_doit)(voidptr *);
  _mc_doit = dlsym(header,"_mc_doit");
  return (*_mc_doit)(symbol_value);
}

void install(expptr statement){ //only the following patterns are allowed.
  ucase{statement;
    {typedef $def;}:{install_preamble(statement);}
    {typedef $def1,$def2;}:{install_preamble(statement);}
    {#define $def}:{install_preamble(statement);}
    {#include <$file>}:{install_preamble(statement);}
    {#include $x}:{install_preamble(statement);}
    {return $e;}:{push(statement,new_statements);}
    {$type $X[0];}.(symbolp(type) && symbolp(X)):{install_var(type,X,`{1});}
    {$type $X[0] = $e;}.(symbolp(type) && symbolp(X)):{install_var(type,X,`{1}); push(`{$X[0] = $e;},new_statements);}
    {$type $X[$dim];}.(symbolp(type) && symbolp(X)):{install_var(type,X,dim);}
    {$type $f($args){$body}}.(symbolp(type) && symbolp(f)):{install_proc(type, f, args, body);}
    {$type $f($args);}.(symbolp(type) && symbolp(f)):{install_proc(type, f, args, NULL);}
    {$e;}:{push(statement,new_statements)}
    {{$e}}:{push(statement,new_statements)}
    {$e}:{push(`{return $e;},new_statements)}}
}

void install_preamble(expptr e){
  if(!getprop(e,`{installed},NULL)){
    push(e,file_preamble);
    setprop(e,`{installed},`{true});}
}

void install_var(expptr type, expptr X, expptr dim){
  expptr oldsig = getprop(X,`{signature}, NULL);
  expptr newsig = `{$type $X[$dim];};
  if(oldsig != NULL && newsig != oldsig)uerror(`{attempt to change the type declaration, $oldsig , to $newsig});
  if(oldsig == NULL){
    setprop(X,`{signature},newsig);
    push(X, arrays);
    push(X,new_arrays);}
}

void install_proc(expptr type, expptr f, expptr args, expptr newbody){
  expptr oldsig = getprop(f,`{signature}, NULL);
  expptr oldbody = getprop(f,`{body},NULL);
  expptr newsig = `{$type $f($args);};
  if(oldsig != NULL && newsig != oldsig)uerror(`{attempt to change $oldsig to \n $newsig});
  if(oldsig == NULL){
    setprop(f,`{signature},newsig);
    push(f, procedures);}
  if(oldbody != newbody){
    push(f, new_procedures);
    setprop(f,`{body},newbody);
    setprop(f,`{new},`{true});}
}

void unrecognized_statement(expptr form){
  uerror( `{unrecognized statement,
	$form ,
	types must be single symbols,
	all global variables must be arrays,
    });
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
    {$type $f($args);}:{
      if(getprop(f,`{new},NULL)){
	setprop(f,`{new},NULL);
	return `{$type $f($args){${getprop(f,`{body},NULL)}}};}
      else {return
	  `{$type $f($args){
	    $type (* _mc_f)($args);
	    _mc_f = symbol_value_copy[${symbol_index_exp(f)}];
	    ${(type == `{void} ?
	       `{(* _mc_f)(${args_variables(args)});}
	       : `{return (* _mc_f)(${args_variables(args)});})}}};}}}
  return NULL;
}

void comp_error(){
    if(!in_repl){
      fprintf(stdout, "}compilation error}");}
    else{
      fprintf(stdout,"\n evaluation aborted\n\n");}
    throw_error();}

voidptr compile_load_file(charptr fstring){
  int flg;
  
  char * s1 = sformat("cc -g -fPIC -Wall -c -Werror %s.c -o %s.o",fstring,fstring);
  flg = system(s1);
  if(flg != 0)comp_error();

  char * s2 = sformat("cc -g -fPIC -shared -lm -Werror %s.o -o %s.so",fstring,fstring);
  flg = system(s2);
  if(flg != 0)comp_error();

  char * s3 = sformat("%s.so",fstring);
  voidptr header = dlopen(s3, RTLD_LAZY|RTLD_GLOBAL);
  if(header == NULL){
    fprintf(stdout,"\nunable to open shared library %s with error %s\n", s3, dlerror());
    comp_error();}
  return header;
}
