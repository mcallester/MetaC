#include "mc.h"

voidptr symbol_value[SYMBOL_DIM] = {0};

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
expptr new_sig_procedures;
expptr new_arrays;
expptr new_statements;
expptr new_preambles;

int compilecount;
int cellcount;
int symbol_count;

expptr args_variables(expptr args);
void install(expptr sig);
voidptr compile_load_file(charptr fstring);
void install_preamble(expptr);
void install_var(expptr,expptr,expptr);
void install_proc(expptr,expptr,expptr,expptr);
void install_proc_def(expptr f);
void install_link_def(expptr f);
int symbol_index(expptr);
expptr symbol_index_exp(expptr);
void install_base();
char * strip_quotes(char *);
expptr load(expptr);
void print_preamble(expptr);

/** ========================================================================
The REPL inserts base procedures into the symbol_value table (the linking table) by calling the macro insert_base.
This macro is expanded by ExpandE on REPL.  Before doing any expansion, ExpandE sets the symbol indeces of the base symbols by calling install_base().
install_base must be run again by the REPL so that indeces used by the REPL match those used
by expandE (the indeces used at REPL load time must be syncronized with those used at REPL compile time).
mcE_init1 establishes base procedure indeces so that all executables built on mcE have the same base procedure indeces.

Becasue of REPL compile and load syncronization, install_base cannot be cleanly replaced with calls to install.
======================================================================== **/

void mcE_init1(){
  file_preamble = nil;
  add_undone_pointer((void**) &file_preamble);
  arrays = nil;
  add_undone_pointer((void**) &arrays);
  procedures = nil;
  add_undone_pointer((void**) &procedures);
  symbol_count = 0;
  add_undone_int(&symbol_count);
  install_base();  //all executables built on mcE (ExpandE, REPL and IDE) have the same indeces for base functions.
  compilecount = 0;
  cellcount = 0;
  add_undone_int(&cellcount);
}

umacro{insert_base()}{
  expptr result = nil;
  dolist(f,procedures){
    push(`{symbol_value[${symbol_index_exp(f)}] = $f;}, result);}
  dolist(X,arrays){
    push(`{symbol_value[${symbol_index_exp(X)}] = $X;}, result);}
  return result;
}

init_fun(mcE_init2)  //the procedure mcE_init2() installs the macro insert_base (without calling it).

void install_base(){
  dolist(sig,file_expressions(sformat("%sbase_decls.h", MetaC_directory))){
    ucase{sig;
      {$type $f($args);}.(symbolp(type) && symbolp(f)):{
	symbol_index(f);  //establish the index
        setprop(f,`{base},`{true});
        push(f,procedures);
	setprop(f,`{signature},sig);
      }
      {$type $x[$dim];}:{
	symbol_index(x);  //establish the index
	push(x,arrays);
	setprop(x,`{signature},sig);}
      {$e}:{push(e,file_preamble);} //typedefs
    }}
}

/** ========================================================================
insertion and extraction from the linking table.  Procedure extraction is
done by defining the procedure in the DLL to go through the linking table.
======================================================================== **/

expptr new_procedure_insertion (expptr f){
    return `{
      symbol_value[${symbol_index_exp(f)}] = ${getprop(f,`{gensym_name},NULL)};};
}

expptr new_array_insertion (expptr x){
  ucase{getprop(x,`{signature},nil);
    {$type $x[$dim];}:{return `{symbol_value[${symbol_index_exp(x)}] = malloc($dim*sizeof($type));};}}
  return nil; //avoids compiler warning
}

expptr array_extraction (expptr x){
  if(getprop(x,`{signature},NULL)) return `{$x = symbol_value[${symbol_index_exp(x)}];};
  else return `{{}};
}

/** ========================================================================
 eval_exp
======================================================================== **/

expptr simple_eval(expptr exp){
  preamble= nil;
  init_forms = nil;
  in_doit = 0;
  expptr e = macroexpand(exp);
  ucase{e;
    {load($sym)}.(atomp(sym)) : {
      fprintf(stdout,"recursive load is not supported in MetaC relase 1.0");
      if(in_ide)send_emacs_tag(comp_error_tag);
      if(in_repl)fprintf(stdout,"\n evaluation aborted");
      throw_error();}
    {$any} : {return load(append(preamble,append(init_forms,cons(e,nil))));}}
}

void simple_eval_noval(expptr e){ //this can be used with mapc
  simple_eval(e);
}

void eval_exp(expptr exp){
  ucase{exp;
    {load($sym)}.(atomp(sym)) : {
      char * require_file=sformat("%s.mc",strip_quotes(atom_string(sym)));
      mapc(simple_eval_noval,file_expressions(require_file));
      fprintf(stdout,"%s Provided\n",require_file); }
    {$any} : {pprint(simple_eval(exp),stdout,0);}}
}

expptr load(expptr forms){ // forms must be fully macro expanded.

  compilecount++; //this needs to be here becasue sformat duplicates the second argument
  char * s = sformat("/tmp/TEMP%d.c",compilecount);
  fileout = fopen(s, "w");
  fprintf(fileout,"#include \"%spremacros.h\"\n", MetaC_directory);

  new_procedures = nil;
  new_sig_procedures = nil;
  new_arrays = nil;
  new_statements = nil;
  new_preambles = nil;
  
  mapc(install,forms);
  mapc(print_preamble,reverse(file_preamble));
  mapc(print_preamble,reverse(new_preambles));
  fputc('\n',fileout);
  pprint(`{void * * symbol_value_copy;},fileout,0);

  //variable declarations
  dolist(f,procedures){
    expptr sig =getprop(f,`{signature},NULL);
    if(sig){pprint(sig,fileout,0);}
  }
  dolist(x,arrays){
    ucase{getprop(x,`{signature},NULL);
      {$type $x[$dim];}:{pprint(`{$type * $x;},fileout,0);}
      {$any}:{}}}

  //procedure value extractions.  array extractions are done in doit.

  mapc(install_link_def,procedures);
  mapc(install_proc_def,new_procedures);

  pprint(`{
      expptr _mc_doit(voidptr * symbol_value){
	symbol_value_copy = symbol_value;
	${mapcar(new_procedure_insertion, new_procedures)}
	${mapcar(new_array_insertion, new_arrays)}
	${mapcar(array_extraction, arrays)} // procedure extractions are done by install_link_def above
	${reverse(new_statements)}
	return string_atom("done");}},
    fileout,0);
  fclose(fileout);
  
  void * header = compile_load_file(sformat("/tmp/TEMP%d",compilecount));

  if(in_ide){send_emacs_tag(ignore_tag);}
   expptr (* _mc_doit)(voidptr *);
  _mc_doit = dlsym(header,"_mc_doit");

  in_doit = 1;
  expptr e = (*_mc_doit)(symbol_value);
  mapc(install_preamble,reverse(new_preambles));
  return `{${int_exp(++cellcount)}: $e};
}

void install(expptr statement){
  ucase{statement;
    {typedef $def;}:{if(!getprop(statement,`{installed},NULL)) push(statement, new_preambles);}
    {typedef $def1,$def2;}:{if(!getprop(statement,`{installed},NULL)) push(statement, new_preambles);}
    {#include <$any>}:{install_preamble(statement);}
    {return $e;}:{push(statement,new_statements);}
    {$type $X[0];}.(symbolp(type) && symbolp(X)):{install_var(type,X,`{1});}
    {$type $X[0] = $e;}.(symbolp(type) && symbolp(X)):{install_var(type,X,`{1}); push(`{$X[0] = $e;},new_statements);}
    {$type $X[$dim];}.(symbolp(type) && symbolp(X)):{install_var(type,X,dim);}
    {$type $f($args){$body}}.(symbolp(type) && symbolp(f)):{install_proc(type, f, args, body);}
    {$type $f($args);}.(symbolp(type) && symbolp(f)):{install_proc(type, f, args, NULL);}
    {$e;}:{push(statement,new_statements)}
    {{$e}}:{push(statement,new_statements)}
    {$any}:{push(`{return $statement;},new_statements)}}
}

void install_preamble(expptr e){
  if(!getprop(e,`{installed},NULL)){
    push(e,file_preamble);
    setprop(e,`{installed},`{true});}
}

void print_preamble(expptr e){
  ucase{e;
    {#include <$f>}:{fprintf(fileout,"#include <%s>\n", exp_string(f));}
    {$any}:{pprint(e,fileout,rep_column);}}
}

void install_var(expptr type, expptr X, expptr dim){
  expptr oldsig = getprop(X,`{signature}, NULL);
  expptr newsig = `{$type $X[$dim];};
  if(!getprop(X,`{on_arrays},NULL)){
    push(X,arrays);
    setprop(X,`{on_arrays},`{true});}
  if(oldsig != NULL && newsig != oldsig){
    berror(sformat("attempt to change signature from\n %s\n to\n %s\n",
		   exp_string(oldsig),
		   exp_string(newsig)));
  }
  if(oldsig == NULL){
    setprop(X,`{signature},newsig);
    push(X,new_arrays);}
}

void install_proc(expptr type, expptr f, expptr args, expptr body){
  expptr oldsig = getprop(f,`{signature}, NULL);
  expptr newsig = `{$type $f($args);};
  if(!getprop(f,`{on_procedures},NULL)){
    push(f,procedures);
    setprop(f,`{on_procedures},`{true});}
  if(!oldsig){
    push(f,new_sig_procedures);
    setprop(f,`{signature},newsig);}    
  if(oldsig && newsig != oldsig){
    berror(sformat("attempt to change signature from\n %s\n to\n %s\n",
		   exp_string(oldsig),
		   exp_string(newsig)));}
  if(body){
    if (getprop(f,`{base},NULL))berror(sformat("attempt to change base function %s",atom_string(f)));
    push(f, new_procedures);
    setprop(f,`{gensym_name},gensym(atom_string(f)));
    setprop(f,`{body},body);
  }
}

int symbol_index(expptr sym){
  int index = getprop_int(sym, `{index}, -1);
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

void install_link_def(expptr f){
  ucase{getprop(f,`{signature},NULL);
    {$type $f($args);}:{
      pprint(
	     `{$type $f($args){
		 $type (* _mc_f)($args);
		 _mc_f = symbol_value_copy[${symbol_index_exp(f)}];
		 
		 if(!_mc_f){berror("call to undefined procedure");}
		 
		 ${(type == `{void} ?
		    `{(* _mc_f)(${args_variables(args)});}
		    : `{return (* _mc_f)(${args_variables(args)});})}}},
	     fileout,
	     0);}
    {$any}:{}}
}

void install_proc_def(expptr f){
  ucase{getprop(f,`{signature},NULL);
    {$type $f($args);}:{
      pprint(
	     `{$type ${getprop(f,`{gensym_name},NULL)}($args){
		 ${getprop(f,`{body},NULL)}}},
	     fileout, 0);}
    {$e}:{}}
}

void comp_error(){
  fflush(stderr);
  dolist(f,new_sig_procedures){setprop(f,`{signature},NULL);}
  dolist(x,new_arrays){setprop(x,`{signature},NULL);}
  if(in_ide)send_emacs_tag(comp_error_tag);
  else fprintf(stdout,"\n evaluation aborted\n\n");
  throw_error();
}

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
