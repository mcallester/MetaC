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

An important design convention is that evaluation of a NIDE cell not have any
effect in the event of any error prior to the execution of the compiled do_it procedure.
In particular errors during macro expansion or compilation should block all effects.
This can be achieved by performing all effects with the add_form construct so that
the effect is done from inside do_it.
======================================================================== **/

expptr file_preamble; // must be careful to avoid name clash with preamble used by add_preamble in mcA.c
expptr procedures;
expptr arrays;

expptr new_body_procedures;
expptr new_sig_procedures;
expptr new_arrays;
expptr doit_statements;
expptr new_preambles;

void install_preamble(expptr e){
  if(!getprop_int(e,`{installed},0)){
    push(e,file_preamble);
    setprop_int(e,`{installed},1);}
}

void install_array(expptr e){
  if(!getprop_int(e,`{installed},0)){
    push(e,arrays);
    setprop_int(e,`{installed},1);}
}

void install_procedure(expptr e){
  if(!getprop_int(e,`{installed},0)){
    push(e,procedures);
    setprop_int(e,`{installed},1);}
}

int compilecount;
int cellcount;
int symbol_count;

expptr args_variables(expptr args);
void preinstall(expptr sig);
voidptr compile_load_file(charptr fstring);
void preinstall_array(expptr,expptr,expptr);
void preinstall_proc(expptr,expptr,expptr,expptr);
void install_proc_def(expptr f);
void install_link_def(expptr f);
int symbol_index(expptr);
expptr symbol_index_exp(expptr);
void install_base();
char * strip_quotes(char *);
expptr eval_internal(expptr);
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
  file_preamble = nil;  // must be careful to avoid name clash with preamble used by add_preamble in mcA.c
  add_undone_pointer((void**) &file_preamble);
  arrays = nil;
  add_undone_pointer((void**) &arrays);
  procedures = nil;
  add_undone_pointer((void**) &procedures);
  symbol_count = 0;
  add_undone_int(&symbol_count);
  install_base();  //ExpandE and NIDE need to have the same indeces for base functions.
  compilecount = 0;
  cellcount = 0;
  add_undone_int(&cellcount);
}

umacro{insert_base()}{
  expptr result = nil;
  dolist(f,procedures){
    push(`{symbol_value[${symbol_index_exp(f)}] = $f;}, result);};
  dolist(X,arrays){
    push(`{symbol_value[${symbol_index_exp(X)}] = $X;}, result);};
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
      };
      {$type $x[$dim];}.(symbolp(type) && symbolp(x)):{
	symbol_index(x);  //establish the index
	push(x,arrays);
	setprop(x,`{signature},sig);};
      {$e}:{push(e,file_preamble);} //typedefs
    }}
}

/** ========================================================================
insertion is the process of filling values in the symbol_value array.
extraction is the process of extracting values from the symbol_value array.

arrays are allocated once and never re-allocated.  Hence extraction can be done
by a simple assignment to the local dll array variable.

procedures can be redefined which changes their memory location.  For dynamic linking to work
procedure extraction must be done at procedure call time which is done by defining the local
dll procedure to do the exprtaction at call time.
======================================================================== **/

expptr new_procedure_insertion (expptr f){
    return `{
      symbol_value[${symbol_index_exp(f)}] = ${getprop(f,`{gensym_name},NULL)};};
}

expptr new_array_insertion (expptr x){
  ucase{getprop(x,`{signature},nil);
    {$type $x[$dim];}:{return `{symbol_value[${symbol_index_exp(x)}] = malloc($dim*sizeof($type));};}};
  return nil; //avoids compiler warning
}

expptr array_extraction (expptr x){
  return `{$x = symbol_value[${symbol_index_exp(x)}];};
}


expptr strip_type(expptr arg){
  ucase{arg;
    {$type1 $var($type2)}.(symbolp(var)):{return var;};
    {$type $var}.(symbolp(var)):{return var;};
    {$any}:{berror("illegal function signature --variable names must be provided");}};
  return NULL;
}

expptr args_variables(expptr args){
  if(args == nil)return nil;
  ucase{args;
    {$first, $rest}:{return `{${strip_type(first)} , ${args_variables(rest)}};};
    {$any}:{return strip_type(args);}};
  return NULL;
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
	     0);};
    {$any}:{}}
}

/** ========================================================================
 eval_exp
======================================================================== **/

expptr simple_eval(expptr exp){
  preamble = nil;  //this is the preamble of add_premble in mcA.c
  init_forms = nil;
  in_doit = 0;
  expptr e = macroexpand(exp);
  return eval_internal(append(preamble,append(init_forms,cons(e,nil))));
}

void eval_from_load(expptr e){
  ucase{e;
    {restart_undo_frame($any);}:{
      fprintf(stdout,"loaded file contains an undo restart");
      if(in_ide)send_emacs_tag(comp_error_tag);
      if(in_repl)fprintf(stdout,"\n evaluation aborted");
      throw_error();};
    {load($sym);}.(atomp(sym)) : {
      fprintf(stdout,"recursive load is not yet supported");
      if(in_ide)send_emacs_tag(comp_error_tag);
      if(in_repl)fprintf(stdout,"\n evaluation aborted");
      throw_error();};
    {$any} : {simple_eval(e);}}
}

expptr eval_exp(expptr exp){
  ucase{exp;
    {load($sym);}.(atomp(sym)) : {
      char * require_file=sformat("%s.mc",strip_quotes(atom_string(sym)));
      mapc(eval_from_load,file_expressions(require_file));
      return `{${int_exp(cellcount)}: $sym provided};};
    {$any}:{
      return simple_eval(exp);}}
}

void write_signature(expptr sym){
  expptr sig =getprop(sym,`{signature},NULL);
  if(!sig)berror(sformat("MCbug: %s has no signature", atom_string(sym)));
  ucase{sig;
    {$type $x[$dim];}:{pprint(`{${type} * ${x};},fileout,0);};
    {$any}:{pprint(sig,fileout,0);}}
}
    
expptr eval_internal(expptr forms){ // forms must be fully macro expanded.
  compilecount++; //this needs to be here becasue sformat duplicates the second argument
  char * s = sformat("/tmp/TEMP%d.c",compilecount);
  fileout = fopen(s, "w");
  fprintf(fileout,"#include \"%spremacros.h\"\n", MetaC_directory);

  new_body_procedures = nil;
  new_sig_procedures = nil;
  new_arrays = nil;
  doit_statements = nil;
  new_preambles = nil;

  mapc(preinstall,forms);
  
  mapc(print_preamble,reverse(file_preamble));
  mapc(print_preamble,reverse(new_preambles));
  fputc('\n',fileout);
  pprint(`{void * * symbol_value_copy;},fileout,0);

  mapc(write_signature, procedures);
  mapc(write_signature, new_sig_procedures);
  mapc(write_signature, arrays);
  mapc(write_signature, new_arrays);

  //procedure value extractions.  array extractions are done in doit.

  mapc(install_link_def,procedures);
  mapc(install_link_def,new_sig_procedures);
  mapc(install_proc_def,new_body_procedures);

  pprint(`{
      expptr _mc_doit(voidptr * symbol_value){
	symbol_value_copy = symbol_value;
	${mapcar(array_extraction, arrays)} // procedure extractions are done by install_link_def above
	${mapcar(new_procedure_insertion, new_body_procedures)}
	${mapcar(new_array_insertion, new_arrays)}
	${mapcar(array_extraction, new_arrays)} // procedure extractions are done by install_link_def above
	${reverse(doit_statements)}
	return string_atom("done");}},
    fileout,0);
  fclose(fileout);
  
  void * header = compile_load_file(sformat("/tmp/TEMP%d",compilecount));

  expptr (* _mc_doit)(voidptr *);
  _mc_doit = dlsym(header,"_mc_doit");

  if(in_ide)send_emacs_tag(ignore_tag); // this cleans output from call to dlsym.
  
  in_doit = 1;
  expptr e = (*_mc_doit)(symbol_value);
  
  mapc(install_preamble,reverse(new_preambles));
  mapc(install_array,reverse(new_arrays));
  mapc(install_procedure,reverse(new_sig_procedures));
  if(!e)e = `{};
  return `{${int_exp(++cellcount)}: $e};
}

expptr left_atom(expptr e){
  if(atomp(e)) return e;
  if(cellp(e)) return left_atom(car(e));
  return NULL;}

void preinstall(expptr statement){
  if(left_atom(statement) == `typedef){
    {if(!getprop(statement,`installed,NULL)) push(statement, new_preambles);};}
  else
    ucase{statement;
      {}:{};
      {#include <$any>}:{push(statement, new_preambles);};
      {return $e;}:{push(statement,doit_statements);};
      {$type $X[0];}.(symbolp(type) && symbolp(X)):{preinstall_array(type,X,`{1});};
      {$type $X[0] = $e;}.(symbolp(type) && symbolp(X)):{preinstall_array(type,X,`{1}); push(`{$X[0] = $e;},doit_statements);};
      {$type $X[$dim];}.(symbolp(type) && symbolp(X)):{preinstall_array(type,X,dim);};
      {$type $f($args){$body}}.(symbolp(type) && symbolp(f)):{preinstall_proc(type, f, args, body);};
      {$type $f($args);}.(symbolp(type) && symbolp(f)):{preinstall_proc(type, f, args, NULL);};
      {$e;}:{push(statement,doit_statements)};
      {{$e}}:{push(statement,doit_statements)};
      {$any}:{push(`{return $statement;},doit_statements)}}
}

void print_preamble(expptr e){
  ucase{e;
    {#include <$f>}:{fprintf(fileout,"#include <%s>\n", exp_string(f));};
    {$any}:{pprint(e,fileout,rep_column);}}
}

void check_signature(expptr sym, expptr sig){
  expptr oldsig = getprop(sym,`{signature},NULL);
  if(sig != oldsig){
    berror(sformat("attempt to change signature of %s from\n %s\n to\n %s\n",
		   atom_string(sym),
		   exp_string(oldsig),
		   exp_string(sig)));}
}

void preinstall_array(expptr type, expptr X, expptr dim){
  expptr sig = `{$type $X[$dim];};
  if(getprop_int(X,`{installed},0) == 0){
    setprop(X,`{signature},sig);
    push(X,new_arrays);}
  else
    check_signature(X, sig);
}

void preinstall_proc(expptr type,  expptr f, expptr args, expptr body){
  expptr sig = `{$type $f($args);};
  if(getprop_int(f,`{installed},0) == 0){
    setprop(f,`{signature},sig);
    push(f,new_sig_procedures);}
  else{
    check_signature(f, sig);}
  if(body){
    if (getprop(f,`{base},NULL))berror(sformat("attempt to change base function %s",atom_string(f)));
    push(f, new_body_procedures);
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


void install_proc_def(expptr f){
  ucase{getprop(f,`{signature},NULL);
    {$type $f($args);}:{
      pprint(
	     `{$type ${getprop(f,`{gensym_name},NULL)}($args){
		 ${getprop(f,`{body},NULL)}}},
	     fileout, 0);};
    {$e}:{}}
}

void comp_error(){
  if(in_ide)
    send_emacs_tag(comp_error_tag);
  else
    fprintf(stdout,"\n evaluation aborted\n\n");
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

void NIDE(){
  send_emacs_tag(continue_from_gdb_tag);
  throw_error();
}
