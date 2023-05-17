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

An important design convention is that evaluation of a NIDE cell not have any
effect in the event of any error prior to the execution of the compiled do_it procedure.
In particular errors during macro expansion or compilation should block all effects.
This can be achieved by performing all effects with the add_form construct so that
the effect is done from inside do_it.
======================================================================== **/

int occurs_in(expptr symbol, expptr exp){
  if(atomp(exp))return (symbol == exp);
  ucase(exp){
    {$e->$any}.(symbolp(any)):{return occurs_in(symbol,e);};
    {$any}:{
      if(cellp(exp))
      return (occurs_in(symbol,car(exp)) || occurs_in(symbol,cdr(exp)));
      else
      return occurs_in(symbol,paren_inside(exp));};}
  }

expptr file_preamble; // must be careful to avoid name clash with preamble used by add_preamble in mcA.c
expptr procedures;
expptr arrays;

expptr new_body_procedures;
expptr new_sig_procedures;
expptr new_arrays;
expptr doit_statements;
expptr new_preambles;
expptr current_forms;

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

expptr args_variables(expptr args);
void preinstall(expptr sig);
voidptr compile_load_file(charptr fstring);
void preinstall_array(expptr,expptr,expptr);
void preinstall_proc(expptr,expptr,expptr,expptr);
void install_proc_def(expptr f);
void install_link_def(expptr f);
expptr symbol_index_exp(expptr);
void install_base();
char * strip_quotes(char *);
expptr eval_internal(expptr);
void print_preamble(expptr);

/** ========================================================================
install_base and install_value_properties, and install_values

install_value_properties must be a macro because each installation needs to be
a seperately compiled line of code.

install_base must be called before inatall_value_properties is macro expanded.
Hence intall_base must be called in ExpandE main and ExpandE must depend on
base_decls.h in the makefile.

ExpandE main is


int main(int argc, char **argv){
  ...
  install_base();
  in_expand = 1;
  catch_all{mcexpand(argv[1],argv[2]);}{return -1;};
  }

NIDE main is

int main(int argc, char **argv){
  ...
  install_value_properties();
  NIDE_init(); //this does install_base
  in_ide = 1;
  IDE_loop();
}

NIDE.mc is expanded by expandE.  The install_value_properties macro
in NIDE main will be expanded peoperly because
ExpandE has done inatall_base prior to expansion.  The resulting
installed procedure and array
pointers are the values they have in the NIDE executable.
======================================================================== **/


void install_base(){
  for(explist sigs = file_expressions(sformat("%sbase_decls.h", MetaC_directory));
      sigs;sigs=sigs->rest){
    expptr sig = sigs->first;
    ucase(sig){
      {$type $f($args);}.(symbolp(type) && symbolp(f)):{
	setprop(f,`{base},`{true});
	push(f,procedures);
	setprop(f,`{signature},sig);};
      {$type $x[$dim];}.(symbolp(type) && symbolp(x)):{
	push(x,arrays);
	setprop(x,`{signature},sig);};
      {$e}:{push(e,file_preamble);};}; //typedefs
    }
  }

umacro{install_value_properties()}{
  expptr result = `{};
  dolist(f,procedures){
    result = `{setprop(`{$f},`symbol_value,$f) ; $result};}
  dolist(X,arrays){
    result = `{setprop(`{$X},`symbol_value,$X) ; $result};}
  return result;
  }

int symbol_index_freeptr = 0;

int symbol_index(expptr sym){
  int i = getprop_int(sym,`symbol_index,-1);
  if(i >0)return i;
  i = symbol_index_freeptr++;
  setprop_int(sym,`symbol_index,i);
  return i;
  }

voidptr symbol_value[STRING_DIM] = {0};

void install_value(expptr f){
  expptr fval = getprop(f,`symbol_value,NULL);
  if(!fval)berror(sformat("%s failed to have its value installed",exp_string(f)));
  symbol_value[symbol_index(f)] = fval;
  }

void install_values(){
  dolist(f,procedures){install_value(f);}
  dolist(X,arrays){install_value(X);}
  }

void NIDE_init(){
  file_preamble = nil;  // must be careful to avoid name clash with preamble used by add_preamble in mcA.c
  add_undone_pointer((void**) &file_preamble);
  arrays = nil;
  add_undone_pointer((void**) &arrays);
  procedures = nil;
  add_undone_pointer((void**) &procedures);
  install_base();
  install_values();
  compilecount = 0;
  cellcount = 0;
  add_undone_int(&cellcount);
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
    undo_set(symbol_value[${symbol_index_exp(f)}],
	     ${getprop(f,`{gensym_name},NULL)});};
}

expptr new_array_insertion (expptr x){
  ucase(getprop(x,`{signature},nil)){
    {$type $x[$dim];}:{return `{undo_set(symbol_value[${symbol_index_exp(x)}],
					 undo_alloc($dim*sizeof($type)));};};};
  return nil; //avoids compiler warning
  }

expptr array_extraction (expptr x){
  if(occurs_in(x,current_forms)) return `{$x = symbol_value[${symbol_index_exp(x)}];};
  return `{};     
  }

expptr strip_type(expptr arg){
  ucase(arg){
    {$type1 $var($type2)}.(symbolp(var)):{return var;};
    {$type $var}.(symbolp(var)):{return var;};
    {$any}:{berror("illegal function signature --variable names must be provided");};};
  return NULL;
  }

expptr args_variables(expptr args){
  if(args == nil)return nil;
  ucase(args){
    {$first, $rest}:{return `{${strip_type(first)} , ${args_variables(rest)}};};
    {$any}:{return strip_type(args);};};
  return NULL;
  }

void install_link_def(expptr f){
  ucase(getprop(f,`{signature},NULL)){
    {$type $f($args);}:{
      pprint(
	     `{$type $f($args){
		 $type (* _mc_f)($args);
		 _mc_f = symbol_value_copy[${symbol_index_exp(f)}];
		 
		 if(!_mc_f){berror("call to undefined procedure");}
		 
		 ${(type == `{void} ?
		    `{(* _mc_f)(${args_variables(args)});}
		    : `{return (* _mc_f)(${args_variables(args)});})}}},
	     fileout);};
    {$any}:{};}
  }

void install_link_def_sparsely(expptr f){
  if(f == `berror ||
     f == `undo_set_proc ||
     f == `undo_set_int_proc ||
     f == `cbreak ||
     f == `string_atom ||
     f == `undo_alloc ||
     occurs_in(f,current_forms)){
    
    install_link_def(f);}
  }

/** ========================================================================
eval_exp
======================================================================== **/

expptr explist_exp(explist l){
  if(!l)return nil;
  return cons(l->first, explist_exp(l->rest));
  }

expptr simple_eval(expptr exp){
  preamble = NULL;  //this is the preamble of add_premble in mcA.c
  init_forms = NULL;
  in_doit = 0;
  expptr e = macroexpand(exp);
  return eval_internal(append(explist_exp(preamble),
			      append(explist_exp(init_forms),cons(e,nil))));
  }

void eval_from_load(expptr e){
  ucase(e){
    {restart_undo_frame($any);}:{
      fprintf(stdout,"loaded file contains an undo restart");
      if(in_ide)send_emacs_tag(comp_error_tag);
      throw_NIDE();};
    {load($sym);}.(atomp(sym)) : {
      fprintf(stdout,"recursive load is not yet supported");
      if(in_ide)send_emacs_tag(comp_error_tag);
      throw_NIDE();};
    {$any} : {simple_eval(e);};}
  }

expptr eval_exp(expptr exp){
  ucase(exp){
    {load($sym);}.(atomp(sym)) : {
      char * require_file=sformat("%s.mc",strip_quotes(atom_string(sym)));
      mapc(eval_from_load,explist_exp(file_expressions(require_file)));
      return `{${int_exp(cellcount)}: $sym provided};};
    {$any}:{
      return simple_eval(exp);};}
  }

void write_signature(expptr sym){
  expptr sig =getprop(sym,`{signature},NULL);
  if(!sig)berror(sformat("MCbug: %s has no signature", atom_string(sym)));
  ucase(sig){
    {$type $x[$dim];}:{pprint(`{${type} * ${x};},fileout);};
    {$any}:{pprint(sig,fileout);};}
  }

void write_signature_sparsely(expptr sym){
  if(sym == `berror || sym == `undo_set_proc || sym == `string_atom || occurs_in(sym, current_forms))write_signature(sym);
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
  current_forms = forms;
  
  mapc(preinstall,forms);
  
  mapc(print_preamble,reverse(file_preamble));
  fputc('\n',fileout);
  mapc(print_preamble,reverse(new_preambles));
  fputc('\n',fileout);
  pprint(`{void * * symbol_value_copy;},fileout);
  fputc('\n',fileout);
  mapc(write_signature_sparsely, procedures);
  fputc('\n',fileout);
  mapc(write_signature, new_sig_procedures);
  fputc('\n',fileout);
  mapc(write_signature_sparsely, arrays);
  fputc('\n',fileout);
  mapc(write_signature, new_arrays);
  fputc('\n',fileout);
  
  //procedure value extractions.  array extractions are done in doit.
  
  mapc(install_link_def_sparsely,procedures);
  fputc('\n',fileout);
  mapc(install_link_def,new_sig_procedures);
  fputc('\n',fileout);
  mapc(install_proc_def,new_body_procedures);
  fputc('\n',fileout);
  
  pprint(`{
	   expptr _mc_doit(voidptr * symbol_value){
	     symbol_value_copy = symbol_value;
	     ${mapcar(array_extraction, arrays)} // procedure extractions are done by install_link_def above
	     ${mapcar(new_procedure_insertion, new_body_procedures)}
	     ${mapcar(new_array_insertion, new_arrays)}
	     ${mapcar(array_extraction, new_arrays)} // procedure extractions are done by install_link_def above
	     ${reverse(doit_statements)}
	     return string_atom("done");}},
	 fileout);
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
  expptr leftexp = left_atom(statement);
  if(leftexp == `typedef)
  {if(!getprop(statement,`installed,NULL)) push(statement, new_preambles);}
  else
    ucase(statement){
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
      {$any}:{push(`{return $statement;},doit_statements)};}
}

void print_preamble(expptr e){
  ucase(e){
    {#include <$f>}:{fprintf(fileout,"#include <%s>\n", exp_string(f));};
    {$any}:{pprint(e,fileout);};}
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
  if(getprop(X,`macro,NULL))berror("attempt to redefine macro as array");
  expptr sig = `{$type $X[$dim];};
  if(getprop_int(X,`{installed},0) == 0){
    setprop(X,`{signature},sig);
    push(X,new_arrays);}
  else
    check_signature(X, sig);
}

void preinstall_proc(expptr type,  expptr f, expptr args, expptr body){
  if(getprop(f,`macro,NULL))berror("attempt to redefine macro as procedure");
  expptr sig = `{$type $f($args);};
  if(getprop_int(f,`{installed},0) == 0){
    setprop(f,`{signature},sig);
    push(f,new_sig_procedures);}
  else{
    check_signature(f, sig);}
  if(body){
    if (getprop(f,`{base},NULL))berror(sformat("attempt to change base function %s",atom_string(f)));
    push(f, new_body_procedures);
    setprop(f,`{gensym_name},gensym(f));
    setprop(f,`{body},body);
  }
}

expptr symbol_index_exp(expptr sym){
  return int_exp(symbol_index(sym));
}

void install_proc_def(expptr f){
  ucase(getprop(f,`{signature},NULL)){
    {$type $f($args);}:{
      pprint(
	     `{$type ${getprop(f,`{gensym_name},NULL)}($args){
		 ${getprop(f,`{body},NULL)}}},
	     fileout);};
    {$e}:{};}
}

void comp_error(){
  if(in_ide)
  send_emacs_tag(comp_error_tag);
  else
  fprintf(stdout,"\n evaluation aborted\n\n");
  throw_NIDE();
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
  voidptr header = dlopen(s3, RTLD_NOW | RTLD_DEEPBIND);
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
  throw_NIDE();
  }

declare_exception(NIDE());

init_fun(expandE_init)

