#include "mc.h"

/** ========================================================================
utilities
========================================================================**/
void expptr_error(expptr x, char* s){
  berror(sformat("%s %s",exp_string(x),s));
  }

void expptr_breakpt(expptr x, char* s){
  breakpt(sformat("%s %s", exp_string(x), s));
  }

expptr combine_atoms(expptr a1, expptr a2){
  char* s1 = atom_string(a1);
  char* s2 = atom_string(a2);
  return string_atom(sformat("%s_%s",s1,s2));
  }

explist file_preamble; // must avoid name clash with preamble in mcA.c
explist procedures;
explist arrays;

explist new_body_procedures;
explist new_sig_procedures;
explist new_arrays;
explist doit_statements;
explist new_preambles;
explist current_forms;

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
charptr strip_quotes(charptr);
expptr eval_internal(explist);
void print_preamble(expptr);

void install_preamble(expptr e);
void install_array(expptr e);
void install_procedure(expptr e);

int symbol_index_freeptr = 0;

int symbol_index(expptr sym){
  int i = getprop_int(sym,`symbol_index,-1);
  if(i >= 0)return i;
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
  if(in_ide)breakpt("install values breakpt");
  explist_do(f,procedures){install_value(f);}
  explist_do(X,arrays){install_value(X);}
  }

void NIDE_init(){
  file_preamble = NULL;  // must be careful to avoid name clash with preamble used by add_preamble in mcA.c
  add_undone_pointer((void**) &file_preamble);
  arrays = NULL;
  add_undone_pointer((void**) &arrays);
  procedures = NULL;
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

In this implementation all global data variables must be arrays.  We
can replace a declaration

<type>  x;

by

<type> x[1];

and replace x by x[0] throughout the code.

An array variable x can be dynamically linked into the DLL code in
such a way that assignments to x[i] in the DLL code do the right
thing.  This is not true of assignments to x as opposed to assignments
to x[i]. To support assignment to x (as opposed to x[i]) we need to be
able to identify the FREE occurances of x in the code. This
implementation does not support that level of analysis of the C code.

An important design convention is that evaluation of a NIDE cell not
have any effect in the event of any error prior to the execution of
the compiled do_it procedure.  In particular errors during macro
expansion or compilation should block all effects.  This can be
achieved by performing all effects with the add_form construct so that
the effect is done from inside do_it.
======================================================================== **/

expptr new_procedure_insertion (expptr f){
  return `{
    undo_set(symbol_value[${symbol_index_exp(f)}],
	     ${getprop(f,`{gensym_name},NULL)});};
}

expptr new_array_insertion (expptr x){
  ucase(getprop(x,`{signature},NULL)){
    {$type $x[$dim];}:{return `{undo_set(symbol_value[${symbol_index_exp(x)}],
					 undo_alloc($dim*sizeof($type)));};};};
  return NULL; //avoids compiler warning
  }

expptr array_extraction (expptr x){
  if(occurs_in_explist(x,current_forms)) return `{$x = symbol_value[${symbol_index_exp(x)}];};
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
  if(!args)return NULL;
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
     occurs_in_explist(f,current_forms)){
    
    install_link_def(f);}
  }

/** ========================================================================
eval_exp

Macros expanded in the NIDE can use add_form exclusively in place of
add_preamble or add_init_form.  Add-form adds to the
preambles. However, macros defined in pre-Nide bootstrapping must
separate add_preamble from add_init_form.  Therefore the NIDE must
handle init_forms as well as preambles.  However, in the NIDE the
distinction between a preamble and a do_it_statement is determined by
examining the statement and preambles and init_forms are treated
equivalently.
======================================================================== **/
expptr simple_eval(explist exps);
void load_check(expptr e);

expptr eval_exp(expptr exp){
  ucase(exp){
    {load($sym);}.(atomp(sym)) : {
      char * fname=sformat("%s.mc",strip_quotes(atom_string(sym)));
      explist forms =file_expressions(fname);
      explist_do(e,forms){load_check(e);}
      simple_eval(forms);
      return `{${int_exp(cellcount)}: $sym provided};};
    {$any}:{
      return simple_eval(expcons(exp,NULL));};}
  }

expptr simple_eval(explist exps){
  preamble = NULL;  //this is the preamble of add_premble in mcA.c
  init_forms = NULL;
  explist_do(e,exps){
    expptr e2 = macroexpand(e);
    preamble = expcons(e2,explist_append(init_forms,preamble));}
  return eval_internal(explist_reverse(preamble));
  }

void load_check(expptr e){
  ucase(e){
    {restart_undo_frame($any);}:{
      fprintf(stdout,"loaded file contains an undo restart");
      if(in_ide)send_emacs_tag(comp_error_tag);
      throw_NIDE(NULL);};
    {load($sym);}.(atomp(sym)) : {
      fprintf(stdout,"recursive load is not yet supported");
      if(in_ide)send_emacs_tag(comp_error_tag);
      throw_NIDE(NULL);};
    {$any}:{return;};}
  }


/** ========================================================================
eval_internal
========================================================================**/

void write_signature(expptr sym);
void write_signature_sparsely(expptr sym);
expptr explist_exp(explist l);

expptr eval_internal(explist forms){
  // forms must be fully macro expanded
  // forms must be in the desired order
  compilecount++; //sformat duplicates the second argument
  char * s = sformat("/tmp/TEMP%d.c",compilecount);
  fileout = fopen(s, "w");
  fprintf(fileout,"#include \"%spremacros.h\"\n", MetaC_directory);
  
  new_body_procedures = NULL;
  new_sig_procedures = NULL;
  new_arrays = NULL;
  doit_statements = NULL;
  new_preambles = NULL;
  current_forms = forms;
  in_doit = 0;
  
  explist_mapc(preinstall,forms);
  
  explist_mapc(print_preamble,explist_reverse(file_preamble));
  fputc('\n',fileout);
  explist_mapc(print_preamble,explist_reverse(new_preambles));
  fputc('\n',fileout);
  pprint(`{void * * symbol_value_copy;},fileout);
  fputc('\n',fileout);
  explist_mapc(write_signature_sparsely, procedures);
  fputc('\n',fileout);
  explist_mapc(write_signature, new_sig_procedures);
  fputc('\n',fileout);
  explist_mapc(write_signature_sparsely, arrays);
  fputc('\n',fileout);
  explist_mapc(write_signature, new_arrays);
  fputc('\n',fileout);
  
  //procedure value extractions.  array extractions are done in doit.
  
  explist_mapc(install_link_def_sparsely,procedures);
  fputc('\n',fileout);
  explist_mapc(install_link_def,new_sig_procedures);
  fputc('\n',fileout);
  explist_mapc(install_proc_def,new_body_procedures);
  fputc('\n',fileout);
  
  pprint(`{
	   expptr _mc_doit(voidptr * symbol_value){
	     symbol_value_copy = symbol_value;
	     ${explist_exp(explist_mapcar(array_extraction, arrays))}
	     // procedure extractions are done by install_link_def above
	     ${explist_exp(explist_mapcar(new_procedure_insertion,
					  new_body_procedures))}
	     ${explist_exp(explist_mapcar(new_array_insertion, new_arrays))}
	     ${explist_exp(explist_mapcar(array_extraction, new_arrays))}
	     // procedure extractions are done by install_link_def above
	     ${explist_exp(explist_reverse(doit_statements))}
	     return string_atom("done");}},
	 fileout);
  fclose(fileout);
  
  void * header = compile_load_file(sformat("/tmp/TEMP%d",compilecount));
  
  expptr (* _mc_doit)(voidptr *);
  _mc_doit = dlsym(header,"_mc_doit");
  
  if(in_ide)send_emacs_tag(ignore_tag); // this cleans output from call to dlsym.
  
  in_doit = 1;
  expptr e = (*_mc_doit)(symbol_value);
  
  explist_mapc(install_preamble,explist_reverse(new_preambles));
  explist_mapc(install_array,explist_reverse(new_arrays));
  explist_mapc(install_procedure,explist_reverse(new_sig_procedures));
  if(!e)e = `{};
  return `{{${int_exp(++cellcount)}; $e}};
  }

void write_signature(expptr sym){
  expptr sig =getprop(sym,`{signature},NULL);
  if(!sig)berror(sformat("MCbug: %s has no signature", atom_string(sym)));
  ucase(sig){
    {$type $x[$dim];}:{pprint(`{${type} * ${x};},fileout);};
    {$any}:{pprint(sig,fileout);};}
  }

void write_signature_sparsely(expptr sym){
  //some symbols can be introduced by C preprocessing and always included
  if(sym == `berror ||
     sym == `undo_set_proc ||
     sym == `string_atom ||
     occurs_in_explist(sym, current_forms)){
    write_signature(sym);}
  }

expptr explist_exp(explist l){
  if(!l)return NULL;
  return mkspace(l->first, explist_exp(l->rest));
  }

expptr left_atom(expptr e){
  if(atomp(e)) return e;
  if(cellp(e)) return left_atom(car(e));
  return NULL;}

void preinstall(expptr statement){
  expptr leftexp = left_atom(statement);
  if(leftexp == `typedef){
    if(!getprop(statement,`installed,NULL)){
      explist_pushnew(statement, new_preambles);}}
  else
  ucase(statement){
    {}:{};
    {#include $any}:{explist_pushnew(statement, new_preambles);};
    {#define $any}:{explist_pushnew(statement, new_preambles);};
    {return $e;}:{explist_pushnew(statement,doit_statements);};
    {$type $X[0];}.(symbolp(type) && symbolp(X)):{
      preinstall_array(type,X,`{1});};
    {$type $X[0] = $e;}.(symbolp(type) && symbolp(X)):{
      preinstall_array(type,X,`{1});
      explist_pushnew(`{$X[0] = $e;},doit_statements);};
    {$type $X[$dim];}.(symbolp(type) && symbolp(X)):{
      preinstall_array(type,X,dim);};
    {$type $f($args){$body}}.(symbolp(type) && symbolp(f)):{
      preinstall_proc(type, f, args, body);};
    {$type $f($args);}.(symbolp(type) && symbolp(f)):{
      preinstall_proc(type, f, args, NULL);};
    {$e;}:{explist_pushnew(statement,doit_statements)};
    {{$e}}:{explist_pushnew(statement,doit_statements)};
    {$any}:{explist_pushnew(`{return $statement;},doit_statements)};}
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
    explist_pushnew(X,new_arrays);}
  else
    check_signature(X, sig);
}

void preinstall_proc(expptr type,  expptr f, expptr args, expptr body){
  if(getprop(f,`macro,NULL))berror("attempt to redefine macro as procedure");
  expptr sig = `{$type $f($args);};
  if(getprop_int(f,`{installed},0) == 0){
    setprop(f,`{signature},sig);
    explist_pushnew(f,new_sig_procedures);}
  else{
    check_signature(f, sig);}
  if(body){
    if(getprop(f,`base,NULL)){
      berror(sformat("attempt to change base function %s",atom_string(f)));}
    explist_pushnew(f, new_body_procedures);
    setprop(f,`gensym_name,gensym(f));
    setprop(f,`body,body);
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
  {send_emacs_tag(comp_error_tag);}
  else
  {fprintf(stdout,"\n evaluation aborted\n\n");}
  throw_NIDE(NULL);
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
  throw_NIDE(NULL);
  }


