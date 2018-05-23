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

#include "mc.h"

umacro{push(!x,?y)}{
  return `{${y} = cons(${x},${y});};
}

umacro{dolist(?x,!y){!body}}{
  expptr yval = gensym(`{yval});
  return `{{
      expptr ${x};
      expptr ${yval} = ${y};
      while(${yval} != NULL){
	${x} = car(${yval});
	${body}
	${yval} = cdr(${yval});}}};
}

umacro{mapc(!f,!list)}{
  expptr x = gensym(`{x});
  return `{dolist(${x}, ${list}){${f}(${x});}};
}
      
/** ========================================================================
file_expressions  This returns a list of macro-expanded expressions in the order they appear
in the file with each expression preceeded by its preamble and followed by its init_forms.
========================================================================**/

expptr file_expressions2();
expptr full_expansion(expptr);

expptr file_expressions(expptr fname){
  open_input_file(exp_string(fname));
  expptr result = file_expressions2();
  fclose(filein);
  return result;
}

expptr file_expressions2(){
  if(readchar == EOF)return NULL;
  expptr e = read_from_file();
  expptr rest = file_expressions2();
  if(e == NULL)return rest;
  return append(full_expansion(e),rest);
}

expptr full_expansion(expptr e){
  preamble = NULL;
  init_forms = NULL;
  expptr e2 = macroexpand(e);
  return append(preamble,cons(e2,init_forms));
}

/** ========================================================================
sformat is like sprintf but stack-allocates the buffer rather than take a buffer argument.
It returns the string pointer.

warning --- this replicates the arguments and hence the arguments should be variables.
this is hard to fix because the argument types are very difficult to determine.
========================================================================**/

umacro{sformat(!args)}{
  return `{({
	int needed_size = snprintf(NULL,0,${args});
	char * buffer = (char *) stack_alloc(needed_size+1);
	sprintf(buffer,${args});
	buffer;})};
}

/** ========================================================================
stack frames for the debugger;
======================================================================== **/

expptr args_variables(expptr args);
expptr args_assignments(expptr args);

umacro{sframe{?type ?f(!args){!body}}}{
  if(type == `{void}){
    return `{${type} ${f}(${args}){
	push_dbg_expression(`{${f}(${args})});
	${args_assignments(args_variables(args))}
	${body};
	pop_dbg_stack();}};}
  expptr var = gensym(`{result});
  return `{${type} ${f}(${args}){
      push_dbg_expression(`{${f}(${args})});
      ${args_assignments(args_variables(args))}
      ${type} ${var};
      catch_return(${var} ${body})
      pop_dbg_stack();
      return ${var};
    }}}

umacro{catch_return{?var !statement}}{
  expptr donelabel = gensym(`{done});
  return `{{
      ${replace_returns(var,donelabel,statement)}
      ${donelabel}:}}
}

expptr replace_returns(expptr var, expptr donelabel, expptr s){
  ucase{s;
    {return !e ;}:{
      return `{${var} = ${e}; goto ${donelabel};}}
    {!x}:{
      if(atomp(s)) return s;
      return intern_exp(constructor(s),
			replace_returns(var, donelabel, arg1(s)),
			replace_returns(var, donelabel, arg2(s)));}}
  return NULL;
}

expptr args_variables(expptr args){
  ucase{args;
    {?type1 ?var(?type2), !rest}:{return cons(var, args_variables(rest))}
    {?type1 ?var(?type2)}:{return cons(var, NULL);}
    {?type ?var, !rest}:{return `{${var}, ${args_variables(rest)}};}
    {?type ?var}:{return cons(var, NULL);}}
  return NULL;
}

expptr args_assignments(expptr args){
  if(length(args) > DBG_DIM_ARGS)berror("sframe with too many arguments");
  int i = 0;
  expptr result = NULL;
  while(args != NULL){
    expptr arg = car(args);
    result = cons(`{dbg_stack_args[dgb_freeptr,int_exp[i]] = ${car(args)};},result);
    i++;
    args = cdr(args);}
  return result;
}

init_fun(mcD_init)