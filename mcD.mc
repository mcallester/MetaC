#include "mc.h"

umacro{push($x,$y)}{
  return `{$y = cons($x,$y);};
}

umacro{dolist($x,$y){$body}}{
  //we need make "break" and "continue" work form inside iteration macros.
  expptr yval = gensym("yval");
  return `{{
      expptr $x;
      for(expptr $yval = $y; cellp($yval); $yval = cdr($yval)){
	$x = car($yval);
	$body}}};
}

/** ========================================================================
sformat is like sprintf but stack-allocates the buffer rather than take a buffer argument.
It returns the string pointer.

warning --- this replicates the arguments and hence the arguments should be variables.
this is hard to fix because the argument types are very difficult to determine.
========================================================================**/

umacro{sformat($args)}{
  return `{({
	int needed_size = snprintf(NULL,0,$args);
	char * buffer = (char *) stack_alloc(needed_size+1);
	sprintf(buffer,$args);
	buffer;})};
}

expptr args_variables(expptr args){
  if(args == nil)return nil;
  ucase{args;
    {$type1 $var($type2), $rest}:{return cons(cons(var,comma), args_variables(rest));}
    {$type1 $var($type2)}:{return cons(var, nil);}
    {$type $var, $rest}:{return cons(cons(var,comma), args_variables(rest));}
    {$type $var}:{return cons(var, nil);}}
  return nil;
}

init_fun(mcD_init)

/** ========================================================================
stack frames for the debugger;

void push_dbg_expression(expptr e){
  if(dbg_freeptr == DBG_DIM)berror("debugging stack exhausted");
  dbg_stack[dbg_freeptr++] = e;
}

void pop_dbg_stack(){
  if(dbg_freeptr == 0)berror("attempt to pop empty dbg stack ");
  dbg_freeptr--;
}

This code would be better written using the gnu extension for expression statements
supporting functional programming.

There should be mapexp macro for code walking.

expptr args_variables(expptr args);
expptr args_assignments(expptr args);

umacro{sframe $type $f($args){$body} }{
  //this should have an unwind protect
  if(type == `{void}){
    return `{$type $f($args){
	push_dbg_expression(`{$f($args)});
	${args_assignments(args_variables(args))}
	$body;
	pop_dbg_stack();}};};
  expptr var = gensym(`{result});
  return `{$type $f($args){
c      push_dbg_expression(`{$f($args)});
      ${args_assignments(args_variables(args))}
      $type $var;
      catch_return($var $body)
      pop_dbg_stack();
      return $var;
    }}}

umacro{catch_return{$var $statement}}{
  expptr donelabel = gensym("done");
  return `{{
      ${replace_returns(var,donelabel,statement)}
      $donelabel:}}
}

expptr replace_returns(expptr var, expptr donelabel, expptr s){
  ucase{s;
    {return $e ;}:{
      return `{$var = $e; goto $donelabel;}}
    {$x}:{
      if(atomp(s)) return s;
      return intern_exp(constructor(s),
			replace_returns(var, donelabel, arg1(s)),
			replace_returns(var, donelabel, arg2(s)));}}
  return NULL;
}

expptr args_assignments(expptr args){
  if(args == NULL)return NULL;
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

======================================================================== **/



