#include "mc.h"

int undostack_freeptr;

void restart_event(expptr name){
  int n = getprop_int(name,`undo_freeptr,-1);
  if(n < 0){
    push_undo_frame();
    setprop_int(name,`undo_freeptr,undostack_freeptr);
    return;}
  while(undostack_freeptr > n+1){pop_undo_frame();}
  clear_undo_frame();
}

/** ========================================================================
versions of catch.
========================================================================**/

int occurs_in_exp(expptr symbol, expptr exp){
  if(!exp)return 0;
  if(atomp(exp))return (symbol == exp);
  ucase(exp){
    {$e->$any}.(symbolp(any)):{return occurs_in_exp(symbol,e);};
    {$any}:{
      if(cellp(exp))
      return (occurs_in_exp(symbol,car(exp)) || occurs_in_exp(symbol,cdr(exp)));
      else
      return occurs_in_exp(symbol,paren_inside(exp));};}
  }

int occurs_in_explist(expptr symbol, explist lst){
  if(!lst)return 0;
  return occurs_in_exp(symbol,lst->first) || occurs_in_explist(symbol,lst->rest);
  }

umacro{catch_all{$body1}{$body2}}{
  if(occurs_in_exp(`return,body1) ||
     occurs_in_exp(`break,body1) ||
     occurs_in_exp(`continue,body1)){
    berror("occurance of return, continue or break in catch body");}
    return `{{
      if(catch_freeptr[0] == CATCH_DIM)berror("catch stack exhausted");
      if(setjmp(catch_stack[catch_freeptr[0]++]) == 0){
	$body1;catch_freeptr[0]--;}
      else {
	catch_freeptr[0]--;
	$body2}
      }};
  }

umacro{unwind_protect{$body1}{$body2}}{
  return `{catch_all{$body1}{$body2; throw_primitive();}};
  }

umacro{declare_exception($name($argtype))}{
  add_init_form(`{declare_except_fun(`{$name},`{$argtype});});
  return `{};
  }

void declare_except_fun(expptr name, expptr argtype){
  expptr oldtype = getprop(name,`exception_argtype,NULL);
  if(oldtype && (argtype != oldtype))berror("attempt to change the argument type of exception");
  setprop(name,`exception_argtype,argtype);
  }

umacro{throw($name($value))}{
  expptr argtype = getprop(name,`exception_argtype,`none);
  if(argtype == `none)berror("undeclared exception");
  
  if(!argtype){
    if(value)berror("ill typed throw");
    return `{{
	catch_name[0] = `{$name};
	throw_primitive();}};}
  
  if(!value)berror("ill typed throw");
  
  expptr valvar = gensym(`val);
  return `{{
      $argtype $valvar = $value;
      catch_name[0] = `{$name};
      catch_val[0] = $valvar;
      throw_primitive();
      }};
  }
  
umacro{catch($name($arg)){$body1}{$body2}}{
  expptr argtype = getprop(name,`exception_argtype,`none);
  if(argtype == `none)berror("undeclared exception");
  
  if(!argtype){
    if(arg)berror("ill-typed catch");
    return `{
      catch_all{
	$body1}
      {if(catch_name[0] == `{$name}){
	  $body2
	  } else {
	  throw_primitive();}}};}
  
  if(!arg)berror("ill-typed catch");
  expptr valvar = gensym(`val);
  return `{{
      catch_all{
	$body1}
      {if(catch_name[0] == `{$name}){
	  $argtype $arg = ($argtype) catch_val[0];
	  $body2
	  } else {
	  throw_primitive();}}}};
  }
  
/** ========================================================================
primitive list operations --- depricated in favor of deflists in objects.mc
========================================================================**/

umacro{explist_push($x,$y)}{
  return `{undo_set($y,expcons($x,$y));};
  }

umacro{explist_pushnew($x,$y)}{
  expptr yvar = gensym(y);
  return `{
    explist $yvar = $y;
    if(!explist_member($x,$yvar)){undo_set($y,expcons($x,$yvar));};};
  }

umacro{explist_do($x,$y){$body}}{
  //we need make "break" and "continue" work from inside iteration macros.
  expptr yval = gensym(`yval);
  return `{{
      expptr $x;
      for(explist $yval = $y; $yval; $yval = $yval->rest){
	$x = $yval->first;
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


umacro{mcprint($args)}{
  return
  `{{if(in_ide_proc()){
	fprintf(stdout,$args); send_print_tag();}
      else fprintf(stdout,$args);}};
}

init_fun(mcD_init)

