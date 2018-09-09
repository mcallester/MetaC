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

umacro{mcprint($args)}{
  return
    `{{if(in_ide_proc()){
	fprintf(stdout,$args); send_print_tag();}
      else fprintf(stdout,$args);}};
}

init_fun(mcD_init)
