#include "mc.h"

umacro{catch{$body1}{$body2}}{ //body1 must not contain "return" or nonlocal "continue" or "break".  This should get fixed.
  return `{{
      catch_name[0] = NULL;
      if(catch_freeptr[0] == CATCH_DIM)berror("catch stack exhausted");
      
      if(setjmp(catch_stack[catch_freeptr[0]++]) == 0){
	{$body1} catch_freeptr[0]--;
	} else {
	$body2}
      }};
  }

umacro{unwind_protect{$body1}{$body2}}{
  return `{catch{$body1}{{$body2} throw();}};
  }

umacro{stop_throw{$body}}{
  return `{catch{$body}{}};
  }

umacro{throw_value($name($value))}{
  return `{{
    catch_name[0] = `{$name};
    catch_val[0] = $value;
    throw();}};
  }

umacro{catch_value($name($val)){$body1}{$body2}}{
  return `{
    catch{$body1}{
      if(catch_name[0] == `{$name}){
	expptr $val = catch_val[0];
	$body2
	} else {
	throw();}}};
  }
  
/** ========================================================================
primitive list operations --- depricated in favor of deflists in objects.mc
========================================================================**/

umacro{push($x,$y)}{
  return `{undo_set($y,cons($x,$y));};
}

umacro{dolist($x,$y){$body}}{
  //we need make "break" and "continue" work from inside iteration macros.
  expptr yval = gensym(`yval);
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

umacro{mcprint($args)}{
  return
    `{{if(in_ide_proc()){
	fprintf(stdout,$args); send_print_tag();}
      else fprintf(stdout,$args);}};
}


umacro{orcase{$valexp;{$firstpattern}{$secondpattern}:{$body}}}{
  return `{ucase{$valexp;{$firstpattern}:{$body}; {$secondpattern}:{$body}}};
}

init_fun(mcD_init)

