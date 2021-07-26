#include "mc.h"

umacro{push($x,$y)}{
  return `{$y = cons($x,$y);};
}

umacro{undo_push($x,$y)}{
  return `{undo_set($y,cons($x,$y));};
}

umacro{dolist($x,$y){$body}}{
  //we need make "break" and "continue" work from inside iteration macros.
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

umacro{mcprint($args)}{
  return
    `{{if(in_ide_proc()){
	fprintf(stdout,$args); send_print_tag();}
      else fprintf(stdout,$args);}};
}


umacro{in_memory_frame{$body}}{
  return
    `{unwind_protect({
	push_memory_frame();
	$body; //can throw an error
	pop_memory_frame();},
      {pop_memory_frame();})};
}

umacro{exp_from_undo_frame($exp)}{
  expptr expvar = gensym("expvar");
  expptr stackexp = gensym("stack_exp");
  expptr newexp = gensym("new_exp");
  return
    `{({expptr $newexp;
	unwind_protect({
	    push_undo_frame();
	    expptr $expvar = $exp; //can throw an error
	    push_memory_frame();
	    expptr $stackexp = expptr_to_stack($expvar); //assumed safe
	    pop_undo_frame();
	    $newexp = expptr_to_undo($stackexp); //assumed safe
	    pop_memory_frame();},
	  {pop_undo_frame();});
	$newexp;})};
}

umacro{int_from_undo_frame($exp)}{
  expptr expvar = gensym("expvar");
  return
    `{({int $expvar;
	unwind_protect({
	    push_undo_frame();
	    $expvar = $exp; //can throw an error
	    pop_undo_frame();},
	  {pop_undo_frame();});
	$expvar;})};
}
/** 152:done **/

umacro{orcase{$valexp;{$firstpattern}{$secondpattern}:{$body}}}{
  return `{ucase{$valexp;{$firstpattern}:{$body}; {$secondpattern}:{$body}}};
}

init_fun(mcD_init)

