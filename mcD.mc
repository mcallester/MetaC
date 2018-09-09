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

umacro{eformat($args)}{
  return `{({
	int needed_size = snprintf(NULL,0,$args);
	if(needed_size >= EPHEMERAL_DIM)berror("ephemeral buffer overflow");
	sprintf(ephemeral_buffer,$args);
	ephemeral_buffer;})};
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

int repl_symbol(expptr exp){
  ucase{exp;
    {$sym[0]}.(symbolp(sym)):{return 1;}
    {$any}:{return 0;}
  }
  return 0; //dead
}

// $sym should already be declared expptr and will be set by the code generated
umacro{exp_from_undo_frame($sym,{$code})}{
  if (!symbolp(sym) && !repl_symbol(sym)){
    uerror(`{First argument to exp_from_undo_frame must be a symbol: $sym});}
  expptr gensym_temp=gensym("temp");

  return `{
    push_undo_frame();
    {$code}
    push_stack_frame();
    //transfer exp value of $sym to stack
    expptr $gensym_temp = stack_copy_exp($sym);
    pop_undo_frame();
    //intern exp value from stack and set $sym
    $sym = intern_from_stack($gensym_temp);
    pop_stack_frame();
  };
}



init_fun(mcD_init)
