#include "mc.h"

void expptr_error(expptr x, char* s){
  berror(sformat("%s %s",exp_string(x),s));
  }

void expptr_breakpt(expptr x, char* s){
  breakpt(sformat("%s %s", exp_string(x), s));
  }

/** ========================================================================
Stack discipline for stack memory and undo frames.
========================================================================**/

umacro{in_memory_frame{$body}}{
  return
  `{unwind_protect{
      push_memory_frame();
      $body;
      pop_memory_frame();
      }{
      {pop_memory_frame();}}};
  }

umacro{in_undo_frame{$body}}{
  return
  `{unwind_protect{
      push_undo_frame();
      $body;
      pop_undo_frame();
      }{
      {pop_undo_frame();}}};
  }

umacro{int_from_undo_frame($exp)}{
  expptr expvar = gensym(`expvar);
  expptr stackexp = gensym(`stack_exp);
  expptr result = gensym(`result);
  return
  `{({
       int $result;
       unwind_protect{
	 push_undo_frame();
	 expptr $result = $exp; //unsafe.
	 pop_undo_frame();
	 }{
	 pop_undo_frame();}
       $result;
       })};
  }

umacro{exp_from_undo_frame($exp)}{
  expptr expvar = gensym(`expvar);
  expptr stackexp = gensym(`stack_exp);
  expptr newexp = gensym(`new_exp);
  return
  `{({
       expptr $newexp;
       unwind_protect{
	 push_undo_frame();
	 expptr $expvar = $exp; //unsafe.  The rest is safe which is required for proper stack memory management.
	 push_memory_frame();
	 expptr $stackexp = expptr_to_stack($expvar);
	 pop_undo_frame();
	 $newexp = expptr_to_undo($stackexp);
	 pop_memory_frame();
	 }{
	 pop_undo_frame();}
       $newexp;
       })};
  }

init_fun(mcD2_init);
