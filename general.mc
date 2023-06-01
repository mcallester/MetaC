
/** ========================================================================
Debugging and code maintenance is is eaier in the NIDE.

Therefore we want as much code as possible written in the NIDE rather than the
code required to bootstrap the NIDE.

So the following procedures, while very general, are written in the NIDE and
not placed in the NIDE kernel.

The efficiency of loading a file into the NIDE needsto be improved.
========================================================================**/

umacro{mention($x)}{
  return `{if($x ? $x : $x){}};}

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


/** ========================================================================
deflists: LIST operations on arbitrary types
========================================================================**/

umacro {declare_pointer($typename)}{
  return `{typedef struct ${combine_atoms(typename, `struct)} * $typename};
  }

void add_list_forms(expptr type){
  char * cstring = atom_string(type);
  expptr listtype = string_atom(sformat("%s_list",cstring));
  expptr structtype = string_atom(sformat("%s_list_struct",cstring));
  expptr consfun = string_atom(sformat("%s_cons",cstring));
  expptr iterator = string_atom(sformat("%s_iter",cstring));
  expptr mapper = string_atom(sformat("%s_map",cstring));
  expptr pusher = string_atom(sformat("push_%s",cstring));
  expptr listfun = string_atom(sformat("%s_listfun",cstring));
  expptr append = string_atom(sformat("%s_append",cstring));
  expptr nth = string_atom(sformat("%s_nth",cstring));
  expptr member = string_atom(sformat("%s_member",cstring));
  expptr delete = string_atom(sformat("%s_delete",cstring));
  
  add_form(`{
	     typedef struct $structtype{$type first; struct $structtype * rest;}$structtype, * $listtype;
	     });
  add_form(`{
	     $listtype $consfun($type first, $listtype rest){
	       $listtype cell = ($listtype) undo_alloc(sizeof($structtype));
	       cell->first = first;
	       cell->rest = rest;
	       return cell;}
	     });
  add_form(`{
	     $listtype $append($listtype x, $listtype y){
	       if(!x)return y;
	       return $consfun(x->first,$append(x->rest,y));}
	     });
  add_form(`{
	     $type $nth($listtype x, int n){
	       if(!x)berror("list too short in nth");
	       if(n == 1){return x->first;}
	       return $nth(x->rest,n-1);}
	     });
  add_form(`{
	     int $member($type x, $listtype y){
	       if(!y) return 0;
	       if(y->first == x)return 1;
	       return $member(x,y->rest);}
	     });
  add_form(`{
	     umacro{$pusher(\$x,\$y)}{
	       return `{undo_set(\$y,$consfun(\$x,\$y))};}
	     });
  add_form(`{
	     umacro{$iterator(\$x,\$y){\$body}}{
	       expptr yval = gensym(`yval);
	       return `{{
		   $type \$x;
		   for($listtype \$yval = \$y; \$yval; \$yval = \$yval ->rest){
		     \$x = \$yval->first;
		     \$body}
		   }};}
	     });
  add_form(`{
	     umacro{$mapper(\$x,\$y){\$body}}{
	       expptr yval = gensym(`yval);
	       expptr result = gensym(`result);
	       return `{
		 ({
		    $type \$x;
		    $listtype \$result = NULL;
		    for($listtype \$yval = \$y; \$yval; \$yval = \$yval ->rest){
		      \$x = \$yval->first;
		      \$result = $consfun(\$body,\$result);}
		    \$result;
		    })};
	       }});
  add_form(`{
	     umacro{$listfun(\$x)}{
	       ucase(x){
		 {\$first,\$rest}:{return `{$consfun(\$first,$listfun(\$rest))};};
		 {\$any}:{return `{$consfun(\$x,NULL)};};}}
	     });
  add_form(`{
	     $listtype $delete($type x, $listtype y){
	       if(!y) return NULL;
	       if(y->first == x)return y->rest;
	       return $consfun(y->first,$delete(x,y->rest));}
	     });
  
  }

umacro{deflists($type)}{ //for use with non-class types
  add_list_forms(type);
  return NULL;
  }


init_fun(mcD2_init);
deflists(expptr);

deflists(voidptr);

/** ========================================================================
  list operations on semicolon lists and comma lists
========================================================================**/

expptr semi_first(expptr x){
  ucase(x){
    {$first;$any}:{return first;};
    {$any}:{return x;};}    
  }


// semi_first(`{a;})

expptr semi_rest(expptr x){
  ucase(x){
    {$any;}:{return NULL;};
    {$any;$rest}:{return rest;};
    {$any}:{return NULL;};
  }
  return NULL;
}

//semi_rest(`{a;})

expptr semi_cons(expptr x, expptr y){
  if(y){return `{$x;$y};}
  return `{$x;};
}

expptr semi_append(expptr x, expptr y){
  if(x)return semi_cons(semi_first(x), semi_append(semi_rest(x),y));
  return y;
}

// semi_append(`{a;b;},`{c;d;})

umacro{semi_iter($x,$y){$body}}{
  expptr yval = gensym(`yval);
  return `{{
      expptr $yval = $y;
      while($yval){
	expptr $x = semi_first($yval);
	{$body};
	$yval = semi_rest($yval);}}};
}

// semi_iter(x,`{a;b;c;}){mcpprint(x);}

umacro{semi_map($x,$y)($expression)}{
  //this reverses the list
  expptr yval = gensym(`yval);
  expptr result = gensym(`result);
  return `{({
	expptr $yval = $y;
	expptr $result = NULL;
	semi_iter($x, $yval){$result = semi_cons($expression,$result);};
	$result;})};
}

// semi_map(x, `{a;b;c;})(`{f($x)})


expptr comma_first(expptr x){
  ucase(x){
    {$first,$any}:{return first;};
    {$any}:{return x;};}
  }

// comma_first
// comma_first(`{a,b})

// comma_first(`a)

expptr comma_rest(expptr x){
  ucase(x){
    {$any,}:{return NULL;};
    {$any,$rest}:{return rest;};
    {$any}:{return NULL;};}
  }

//comma_rest(`{a})
//comma_rest(`{a,})
//comma_rest(`{a,b,c})

int comma_length(expptr x){
  ucase(x){
    {}:{return 0;};
    {$any,}:{return 1;};
    {$any,$rest}:{return 1+ comma_length(rest);};
    {$any}:{return 1;};}
  }

//int_exp(comma_length(`{}))
//int_exp(comma_length(`{a}))
//int_exp(comma_length(`{a,}))
//int_exp(comma_length(`{a,b,c}))
//int_exp(comma_length(`{a b, c d}))

expptr comma_cons(expptr x, expptr y){
  if(y){return `{$x,$y};}
  return x;
}

expptr comma_append(expptr x, expptr y){
  if(x)return comma_cons(comma_first(x), comma_append(comma_rest(x),y));
  return y;
}

// comma_append(`{a,b},`{c,d})

umacro{comma_iter($x,$y){$body}}{
  expptr yval = gensym(`yval);
  return `{{
      expptr $yval = $y;
      while($yval){
	expptr $x = comma_first($yval);
	{$body};
	$yval = comma_rest($yval);}}};
}

//comma_iter(x,`{a,b,c}){mcpprint(x);}

expptr comma_reverse(expptr cl){
  expptr result = NULL;
  comma_iter(x, cl){result = comma_cons(x,result);};
  return result;
}

//comma_reverse(`{a,b})

umacro{comma_map($x,$y)($expression)}{
  expptr yval = gensym(`yval);
  expptr result = gensym(`result);
  return `{({
	expptr $yval = $y;
	expptr $result = NULL;
	comma_iter($x, $yval){$result = comma_cons($expression,$result);};
	$result = comma_reverse($result);
	$result;})};
}

comma_map(x, `{a,b,c})(`{f($x)})

/** ========================================================================
  miscellaneous
========================================================================**/

//  pushprop assumes the property value is a list of pointers

umacro{pushprop($val, getprop($x, $prop))}{
  expptr xval = gensym(`xval);
  expptr propval = gensym(`prop);
  return `{{
      voidptr $xval = $x;
      voidptr $propval = $prop;
      setprop($xval, $propval, voidptr_cons($val, (voidptr_list) getprop($xval, $propval, NULL)));}};
  }
