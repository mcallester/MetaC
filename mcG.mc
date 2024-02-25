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

umacro{stackexp_from_undo_frame($exp)}{
  expptr expvar = gensym(`expvar);
  expptr stackexp = gensym(`stack_exp);
  return
  `{({
       expptr $stackexp;
       unwind_protect{
	 push_undo_frame();
	 expptr $expvar = $exp; //unsafe.  The rest is safe which is required for proper stack memory management.
	 expptr $stackexp = expptr_to_stack($expvar);
	 pop_undo_frame();
	 }{
	 pop_undo_frame();}
       $stackexp;
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
	       if(!x)return `NULL;
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

//comma_map(x, `{a,b,c})(`{f($x)})

/** ========================================================================
closures
========================================================================**/

expptr wrap_body(expptr freevars, expptr cname, int offset, expptr body){
  if(!freevars || freevars == `{})return body;
  return `{
    ${comma_first(freevars)} = *($cname + ${int_exp(offset)});
    ${wrap_body(comma_rest(freevars),cname,offset+1, body)}};}

//wrap_body(`{expptr x, expptr y},`h, 1,`{foo(x,y,z);})

expptr install_vars(expptr cname, expptr freevars, int offset){
  if(!freevars || freevars == `{})return `{};
  return `{
    *($cname + ${int_exp(offset)}) = ${rightarg(comma_first(freevars))};
    ${install_vars(cname,comma_rest(freevars),offset+1)}};}

//install_vars(`c,`{expptr x, expptr y}, 1)

umacro{closure_type(($args)->$outtype)}{
  return(args == `{})? `{$outtype (**)(void*)} : `{$outtype (**)(void*,$args)};}

//macroexpand(`{closure_type((int,int)->int)})

//macroexpand(`{closure_type(()->int)})

umacro{closure_typeexp($typename:($args)->$outtype)}{
  return (args== `{})? `{$outtype (**$typename)(void*)} : `{$outtype (**$typename)(void*,$args)};}

umacro{closure_typedef($typename:($args)->$outtype)}{
  return `{typedef closure_typeexp($typename: ($args)->$outtype)};
  }

//macroexpand(`{closure_typedef(foo:(int,int)->int)})

//macroexpand(`{closure_typedef(foo:()->int)})

umacro{apply_closure($f)($args)}{
  if(args == `{}) return `{(* $f)($f)};
  return  `{(* $f)($f,$args)};}

umacro{lambda $outtype($freevars)($args){$body}}{ // all free variables must be pointers
  expptr pname = gensym(`lambda_proc);
  expptr cname = gensym(`closure);
  expptr f = gensym(`f);
  if(args == `{})add_form(`{void $pname(voidptrptr $cname){${wrap_body(freevars,cname,1,body)}}});
  else
  add_form(`{void $pname(voidptrptr $cname,$args){${wrap_body(freevars,cname,1,body)}}});
  return `{({
	      void** $cname = undo_alloc(${int_exp(sizeof(void*)*(1 + comma_length(freevars)))});
	      *$cname = $pname;
	      {${install_vars(cname,freevars,1)}}
	      closure_typeexp($f:($args)->$outtype);
	      $f = (closure_type(($args)->$outtype)) $cname;
	      $f;})};}


//macroexpand(`{lambda void (expptr x)(expptr y){e[0]= `{\$x,\$y};}})

//macroexpand(`{lambda void ()(expptr y){e[0]= `{\$y};}})

//expptr e[0];

//typedef closure_typedef(mcnoticer:(expptr)->void);

//mcnoticer foo(expptr x){mcpprint(x); return lambda void(expptr x)(expptr y){e[0] = `{$x,$y};};}

//mcnoticer n[0];

//n[0] = foo(`a);

//apply_closure(n[0])(`b);

//e[0]

//typedef closure_typedef(mcthunk:()->void);

//mcthunk bar(expptr x){mcpprint(x); return lambda void(expptr x)(){e[0] = `{$x};};}

//mcthunk m[0];

//m[0] = bar(`a);

//apply_closure(m[0])();

//e[0]
