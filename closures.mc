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
    *($cname + ${int_exp(offset)}) = ${cdr(comma_first(freevars))};
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

