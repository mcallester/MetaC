
/** ========================================================================
general utilities
========================================================================**/

//  pushprop assumes the property value is a list of pointers

deflists(voidptr);
/** 7:done **/

umacro{pushprop($val, getprop($x, $prop))}{
  expptr xval = gensym("xval");
  expptr propval = gensym("prop");
  return `{{
      voidptr $xval = $x;
      voidptr $propval = $prop;
      setprop($xval, $propval, voidptr_cons($val, (voidptr_list) getprop($xval, $propval, NULL)));}};
}
/** 10:done **/

// macroexpand1(`{pushprop(v,getprop(x,`{foo}))})

// pushprop(`{a}, getprop(`{b}, `{foo}));

// (expptr) (((voidptr_list) getprop(`{b},`{foo},NULL))->first)

int occurs_in(expptr symbol, expptr exp){
  if(atomp(exp))return (symbol == exp);
  if(cellp(exp))return (occurs_in(symbol,car(exp)) || occurs_in(symbol,cdr(exp)));
  return occurs_in(symbol,paren_inside(exp));
}

//    general exceptions

expptr exception[0] = NULL;

expptr exception_value[0];

umacro{catch_excep{$exception}{$body}{$handler}}{
  return `{
    catch({$body})
      if(exception[0]){
	if(exception[0] == `{$exception}){
	  $handler
	  expception[0] = NULL;}
	else continue_throw();}};
}

umacro{throw_excep{$exception;$value}}{
  return `{
    expcetion[0] = `{$exception};
    exception_value[0] = $value;
    throw();};
}

//   a macro to prevent "unmentioned variable" compiler errors

umacro{mention($x)}{
  return `{if($x ? $x : $x){}};}

// define_lists defines list types and list operations over a given c type.

void define_lists(expptr class){
  char * cstring = atom_string(class);
  expptr listtype = string_atom(sformat("%s_list",cstring));
  expptr structtype = string_atom(sformat("%s_list_struct",cstring));
  expptr consfun = string_atom(sformat("%s_cons",cstring));
  expptr iterator = string_atom(sformat("%s_iter",cstring));
  expptr pusher = string_atom(sformat("%s_push",cstring));
  expptr listfun = string_atom(sformat("%s_listfun",cstring));
  expptr append = string_atom(sformat("%s_append",cstring));
  expptr nth = string_atom(sformat("%s_nth",cstring));
  expptr member = string_atom(sformat("%s_member",cstring));
  
  add_form(`{
      typedef struct $structtype{$class first; struct $structtype * rest;} * $listtype;
    });
  add_form(`{
      $listtype $consfun($class first, $listtype rest){
	$listtype cell = ($listtype) undo_alloc(sizeof($listtype *));
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
      $class $nth($listtype x, int n){
	if(!x)berror("list too short in nth");
	if(n == 1){return x->first;}
	return $nth(x->rest,n-1);}
    });
  add_form(`{
	int $member($class x, $listtype y){
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
	expptr yval = gensym("yval");
	return `{{
	    $class \$x;
	    for($listtype \$yval = \$y; \$yval; \$yval = \$yval ->rest){
	      \$x = \$yval->first;
	      \$body}
	  }};}
    });
  add_form(`{
      umacro{$listfun(\$x)}{
	ucase{x;
	  {\$first,\$rest}:{return `{$consfun(\$first,$listfun(\$rest))};}
	  {\$any}:{return `{$consfun(\$x,NULL)};}}}
    });
}
/** 1:done **/

umacro{deflists($type)}{ //for use with non-class types
  define_lists(type);
  return `{{}};
}
/** 2:done **/

//list operations on C statement lists (semicolon lists).

expptr semi_append(expptr x, expptr y){
  ucase{x;
    {$first ; $rest}:{return `{$first;${semi_append(rest,y)}};}
    {$last ;}:{return `{$last ; $y};}}
  return NULL;
}
/** 3:done **/

expptr semi_reverse(expptr x){
  expptr result = NULL;
  ucase{x;
    {{}}:{return x;}
    {$any ;}:{return x;}
    {$first ; $rest}:{result = `{$first ;}; x = rest;}}
  while(1){
    ucase{x;
      {$last ;}:{return `{$last ; $result};}
      {$first ; $rest}:{result = `{$first ; $result}; x = rest;}}}
}
/** 4:done **/


//  semi_reverse(`{a; b; c;})  

umacro{semi_map($x,$y)($expression)}{
  //intuitively map((lambda x expression) y)
  //except map_semi reverses the list
  expptr yval = gensym("yval");
  expptr result = gensym("result");
  expptr rest = gensym("rest");
  return `{({
	expptr $yval = $y;
	expptr $result = NULL;
	ucase{$yval;
	  {\$$x ; \$$rest}:{$result = `{\${($expression)} ;}; $yval = $rest;}
	  {\$$x ;}:      {$result = `{\${($expression)} ;}; $yval = NULL;}}
	while($yval){
	  ucase{$yval;
	    {\$$x ; \$$rest}:{$result = `{\${($expression)} ; \$$result}; $yval = $rest;}
	    {\$$x ;}:      {$result = `{\${($expression)} ; \$$result}; $yval = NULL;}}}
	$result;})};
}
/** 5:done **/


//  macroexpand1(`{semi_map(z,`{a;b;c})(`{(d,\$z)})})

//  semi_map(z,`{a;b;c;})(`{(d,$z)})


expptr semi_to_comma(expptr e){
  ucase{e;
    {$first ; $rest}:{return `{$first,${semi_to_comma(rest)}};}
    {$last ;}:{return last;}
    {$any}:{return e;}}
}
/** 14:done **/

// semi_to_comma(`{a; b;})

/** ========================================================================
  classes

  the class hierarchy must be a tree with each class having a unique superclass rooted at `object.

  after doing

  defclass{object, foo(expptr x;)}

  the declaration

  defclass{foo; bar{foo y; bar z;}} generates

 typedef struct bar_struct{
  int class_index;
  expptr x;
  foo y;
  bar_struct * z;} bar_struct, *bar;

 bar new_bar(expptr x, foo y, bar z;){
  bar bar_37 = (bar) undo_alloc(sizeof (bar_struct));
  bar_37->class_index = 2;
  bar_37->x = s;
  bar_37->y = y;
  bar_37->z = z;
  return bar_37;
 }

 Any class foo in the class hierarchy can be instantiated by calling new_foo.
========================================================================**/

expptr fix_typedef_recursion(expptr pointertype, expptr structtype, expptr ivars){
  return semi_reverse(semi_map(e,ivars)({
	expptr result = NULL;
	ucase{e;
	  {$type $var}:{
	    if(type == pointertype){
	      result = `{$structtype * $var};
	    }
	    else {
	      result = e;}}}
	result;}));
}
/** 20:done **/

// fix_typedef_recursion(`{foo}, `{bar}, `{int x; foo y; int z;})

expptr complete_ivars(expptr ivars, expptr superclass){
  if(class == `{object})
    return `{int class_index;};
  else
    return semi_append(complete_ivars((expptr) getprop(superclass, `{ivars}, NULL),
				      (expptr) getprop(superclass,`{superclass},NULL)),
		       ivars);
}
/** 16:done **/

expptr init_ivars(expptr ivars){
  ucase{ivars;
    {$any $var ; $rest}:{return `{self->$var = $var; ${init_ivars(rest)}};}
    {$any $var;}:{return `{self->$var = $var;};}}
  return NULL;
}
/** 17:done **/

void add_class_constructor(expptr superclass, expptr class, expptr complete_ivars){
  int index = class_index(class);
  expptr constructor = string_atom(sformat("new_%s",atom_string(class)));
  expptr complete_ivars = complete_ivars(ivars, superclass);
  ucase{complete_ivars;
    {$any;}:{berror(sformat("class %s has no instance variables",atom_string(class)));}
    {$any;$rest_ivars}:{
      add_form(`{
	  $class $constructor(${semi_to_comma(rest_ivars)}){
	    $class self = ($class) undo_alloc(sizeof($structname));
	    self->class_index = ${int_exp(index)};
	    ${init_ivars(rest_ivars)}
	    return self;}
	});}}
}

void declare_class(expptr superclass, expptr class, exptr ivars){
  setprop(class, `{ivars} `{$ivars});
  setprop(class,`{superclass}, superclass);
  pushprop(class, getprop(superclass, `{subclasses}));
  copy_methods(superclass,class);
}

int class_counter[0] = 0;
/** c compilation error **/

ucase{`{int foobar[0] = 0;}; {$any$any[0]=$any;}:{}}
/** dynamic-check error **/

car(car(file_expressions("test.mc")))
/** 2:$type$x[0]=$any **/
// _5

cdr(car(car(file_expressions("test.mc"))))
/** 3:$x[0]=$any **/
// _10

cdr(cdr(car(car(file_expressions("test.mc")))))
/** 1:[0]=$any **/
// _8

{{expptr top_1=cons
      (cons
        (string_atom("int"),
        cons
          (cons
            (cons(string_atom("foobar"),intern_paren('[',string_atom("0"))),
            string_atom("=")),
          string_atom("0"))),
      string_atom(";"));
    expptr _5=top_1;
    if(cellp(_5))
      {expptr _10=car(_5);
      if(cellp(_10))
        {expptr _24=car(_10);
        
          {expptr type=_24;
          expptr _8=cdr(_10);
          if(cellp(_8))
            {expptr _17=car(_8);
            
              {expptr X=_17;
              expptr _9=cdr(_8);
              if(cellp(_9))
                {expptr _14=car(_9);
                if(cellp(_14))
                  {expptr _19=car(_14);
                  if(parenp(_19)&&constructor(_19)=='[')
                    {expptr _6=paren_inside(_19);
                    if(_6==string_atom("0"))
                      {expptr _26=cdr(_14);
                      if(_26==string_atom("="))
                        {expptr _2=cdr(_5);
                        if(_2==string_atom(";")){if(symbolp(type)&&symbolp(X)){goto done_1;}}
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    match_failure
      (top_1,
      cons
        (cons
          (cons
            (intern_paren
              ('{',
              cons
                (cons
                  (cons(string_atom("$"),string_atom("type")),
                  cons
                    (cons(string_atom("$"),string_atom("X")),
                    cons
                      (cons(intern_paren('[',string_atom("0")),string_atom("=")),
                      cons(string_atom("$"),string_atom("any"))))),
                string_atom(";"))),
            string_atom(".")),
          intern_paren
            ('(',
            cons
              (cons
                (cons
                  (string_atom("symbolp"),
                  intern_paren('(',string_atom("type"))),
                string_atom("&&")),
              cons(string_atom("symbolp"),intern_paren('(',string_atom("X")))))),
        string_atom("")));
    done_1: ;
    }
  return string_atom("done");
  }
/** dynamic-check error **/


expptr foobar[0] = `{a};
/** **/

int class_dim[0] = 100;
/** c compilation error **/

expptr class_name[100];
/** 18:done **/

{for(int i = 0; i<100; i++){class_name[i] = NULL;}}
/** 19:done **/

int class_index(expptr class){
  int index = getprop_int(implementation,`{index},-1);
  if(index < 0)berror(sformat("no class index for %s",atom_string(class)));
  return index;
}

void add_class_index_forms(class){
  index = implement_counter[0];
  if(index == class_dim[0]){
    berror(sformat("attempt to create more than %d implementations", class_dim[0]));}
  add_form(`{implement_counter[0]++ ;});
  add_form(`{setprop_int(`{$implementation},`{index}, ${int_exp(index)});});
}

void add_class_forms(expptr superclass, expptr class, expptr ivars){
  if( superclass != `{object} && !getprop(superclass,`{superclass},NULL)){
    berror(sformat("illegal superclass %s",atom_string(superclass)));}
  expptr old_superclass = getprop(class,`{superclass},NULL);
  expptr old_ivars = getprop(class,`{class_ivars},NULL);
  if(old_superclass && (ivars != old_ivars || superclass != old_superclass)){
    berror(sformat("attempt to change definition of class %s", atom_string(class)));}
  if(!old_superclass){
    expptr structname = string_atom(sformat("%s_struct",atom_string(class)));
    complete_ivars = complete_ivars(fix_typedef_recursion(class, structname, superclass), superclass);
    add_form(`{typedef struct $structname{$complete_ivars} $structname, * $class;})
    add_form(`{define_lists(`{$class});});
    add_form(`{install_class(`{$superclass}, `{$class});});
}


umacro{defclass{$superclass; $class{$ivars}}}{
  add_class_forms(superclass, class, ivars);
  return `{};
}

/** ========================================================================

 if, for some class foo, we do

 defmethod{expptr bar.g(foo self; expptr x;){return x;}}

 then g is declared by expptr g(expptr x);

 For g(z) to be defined the class of z must be a subclass (inclusive) of foo.

The code that gets fun for g(z) depends on the subtype of foo to which g is applied.

 If g is a declared method then g(z) macro-expands to an appropriately typed version of M[z->class_index](z)
 where M is the statically determined method table for g.

  all instance variables, including inherited instance variables, get bound to local
  variables of the same name.  There is also a local self variable.
========================================================================**/

 
expptr method_table(expptr f){
  expptr table = getprop(f,`{method_table},NULL);
  if(!table)berror(sformat("no method table for %s",atom_string(f)));
  return table;
}

void add_method_table_forms(expptr f){
  table = gensym("method_table");
  add_form(`{voidptr $table[class_dim[0]];});
  add_form(`{for(int i = 0;i<class_dim[0];i++){$table[i] = NULL;};});
  add_form(`{setprop(`{$f},`{method_table},`{$table});});
}

void add_method_forms(expptr outtype, expptr f, expptr argvars){
  expptr oldtype = getprop(f,`{type},NULL);
  if(oldtype){
    ucase{oldtype;{$outtype2($argvars2)}:{
	if(!(outtype2 == outtype && argvars2 == argvars))berror(sformat("attempt to change the signature of method %s",atom_string(f)));}}}
  else{
    orcase{argvars; {$any self}{$any self, $any}:{}} //check that the first agument is "self"
    add_form(`{set_macro(`{$f}, method_expansion);});
    add_form(`{setprop(`{$f}, `{type}, `{$outtype($argvars)});});}
}

expptr method_type(expptr f){
  expptr type = getprop(f,`{type}, NULL);
  if(!type)berror("MC bug: no type for method");
  return type;
}

expptr method_expansion(expptr e){
  expptr f_imp = gensym("f_imp");
  expptr y = gensym("y");
  ucase{e;
    {$f($argexps)}:{
      expptr restexps = NULL;
      orcase{argexps;{$selfexp, $restexps}{$selfexp}:{
	  ucase{method_type(f); {$outtype($argvars)}:{
	      orcase{argvars; {$selftype self, $any}{$selftype self}:{
		  return `{
		    ({$selftype $selfvar = $selfexp;
		      $outtype (* $f_imp)($argvars);
		      $f_imp = ${method_table(f)}[$selfvar -> class_index];
		      if(!$f_imp)berror(sformat("method %s not defined on given object",atom_string(`{$f})));
		      $outtype $y = $f_imp(${restexps ? `{$selfvar, $restexps} : selfexp});
		      $y;})};
		}}}}}}}}
  return NULL;
}


expptr localize_ivars(expptr ivars, expptr body){
  ucase{ivars;
    {$type $var ; $rest}:{
      if(occurs_in(var, body)){
	return `{$type $var = self->$var; ${localize_ivars(rest, body)}};}
      else{
	return localize_ivars(rest, body);}}
    {$type $var;}:{
      if(occurs_in(var, body)){
	return `{$type $var = self->$var;};}
      else{
	return `{};}}}
  return NULL;
}

umacro{defmethod{$outtype $impclass.$f($argvars){$body}}}{

  int index = getprop_int(impclass, `{index}, -1);
  if(index < 0)berror(sformat("class %s is not an implementation class", atom_string(impclass)));
  declare_method(outtype, f, argvars);

  expptr f_imp = gensym(atom_string(f));
  expptr selfvar  = gensym("self");
  expptr restargs = NULL;

  orcase{argvars; {$superclass self, $restargs}{$superclass self}:{
      add_form(`{
	  ${method_table(f)}[${int_exp(index)}] = $f_imp;
	});
      expptr ivars = complete_ivars(impclass);
      ucase{ivars;
	{$any ; $rest_ivars}:{
	  add_form(`{
	      $outtype $f_imp(${restargs ? `{$superclass $selfvar, $restargs} : `{$superclass $selfvar}}){
		$impclass self = ($impclass) $selfvar;
		${localize_ivars(rest_ivars,body)}
		$body}});}}}}
  return `{};
}



/** ========================================================================
  tests

subclass{object; foo{expptr x;}};

void f(foo x);

defimp{foo; bar{expptr y;}};

foo foovar[0] = new_bar(`{a},`{b});

int_exp(foovar[0]->class_index)

foovar[0]->x

defimp{foo; baz{expptr y;}};

foo foovar2[0] = new_baz(`{a},`{b});

int_exp(foovar2[0]->class_index)

defmethod{expptr bar.msg(foo self){return y;}};

msg(foovar[0])

========================================================================**/
