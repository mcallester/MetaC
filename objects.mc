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

umacro{deflists($type)}{ //for use with non-class types
  define_lists(type);
  return `{{}};
}

void declare_class(expptr class){
  if(0 == getprop_int(class,`{defined_class},0)){
    expptr structname = string_atom(sformat("%s_struct",atom_string(class)));
    add_form(`{typedef struct $structname{int implementation_index;} * $class;}); //this index identifies the implementation of the object
    add_form(`{setprop_int(`{$class},`{defined_class},1);});
    define_lists(class);}
}

umacro{defclass($class)}{
  declare_class(class);
  return `{{}};
}

int implement_counter[0] = 0;

expptr method_table(expptr f){
  expptr table = getprop(f,`{method_table},NULL);
  if(table)return table;
  table = gensym("method_table");
  add_form(`{voidptr $table[1000];});
  add_form(`{for(int i = 0;i<1000;i++){$table[i] = NULL;};});
  add_form(`{setprop(`{$f},`{method_table},`{$table});});
  return table;
}

int implementation_index(expptr implementation){
  int index = getprop_int(implementation,`{index},-1);
  if(index >= 0)return index;
  index = implement_counter[0];
  if(index == 1000)berror("attempt to create more than 1000 implementations");
  add_form(`{implement_counter[0]++ ;});
  add_form(`{setprop_int(`{$implementation},`{index}, ${int_exp(index)});});
  return index;
}

void declare_method(expptr outtype, expptr f, expptr argvars){
  expptr oldtype = getprop(f,`{type},NULL);
  if(oldtype){
    ucase{oldtype;{$outtype2($argvars2)}:{
	if(!(outtype2 == outtype && argvars2 == argvars))berror(sformat("attempt to change the signature of %s",atom_string(f)));}}}
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

expptr maybe_comma(expptr x, expptr y){if(y)return `{$x, $y}; return x;}

expptr method_expansion(expptr e){
  expptr f_imp = gensym("f_imp");
  expptr selfvar = gensym("selfvar");
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
		      $f_imp = ${method_table(f)}[$selfvar -> implementation_index];
		      if(!$f_imp)berror(sformat("method %s not defined on given object",atom_string(`{$f})));
		      $outtype $y = $f_imp(${maybe_comma(selfvar, restexps)});
		      $y;})};
		}}}}}}}}
  return NULL;
}

expptr instancevars_for(expptr ivars){
  ucase{ivars;
	{$type $var , $rest}:{return `{$type $var ; ${instancevars_for(rest)}};}
	{$type $var}:{return `{$type $var;};}}
  return NULL; //avoids compiler error
}

expptr assign_ivars(expptr ivars){
  ucase{ivars;
	{$any $var , $rest}:{return `{self->$var = $var; ${assign_ivars(rest)}};}
	{$any $var}:{return `{self->$var = $var;};}}
  return NULL;
}

int occurs_in(expptr symbol, expptr exp){
  if(atomp(exp))return (symbol == exp);
  if(cellp(exp))return (occurs_in(symbol,car(exp)) || occurs_in(symbol,cdr(exp)));
  return occurs_in(symbol,paren_inside(exp));
}

expptr install_ivars(expptr ivars, expptr body){
  ucase{ivars;
	{$type $var , $rest}:{
	  if(occurs_in(var,body))
	    return `{$type $var = self->$var; ${install_ivars(rest,body)}};
	  else
	    return install_ivars(rest,body);}
	{$type $var}:{
	  if(occurs_in(var,body))
	    return `{$type $var = self->$var;};
	  else
	    return `{};}}
  return NULL; //avoids compiler warning.
}

umacro{defimp{$class $implementation($ivars){$methods}}}{
  declare_class(class);
  int index = implementation_index(implementation);
  expptr impstruct = string_atom(sformat("%s_struct",atom_string(implementation)));
  expptr impclass = string_atom(sformat("%s_class",atom_string(implementation)));

  add_form(`{
      typedef struct $impstruct{int implementation_index; ${instancevars_for(ivars)}} * $impclass;
    });
  add_form(`{
      $class $implementation(${ivars}){
	$impclass self = ($impclass) undo_alloc(sizeof($impclass *));
	self->implementation_index = ${int_exp(index)};
	${assign_ivars(ivars)}
	return ($class) self;}
    });

  while(methods){
    expptr class_selfvar = gensym("self");
    expptr rest_methods = NULL;
    orcase{methods;{$outtype $f($argvars){$body}}{$outtype $f($argvars){$body} $rest_methods}:{
	expptr f_imp = gensym(atom_string(f));
	declare_method(outtype, f, argvars);
	expptr restvars= NULL;
	orcase{argvars; {$selftype self, $restvars}{$selftype self}:{
	    if(selftype != class)berror("method self type does not match implementation class");
	    add_form(`{
		$outtype $f_imp(${maybe_comma(`{$class $class_selfvar}, restvars)}){
		  $impclass self = ($impclass) $class_selfvar;
		  ${install_ivars(ivars,body)}
		  $body}
	      });
	    add_form(`{
		${method_table(f)}[${int_exp(index)}] = $f_imp;
	      });

	    methods = rest_methods;}}}}}
  return `{{}};
}


/** ========================================================================
 test cases

  defimp{foo simple_foo(expptr x){
  expptr getx(foo self){return x;}
  }
}

  getx(simple_foo(`{gotit}))

======================================================================== **/



/** ========================================================================
  general exceptions
======================================================================== **/

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

`{a}

umacro{throw_excep{$exception;$value}}{
  return `{
    expcetion[0] = `{$exception};
    exception_value[0] = $value;
    throw();};
}


/** ========================================================================
  a macro to prevent "unmentioned variable" compiler errors
======================================================================== **/

umacro{mention($x)}{
  return `{if($x ? $x : $x){}};}
