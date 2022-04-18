/** ========================================================================
  LIST operations on arbitrary types
========================================================================**/


// define_lists defines list types and list operations over a given c type.

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
	expptr yval = gensym("yval");
	return `{{
	    $type \$x;
	    for($listtype \$yval = \$y; \$yval; \$yval = \$yval ->rest){
	      \$x = \$yval->first;
	      \$body}
	  }};}
    });
  add_form(`{
      umacro{$mapper(\$x,\$y){\$body}}{
	expptr yval = gensym("yval");
	expptr result = gensym("result");
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
	ucase{x;
	  {\$first,\$rest}:{return `{$consfun(\$first,$listfun(\$rest))};};
	  {\$any}:{return `{$consfun(\$x,NULL)};}}}
    });
}

umacro{deflists($type)}{ //for use with non-class types
  add_list_forms(type);
  return `{{}};
}

deflists(expptr);
deflists(voidptr);


/** ========================================================================
  list operations on semicolon lists and comma lists
========================================================================**/

expptr semi_first(expptr x){
  ucase{x;
    {$first;$any}:{return first;};
    {$any}:{return x;}    
  }
}

// semi_first(`{a;})

expptr semi_rest(expptr x){
  ucase{x;
    {$any;}:{return NULL;};
    {$any;$rest}:{return rest;};
    {$any}:{return NULL;}
  };
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
  expptr yval = gensym("yval");
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
  expptr yval = gensym("yval");
  expptr result = gensym("result");
  return `{({
	expptr $yval = $y;
	expptr $result = NULL;
	semi_iter($x, $yval){$result = semi_cons($expression,$result);};
	$result;})};
}

// semi_map(x, `{a;b;c;})(`{f($x)})


expptr comma_first(expptr x){
  ucase{x;
    {$first,$any}:{return first;};
    {$any}:{return x;}}
}

// comma_first
// comma_first(`{a,b})

// comma_first(`a)

expptr comma_rest(expptr x){
  ucase{x;
    {$any,}:{return NULL;};
    {$any,$rest}:{return rest;};
    {$any}:{return NULL;}};
  }

//comma_rest(`{a})
//comma_rest(`{a,})
//comma_rest(`{a,b,c})

int comma_length(expptr x){
  ucase{x;
    {}:{return 0;};
    {$any,}:{return 1;};
    {$any,$rest}:{return 1+ comma_length(rest);};
    {$any}:{return 1;}};
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
  expptr yval = gensym("yval");
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
  expptr yval = gensym("yval");
  expptr result = gensym("result");
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

//   a macro to prevent "unmentioned variable" compiler errors

umacro{mention($x)}{
  return `{if($x ? $x : $x){}};}


//  pushprop assumes the property value is a list of pointers

umacro{pushprop($val, getprop($x, $prop))}{
  expptr xval = gensym("xval");
  expptr propval = gensym("prop");
  return `{{
      voidptr $xval = $x;
      voidptr $propval = $prop;
      setprop($xval, $propval, voidptr_cons($val, (voidptr_list) getprop($xval, $propval, NULL)));}};
}

int occurs_in(expptr symbol, expptr exp){
  if(atomp(exp))return (symbol == exp);
  ucase{exp;
    {$e->$any}:{return occurs_in(symbol,e);};
    {$any}:{
      if(cellp(exp))
	return (occurs_in(symbol,car(exp)) || occurs_in(symbol,cdr(exp)));
      else
	return occurs_in(symbol,paren_inside(exp));}}
}

//    general exceptions  

expptr exception[0] = NULL;

expptr exception_value[0];

umacro{catch_excep{$exception}{$body}{$handler}}{
  return `{
    catch({$body})
      if(exception[0]){
	if(exception[0] == `$exception){
	  exception[0] = NULL;
	  $handler}
	else continue_throw();}};
}

umacro{throw_excep{$exception;$value}}{
  return `{
    exception[0] = `{$exception};
    exception_value[0] = $value;
    throw();};
}

/** ========================================================================
  classes

  Each class is associated with a sequence of instnace variables.  The class hierarchy is a dag
  subject to the constraint that the instance variables of a superclass are a prefix of
  the instance variables of a subclass. The top of the class heirarchy is the class "object".

  Each class definition specifies a superclass but additional superclasses can be added with declare_subclass(subclass,superclass)

  after doing

  defclass{foo{object; expptr x;}}

  the declaration

  defclass{bar{foo; foo y; bar z;}} generates

 typedef struct bar_struct{
  int class_index;
  expptr x;
  foo y;
  bar_struct * z;} bar_struct, *bar;

 Any class foo in the class hierarchy can be allocated with the macro new(foo).
========================================================================**/

void add_class_forms(expptr superclass, expptr class, expptr added_ivars);

umacro{defclass{$class{$ivars}}}{
  expptr superclass = semi_first(ivars);
  if(!(atomp(class) & atomp(superclass)))berror("illegal syntax in defclass --- class names must be atoms");
  ivars = semi_rest(ivars);
  add_class_forms(superclass, class, ivars);
  return `{};
}

typedef struct object_struct{int class_index;} * object;

setprop(`object,`ivars,`{int class_index;});

setprop_int(`object,`index,0);

expptr class_ivars(expptr class){
  expptr ivars = getprop(class,`ivars,NULL);
  if(!ivars)berror(sformat("undefined class %s",atom_string(class)));
  return ivars;
}

int class_index(expptr class){
  int index = getprop_int(class,`index,-1);
  if(index < 0)berror(sformat("undefined class %s",atom_string(class)));
  return index;
}

void add_class_forms(expptr superclass, expptr class, expptr added_ivars){
  if(class == `object)berror("attempt to redefine the object class");
  expptr complete_ivars = semi_append(class_ivars(superclass), added_ivars); //this ensures that superclass is a defined class.
  expptr old_ivars = getprop(class,`ivars,NULL);
  if(old_ivars && complete_ivars != old_ivars){
    berror(sformat("attempt to change definition of class %s", atom_string(class)));}
  expptr structname = string_atom(sformat("%s_struct",atom_string(class)));
  add_form(`{typedef struct $structname * $class;});
  add_form(`{setprop_int(`$class, `size, sizeof($structname));});
  add_list_forms(class);
  add_form(`{typedef struct $structname{$complete_ivars} $structname, * $class;});
  add_form(`{install_class(`$superclass, `$class, `{$complete_ivars});});
}

int class_counter[0] = 1;

umacro{class_dim()}{return `100;}

expptr class_name[class_dim()];

class_name[0] = `object;

for(int i = 1; i<class_dim(); i++){class_name[i] = NULL;};

expptr classof(voidptr x){
  int index = ((object) x)->class_index;
  if(index < 0 || index >= class_dim())berror("attempt to determine the class of non-object");
  expptr class = class_name[index];
  if(!class)berror("attempt to determine the class of a non-object");
  return class;
}

umacro{declare_subclass($subclass,$superclass)}{
  return `{add_superclass(`{$subclass},`{$superclass})};
}

void add_superclass(expptr subclass, expptr superclass);


void install_class(expptr superclass, expptr class, expptr complete_ivars){
  int index = class_counter[0];
  if(index == class_dim()){
    berror(sformat("attempt to create more than %d classes", class_dim()));}
  setprop_int(class,`index, index);
  undo_set_int(class_counter[0], class_counter[0]+1);
  class_name[index] = class;
  setprop(class, `ivars, complete_ivars);
  add_superclass(class, superclass);
}

void copy_methods(expptr superclass, expptr subclass);

void check_subclass(expptr subclass, expptr superclass);

void add_superclass(expptr subclass, expptr superclass){
  check_subclass(subclass,superclass);
  pushprop(subclass, getprop(superclass,`subclasses));
  pushprop(superclass, getprop(subclass,`superclasses));
  copy_methods(superclass,subclass);
}

void check_subclass(expptr subclass, expptr superclass){
  expptr_list supervars = (expptr_list) getprop(superclass, `ivars, NULL);
  expptr_list subvars = (expptr_list) getprop(superclass, `ivars, NULL);
  while(supervars){
    if((subvars == NULL) | (supervars->first != subvars->first))
      berror(sformat("subclass %s is not compatible with superclass %s",
		     exp_string(subclass),
		     exp_string(superclass)));
    supervars = supervars->rest;
    subvars = subvars->rest;
  }
}
 
expptr init_ivars(expptr ivars){
  ucase{ivars;
    {$any $var ; $rest}:{return `{self->$var = $var; ${init_ivars(rest)}};};
    {$any $var;}:{return `{self->$var = $var;};}};
  return NULL;
}

umacro{new($class)}{
  expptr object = gensym("object");
  expptr struct_type = string_atom(sformat("%s_struct",atom_string(class)));
  return `{({
	$class $object = ($class) undo_alloc(sizeof($struct_type));
	$object->class_index = class_index(`$class);
	$object;})};
}

/** ========================================================================
 declaring a method

 declare_method{expptr g(foo self; expptr x;){return x;}}

 declares the abstract type of the method g.

========================================================================**/

void add_declare_method_forms(expptr outtype, expptr f_name, expptr argtypes);

void install_method_ptr(expptr f_name, expptr class, voidptrptr f_table, voidptr f_method);

expptr strip_var(expptr argpair){
  ucase{argpair;
    {$type $any}:{return type;};
    {$any}:{return argpair;}
  };
}

expptr strip_vars(expptr arglist){
  ucase{arglist;
    {$first,$rest}:{return `{${strip_var(first)},${strip_vars(rest)}};};
    {$any}:{return strip_var(arglist);}
  };
}

// strip_vars(`{a b, c d})

umacro{declare_method{$outtype $f($argvars)}}{
  add_declare_method_forms(outtype,f,strip_vars(argvars));
  return `{};
}

expptr method_table_name(expptr f){
  return string_atom(sformat("%s__method_table",atom_string(f)));
}

expptr localize_ivars(expptr ivars, expptr body);

void add_declare_method_forms(expptr outtype, expptr f_name, expptr argtypes){
  expptr f_table_name = method_table_name(f_name);
  expptr f_type = `{$outtype($argtypes)};
  expptr old_type = getprop(f_name,`method_type,NULL);
  if(!old_type){
    add_form(`{voidptr $f_table_name[class_dim()];});
    add_form(`{install_method_name(`$f_name, $f_table_name, `{$f_type});});}
  else if(f_type != old_type){
    berror(sformat("attempt to change the signature of method %s",atom_string(f_name)));}
}

expptr method_expansion(expptr e);

void install_method_name(expptr f_name, voidptrptr f_table, expptr f_type){
  set_macro(f_name, method_expansion);
  for(int i = 0;i<class_dim();i++){f_table[i] = NULL;};
  setprop(f_name,`method_type,f_type);
  setprop(f_name, `method_table, f_table);
}

voidptrptr method_table(expptr f_name){
  voidptrptr f_table = getprop(f_name,`method_table,NULL);
  if(!f_table)berror(sformat("undefined method name %s", atom_string(f_name)));
  return f_table;
}

void copy_methods(expptr superclass, expptr subclass){
  int superindex = class_index(superclass);
  expptr_iter(f_name, (getprop(superclass, `method_names, NULL))){
    voidptrptr table = method_table(f_name);
    voidptr f_ptr =  table[superindex];
    install_method_ptr(f_name,subclass,table,f_ptr);}
}

declare_method{expptr object_exp(object self)};

/** ========================================================================
 defining a method

 defmethod{expptr g(foo self; expptr x;){return x;}}

 this creates a foo-specific definition of g "expptr g_52(foo self_37, expptr x);"

 the method g must already be declared.  The self type of g can be a proper superclass of foo.

 For orbject arguments the arguments are automatically cast to the abstract argument type of g.
 this allows the self argument to be a proper subclass of foo in which case the method is inhereted.

 If g is a declared method then g(s,z) macro-expands to an appropriately typed version of M_g[s->class_index](s,z)
 where M_g is the statically determined method table for g.

 all instance variables, including inherited instance variables, get bound to local
 variables of the same name.  There is also a local self variable of type foo (in the above example)
 even if the self argument is actually a proper subclass of foo.

 Warning: do not use "x=v;" for instance variable x --- do "self->x = v;" instead.

========================================================================**/

void add_defmethod_forms(expptr outtype, expptr f_name, expptr argvars, expptr body);

umacro{defmethod{$outtype $f($argvars){$body}}}{
  add_defmethod_forms(outtype, f, argvars, body);
  return `{};
}

void check_method(expptr outtype, expptr fname, expptr argvars);

void add_defmethod_forms(expptr outtype, expptr f_name, expptr argvars, expptr body){
  check_method(outtype, f_name,argvars);
  expptr f_table_name = method_table_name(f_name);
  orcase{argvars; {$class self, $any}{$class self}:{
      expptr ivars = class_ivars(class);
      expptr f_ptr_name = string_atom(sformat("%s_at_%s", atom_string(f_name), atom_string(class)));
      ucase{ivars;
	{$any ; $rest_ivars}:{
	  add_form(`{
	      $outtype $f_ptr_name($argvars){
		${localize_ivars(rest_ivars,body)}
		$body}});}};
      add_form(`{install_method_ptr(`$f_name,`$class,$f_table_name,$f_ptr_name);});}}
}

int is_subclass(expptr type1, expptr type2){
  if(type1 == type2)return 1;
  expptr_iter(superclass, (getprop(type1,`superclasses,NULL))){
    if(is_subclass(superclass, type2))return 1;};
  return 0;
}

void check_method(expptr outtype, expptr fname, expptr argvars){
  expptr method_type = getprop(fname,`method_type,NULL);
  if(!method_type)berror(sformat("attempt to define undeclared method %s", exp_string(fname)));
  ucase{method_type;
    {$m_outtype($mtypes)}:{
      if(outtype != m_outtype)berror(sformat("method output type %s does not match declareed output type for maethod %s",
					     exp_string(outtype),
					     exp_string(fname)));
      comma_iter(argvar, argvars){
	if(!mtypes)berror(sformat("too many arguments in method definition for %s", exp_string(fname)));
	expptr mtype = comma_first(mtypes);
	ucase{argvar;{$atype $any}:{
	    if(!is_subclass(atype,mtype))berror(sformat("illegal argument type in method for %s",exp_string(fname)));}};
	mtypes = comma_rest(mtypes);};
      if(mtypes)berror(sformat("too few arguments in method definition for %s", exp_string(fname)));}}
}

void install_method_ptr(expptr f_name, expptr class, voidptrptr f_table, voidptr f_method){
  pushprop(f_name,getprop(class,`method_names));
  f_table[class_index(class)] = f_method;
  expptr_iter(subclass, getprop(class,`subclasses,NULL)){
    install_method_ptr(f_name,subclass,f_table,f_method);}
}


/** ========================================================================
 the method call macro.
========================================================================**/

expptr localize_ivars(expptr ivars, expptr body){
  ucase{ivars;
    {}:{return `{};};
    {$type $var ; $rest}:{
      if(occurs_in(var, body)){
	return `{$type $var = self->$var; ${localize_ivars(rest, body)}};}
      else{
	return localize_ivars(rest, body);}}};
  return NULL;
}

// localize_ivars(`{foo x; int y;}, `{return `{x,y};})

expptr method_type(expptr f_name){
  expptr type = getprop(f_name,`method_type, NULL);
  if(!type)berror(sformat("no type for method %s",atom_string(f_name)));
  return type;
}

expptr add_coercions(expptr argexps, expptr types);

expptr method_expansion(expptr e){
  expptr f_ptr = gensym("f_ptr");
  expptr y = gensym("y");
  expptr selfvar = gensym("self");
  expptr index = gensym("index");
  ucase{e;
    {$f($argexps)}:{
      ucase{method_type(f); {$outtype($argtypes)}:{
	  if(outtype == `void){
	    return `{
	      {${comma_first(argtypes)} $selfvar = (${comma_first(argtypes)}) ${comma_first(argexps)};
		$outtype (* $f_ptr)($argtypes); //declares the variable f_ptr
		int $index = ((object) $selfvar) -> class_index;
		if($index < 0 || $index >= class_dim())berror("illegal self object in method call");
		$f_ptr = ${method_table_name(f)}[$index];
		if(!$f_ptr)berror(sformat("method %s not defined on class %s",
					  atom_string(`$f),
					  atom_string(class_name[$index])));
		$f_ptr(${comma_cons(selfvar,
				    add_coercions(comma_rest(argexps),comma_rest(argtypes)))});}};}
	  else{
	    return `{
	      ({${comma_first(argtypes)} $selfvar = (${comma_first(argtypes)}) ${comma_first(argexps)};
		$outtype (* $f_ptr)($argtypes); //declares the variable f_ptr
		int $index = ((object) $selfvar) -> class_index;
		if($index < 0 || $index >= class_dim())berror("illegal self object in method call");
		$f_ptr = ${method_table_name(f)}[$index];
		if(!$f_ptr)berror(sformat("method %s not defined on class %s",
					  atom_string(`$f),
					  atom_string(class_name[$index])));
		$outtype $y = $f_ptr(${comma_cons(selfvar,
				    add_coercions(comma_rest(argexps),comma_rest(argtypes)))});
		$y;})};}
	}}}};
  berror("illegal syntax in method call");
  return NULL;
}

expptr leftmost_atom(expptr e){
  if(atomp(e))return e;
  if(parenp(e))return leftmost_atom(paren_inside(e));
  return leftmost_atom(car(e));
}

expptr infer_type(expptr e){
  ucase{e;
    {($type) $any}:{return type;};
    {$f $any}:{
      expptr sig = getprop(f,`signature,NULL);
      if(sig){return leftmost_atom(sig);}
      expptr mtype = getprop(f,`method_type,NULL);
      if(mtype){
	ucase{mtype;
	  {$type $any}:{return type;}}}
      berror(sformat("method invocation unable to determine the type of %s", exp_string(e)));};
    {$any}:{berror(sformat("method invocation unable to determine the type of %s", exp_string(e)));}};
  return NULL;
}

expptr add_coercions(expptr argexps, expptr types){
  if(argexps && !types)berror("too many arguments in method call");
  if(!argexps && types)berror("too few arguments in method call");
  if(!argexps)return NULL;
  expptr firstexp = comma_first(argexps);
  expptr first_exptype = infer_type(firstexp);
  expptr first_type = comma_first(types);
  expptr reduced_exp;
  ucase{firstexp;
    {($any) $e}:{reduced_exp = e;};
    {$any}:{reduced_exp = firstexp;}};
  if(!is_subclass(first_exptype,first_type))berror("subclass failure in method call");
  return comma_cons(`{(${comma_first(types)}) $reduced_exp},
		    add_coercions(comma_rest(argexps), comma_rest(types)));
}
	


/** ========================================================================
  piecewise mthods
========================================================================**/

void add_piece(expptr piecename, expptr f_name, expptr argvars, expptr body);

umacro{defpiece{$piecename $f($argvars){$body}}}{
  add_piece(piecename, f, argvars, body);
  return `{};
}

void add_piece(expptr piecename, expptr f_name, expptr argvars, expptr body){
  check_method(`void,f_name,argvars);
  ucase{comma_first(argvars);
    {$class self}:{
      expptr full_method = string_atom(sformat("%s_at_%s", atom_string(f_name), atom_string(class)));
      expptr full_piece = string_atom(sformat("%s_at_%s", atom_string(piecename), atom_string(class)));
      setprop(full_piece,`piece_code,body);
      expptr names = getprop(full_method,`piecenames,`{});
      if(!member(full_piece,names)){setprop(full_method,`piecenames,cons(full_piece,names));}}}
}

umacro{collect_method{$f($argvars)}}{
  check_method(`void,f,argvars);
  ucase{comma_first(argvars);
    {$class self}:{
      expptr full_method = string_atom(sformat("%s_at_%s", atom_string(f), atom_string(class)));
      expptr pieces = getprop(full_method,`piecenames,NULL);
      if(!pieces)berror(sformat("%s is not a piece method",atom_string(f)));
      expptr body = `{};
      dolist(piecename,pieces){body = cons(getprop(piecename,`piece_code,`{}),body);};
      return `{defmethod{void $f($argvars){$body}}};}}
}
    
  
/** ========================================================================
  test cases
========================================================================**/

// restart_undo_frame(0);

// expptr blah[0];

// leftmost_atom(getprop(`blah,`signature,NULL))

// expptr blah2(expptr e);

// leftmost_atom(getprop(`blah2,`signature,NULL))

//defclass{foo{object; expptr name;expptr y;}};

//int_exp(getprop_int(`foo,`size,-1))

// defclass{bar{foo;expptr wife;}};

// int_exp(is_subclass(`bar,`foo))

// foo fred[0] = new(foo);

// infer_type(`{fred[0]})

// fred[0]->name = `fred;

// fred[0]->name

// fred[0]->y = NULL;

// int_exp(fred[0]->y == NULL)

// bar barney[0] = new(bar);

// barney[0]->name = `barney;

// barney[0]->wife = `betty;

// declare_method{expptr name(foo self)}

// defmethod{expptr name(foo self){return name;}}

// name(fred[0])

// name(barney[0])

// defclass{bar2{foo; expptr wife;}}

// bar2 bambam[0] = new(bar2);

// name(bambam[0])

// declare_method{void set_y(foo self, foo new y)};

// defmethod{void set_y(bar self, bar friend){ self->y = friend->wife;}}

// set_y(barney[0], barney[0]);

// barney[0]->y

//defclass{piecemethod{object;}}

//declare_method{void bar(piecemethod self, int i)}

//defpiece{p1 void bar(piecemethod self, int i){do1(); do2();}}

//defpiece{p2 void bar(piecemethod self, int i){do3(); do4();}}

//macroexpand1(`{collect_method{void bar(piecemethod self, int i)}})

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

umacro{lambda($freevars)($args){$body}}{ // all free variables must be pointers and the closure must not return a value
  expptr pname = gensym("lambda_proc");
  expptr cname = gensym("closure");
  if(args == `{})add_form(`{void $pname(voidptrptr $cname){${wrap_body(freevars,cname,1,body)}}});
  else
  add_form(`{void $pname(voidptrptr $cname,$args){${wrap_body(freevars,cname,1,body)}}});
  return `{({
	      void** $cname = undo_alloc(${int_exp(sizeof(void*)*(1 + comma_length(freevars)))});
	      *$cname = $pname;
	      ${install_vars(cname,freevars,1)}
	      $cname;})};}

umacro{closure_type(($args)->$outtype)}{
  return(args == `{})? `{$outtype (**)(void*)} : `{$outtype (**)(void*,$args)};}

//macroexpand(`{closure_type((int,int)->int)})

//macroexpand(`{closure_type(()->int)})

umacro{closure_typedef($typename:($args)->$outtype)}{
  return (args== `{})? `{$outtype (**$typename)(void*)} : `{$outtype (**$typename)(void*,$args)};}

//macroexpand(`{closure_typedef(foo:(int,int)->int)})

//macroexpand(`{closure_typedef(foo:()->int)})

umacro{apply_closure($f)($args)}{
  if(args == `{}) return `{(* $f)($f)};
  return  `{(* $f)($f,$args)};}

umacro{lambda $outtype($freevars)($args){$body}}{ // all free variables must be pointers and the closure must not return a value
  expptr pname = gensym("lambda_proc");
  expptr cname = gensym("closure");
  expptr f = gensym("f");
  if(args == `{})add_form(`{void $pname(voidptrptr $cname){${wrap_body(freevars,cname,1,body)}}});
  else
  add_form(`{void $pname(voidptrptr $cname,$args){${wrap_body(freevars,cname,1,body)}}});
  return `{({
	      void** $cname = undo_alloc(${int_exp(sizeof(void*)*(1 + comma_length(freevars)))});
	      *$cname = $pname;
	      {${install_vars(cname,freevars,1)}}
	      closure_typedef($f:($args)->$outtype);
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

