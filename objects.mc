
/** ========================================================================
  general utilities
========================================================================**/


// define_lists defines list types and list operations over a given c type.

void add_list_forms(expptr type){
  char * cstring = atom_string(type);
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

//list operations on C statement lists (semicolon lists).

expptr semi_append(expptr x, expptr y){
  ucase{x;
    {$first ; $rest}:{return `{$first;${semi_append(rest,y)}};};
    {$last ;}:{return `{$last ; $y};}};
  return NULL;
}

expptr semi_reverse(expptr x){
  expptr result = NULL;
  ucase{x;
    {{}}:{return x;};
    {$any ;}:{return x;};
    {$first ; $rest}:{result = `{$first ;}; x = rest;}};
  while(1){
    ucase{x;
      {$last ;}:{return `{$last ; $result};};
      {$first ; $rest}:{result = `{$first ; $result}; x = rest;}}}
}


// semi_reverse(`{a; b; c;})  

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
	  {\$$x ; \$$rest}:{$result = `{\${($expression)} ;}; $yval = $rest;};
	  {\$$x ;}:{$result = `{\${($expression)} ;}; $yval = NULL;}};
	while($yval){
	  ucase{$yval;
	    {\$$x ; \$$rest}:{$result = `{\${($expression)} ; \$$result}; $yval = $rest;};
	    {\$$x ;}:      {$result = `{\${($expression)} ; \$$result}; $yval = NULL;}}};
	$result;})};
}


// macroexpand1(`{semi_map(z,`{a;b;c})(`{(d,\$z)})})

// semi_map(z,`{a;b;c;})(`{(d,$z)})

expptr semi_to_comma(expptr e){
  ucase{e;
    {$first ; $rest}:{return `{$first,${semi_to_comma(rest)}};};
    {$last ;}:{return last;};
    {$any}:{return e;}}
}

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

restart_undo_frame(0);

void add_class_forms(expptr superclass, expptr class, expptr added_ivars);

umacro{defclass{$superclass; $class{$added_ivars}}}{
  add_class_forms(superclass, class, added_ivars);
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

expptr class_superclass(expptr class){
  expptr superclass = getprop(class,`superclass,NULL);
  if(!superclass)berror(sformat("undefined class %s",atom_string(class)));
  return superclass;
}

expptr fix_typedef_recursion(expptr class, expptr structname, expptr ivars); // this allows the given class to be used directly as a type in ivars.

void add_class_constructor(expptr class, expptr ivars);

void add_class_forms(expptr superclass, expptr class, expptr added_ivars){
  if(class == `object)berror("attempt to redefine the object class");
  expptr complete_ivars = semi_append(class_ivars(superclass), added_ivars); //this ensures that superclass is a defined class.
  expptr old_superclass = getprop(class,`superclass,NULL);
  if(old_superclass){
    if(superclass != old_superclass || complete_ivars != class_ivars(class))
      berror(sformat("attempt to change definition of class %s", atom_string(class)));}
  else{
    expptr structname = string_atom(sformat("%s_struct",atom_string(class)));
    add_form(`{typedef struct $structname{${fix_typedef_recursion(class, structname, complete_ivars)}} $structname, * $class;});
    add_list_forms(class);
    add_class_constructor(class, complete_ivars);
    add_form(`{install_class(`$superclass, `$class, `{$complete_ivars});});}
}

expptr fix_typedef_recursion(expptr class, expptr structname, expptr ivars){
  return semi_reverse(semi_map(e,ivars)({
	expptr result = NULL;
	ucase{e;
	  {$type $var}:{
	    if(type == class){
	      result = `{struct $structname * $var};
	    }
	    else {
	      result = e;}}};
	result;}));
}

// fix_typedef_recursion(`{foo}, `{foo_struct}, `{int x; foo y; int z;})

int class_counter[0] = 1;

umacro{class_dim()}{return `100;}

expptr class_name[class_dim()];

class_name[0] = `object;

for(int i = 1; i<class_dim(); i++){class_name[i] = NULL;};

void copy_methods(expptr superclass, expptr new_class);

void install_class(expptr superclass, expptr class, expptr complete_ivars){
  int index = class_counter[0];
  if(index == class_dim()){
    berror(sformat("attempt to create more than %d classes", class_dim()));}
  setprop_int(class,`index, index);
  class_counter[0]++ ;
  class_name[index] = class;
  setprop(class, `ivars, complete_ivars);
  setprop(class,`superclass, superclass);
  pushprop(class, getprop(superclass, `subclasses));
  copy_methods(superclass, class);
}

expptr init_ivars(expptr ivars){
  ucase{ivars;
    {$any $var ; $rest}:{return `{self->$var = $var; ${init_ivars(rest)}};};
    {$any $var;}:{return `{self->$var = $var;};}};
  return NULL;
}

void add_class_constructor(expptr class, expptr ivars){
  expptr constructor = string_atom(sformat("new_%s",atom_string(class)));
  expptr structname = string_atom(sformat("%s_struct",atom_string(class)));
  ucase{ivars;
    {$any;}:{berror(sformat("class %s has no instance variables",atom_string(class)));};
    {$any;$rest_ivars}:{
      add_form(`{
	  $class $constructor(${semi_to_comma(rest_ivars)}){
	    $class self = ($class) undo_alloc(sizeof($structname));
	    self->class_index = class_index(`$class);
	    ${init_ivars(rest_ivars)}
	    return self;}
	});}}
}

/** ========================================================================
  
 if, for some class foo, we do

 defmethod{expptr bar.g(foo self; expptr x;){return x;}}

 then g is declared by expptr g(foo self, expptr x);

 For g(s,z) to be defined the class of s must be a subclass (inclusive) of foo.

 The code that gets fun for g(s,z) depends on the subtype of foo to which g is applied.

 If g is a declared method then g(s,z) macro-expands to an appropriately typed version of M_g[s->class_index](s,z)
 where M_g is the statically determined method table for g.

 all instance variables, including inherited instance variables, get bound to local
 variables of the same name.  There is also a local self variable.

========================================================================**/

restart_undo_frame(1);

void add_method_forms(expptr outtype, expptr f_name, expptr argvars, expptr body);

void install_method_ptr(expptr f_name, expptr class, voidptrptr f_table, voidptr f_method);

umacro{defmethod{$outtype $f($argvars){$body}}}{
  add_method_forms(outtype, f, argvars, body);
  return `{};
}

expptr method_table_name(expptr f){
  return string_atom(sformat("%s__method_table",atom_string(f)));
}

expptr localize_ivars(expptr ivars, expptr body);

void add_method_forms(expptr outtype, expptr f_name, expptr argvars, expptr body){
  expptr f_table_name = method_table_name(f_name);
  expptr f_type = `{$outtype($argvars)};
  expptr selfvar = gensym("self");
  expptr restargvars = NULL;
  expptr old_type = getprop(f_name,`method_type,NULL);
  orcase{argvars; {$class self, $restargvars}{$class self}:{
      if(!old_type){
	add_form(`{voidptr $f_table_name[class_dim()];});
	add_form(`{install_method_name(`$f_name, $f_table_name, `{$f_type});});}
      else if(f_type != old_type){
	berror(sformat("attempt to change the signature of method %s",atom_string(f_name)));}
      expptr ivars = class_ivars(class);
      expptr f_ptr_name = string_atom(sformat("%s_at_%s", atom_string(f_name), atom_string(class)));
      ucase{ivars;
	{$any ; $rest_ivars}:{
	  add_form(`{
	      $outtype $f_ptr_name(${restargvars ? `{voidptr $selfvar, $restargvars} : `{voidptr $selfvar}}){
		$class self = $selfvar;
		${localize_ivars(rest_ivars,body)}
		$body}});}};
      add_form(`{install_method_ptr(`$f_name,`$class,$f_table_name,$f_ptr_name);});}}
}

expptr method_expansion(expptr e);

void install_method_name(expptr f_name, voidptrptr f_table, expptr f_type){
  set_macro(f_name, method_expansion);
  for(int i = 0;i<class_dim();i++){f_table[i] = NULL;};
  setprop(f_name,`method_type,f_type);
  setprop(f_name, `method_table, f_table);
}

void install_method_ptr(expptr f_name, expptr class, voidptrptr f_table, voidptr f_method){
  pushprop(f_name,getprop(class,`method_names));
  f_table[class_index(class)] = f_method;
  expptr_iter(subclass, getprop(class,`subclasses,NULL)){
    install_method_ptr(f_name,subclass,f_table,f_method);}
}

voidptrptr method_table(expptr f_name){
  voidptrptr f_table = getprop(f_name,`method_table,NULL);
  if(!f_table)berror(sformat("undefined method name %s", atom_string(f_name)));
  return f_table;
}

void copy_methods(expptr superclass, expptr new_class){
  int superindex = class_index(superclass);
  expptr_iter(f_name, (getprop(superclass, `method_names, NULL))){
    voidptrptr table = method_table(f_name);
    voidptr f_ptr =  table[superindex];
    install_method_ptr(f_name,new_class,table,f_ptr);}
}

expptr localize_ivars(expptr ivars, expptr body){
  if(!ivars) return `{};
  expptr rest = NULL;
  orcase{ivars;{$type $var ; $rest}{$type $var;}:{
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


expptr method_expansion(expptr e){
  expptr f_ptr = gensym("f_ptr");
  expptr y = gensym("y");
  expptr selfvar = gensym("self");
  expptr index = gensym("index");
  ucase{e;
    {$f($argexps)}:{
      expptr restexps = NULL;
      orcase{argexps;{$selfexp, $restexps}{$selfexp}:{
	  ucase{method_type(f); {$outtype($argvars)}:{
	      if(outtype == `void){
		return `{
		  {voidptr $selfvar = $selfexp;
		    $outtype (* $f_ptr)($argvars); //declares the variable f_ptr
		    int $index = ((object) $selfvar) -> class_index;
		    if($index < 0 || $index >= class_dim())berror("illegal self object in method call");
		    $f_ptr = ${method_table_name(f)}[$index];
		    if(!$f_ptr)berror(sformat("method %s not defined on class %s",
					      atom_string(`$f),
					      atom_string(class_name[$index])));
		    
		    $f_ptr(${restexps ? `{$selfvar, $restexps} : selfvar});}};}
	      else{
		return `{
		  ({voidptr $selfvar = $selfexp;
		    $outtype (* $f_ptr)($argvars); //declares the variable f_ptr
		    int $index = ((object) $selfvar) -> class_index;
		    if($index < 0 || $index >= class_dim())berror("illegal self object in method call");
		    $f_ptr = ${method_table_name(f)}[$index];
		    if(!$f_ptr)berror(sformat("method %s not defined on class %s",
					      atom_string(`$f),
					      atom_string(class_name[$index])));
		    $outtype $y = $f_ptr(${restexps ? `{$selfvar, $restexps} : selfvar});
		    $y;})};
	      }}}}}}};
  return NULL;
}


/** ========================================================================
  test cases
========================================================================**/

// restart_undo_frame(2);

// defclass{object;foo{expptr name;foo y;}}

// foo fred[0] = new_foo(`fred,NULL);

// fred[0]->name

// int_exp(fred[0]->y == NULL)

// defclass{foo; bar{expptr wife;}}

// bar barney[0] = new_bar(`barney,NULL,`betty);

// barney[0]->name

// barney[0]->wife



// defmethod{expptr name(foo self){return name;}}

// name(fred[0])

// name(barney[0])

// defclass{foo; bar2{expptr wife;}}

// bar2 bambam[0] = new_bar2(`bambam,NULL,`huh);

// name(bambam[0])


// defmethod{void set_y(foo self, foo new_y){
    self->y = new_y;}}

// set_y(fred[0],(foo) barney[0]);

// fred[0]->y->name
