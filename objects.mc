/** ========================================================================
  define_lists defines list types and list operations over a given c type.
========================================================================**/

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

/** ========================================================================
  abstract classes (classes which are inhereted from but not instantiated)

  defclass{superclass foo(<var1> <type1>; ... <varn> <typen>;)} defines foo as a pointer
  to a structure type with an index slot added at the front.  Instances of this type
  will typically have additional fields and the index identifies the implementation
  class.

  currently instance variables are inhereted but methods are not.  In fact, Methods cannot be defined
  on abstract classes. However, concrete class methods can call independently defined superclass procedures.

  Methods inheretance is not difficult to implement and may be implemented at some future point.
========================================================================**/
expptr semi_append(expptr x, expptr y){
  ucase{x;
    {$first ; $rest}:{return `{$first;${semi_append(rest,y)}};}
    {$last ;}:{return `{$last ; $y};}}
  return NULL;
}
/** 3:done **/

expptr complete_ivars(expptr class){
  if(class == `{object})
    return `{int implementation_index;};
  else
    return semi_append(complete_ivars(getprop(class,`{superclass},NULL)),
		       getprop(class,`{class_ivars},NULL));
}
/** 4:done **/

void declare_class(expptr superclass, expptr class, expptr ivars){
  if( superclass != `{object} && !getprop(superclass,`{class_ivars},NULL)){
    berror(sformat("illegal superclass %s",atom_string(superclass)));}
  expptr old_superclass = getprop(class,`{superclass},NULL);
  expptr old_ivars = getprop(class,`{class_ivars},NULL);
  if(old_ivars && (ivars != old_ivars || superclass != old_superclass)){
    berror(sformat("attempt to change definition of class %s", atom_string(class)));}
  if(!old_ivars){
    add_form(`{setprop(`{$class},`{class_ivars}, `{$ivars});});
    add_form(`{setprop(`{$class},`{superclass}, `{$superclass});});
    expptr structname = string_atom(sformat("%s_struct",atom_string(class)));
    add_form(`{typedef struct $structname{${semi_append(complete_ivars(superclass),ivars)}} $structname, * $class;});
    define_lists(class);}
}
/** 26:done **/

umacro{defclass{$superclass $class($ivars)}}{
  declare_class(superclass, class, ivars);
  return `{};
}
/** 6:done **/

/** ========================================================================
  Concrete classes

  the constructor for a concrete class takes an argument for all instance variables
  (including inherited variables) other than the implementation index.  The return
  type of the constructor is the immediate superclass.
========================================================================**/

expptr init_ivars(expptr ivars){
  ucase{ivars;
    {$any $var ; $rest}:{return `{self->$var = $var; ${init_ivars(rest)}};}
    {$any $var;}:{return `{self->$var = $var;};}}
  return NULL;
}
/** 7:done **/

expptr semi_to_comma(expptr e){
  ucase{e;
    {$first ; $rest}:{return `{$first,${semi_to_comma(rest)}};}
    {$last ;}:{return last;}
    {$any}:{return e;}}
}
/** 8:done **/


int implement_counter[0] = 0;
/** 9:done **/

int implementation_dim[0] = 100;
/** 10:done **/

int implementation_index(expptr implementation){
  int index = getprop_int(implementation,`{index},-1);
  if(index >= 0)return index;
  index = implement_counter[0];
  if(index == implementation_dim[0]){
    berror(sformat("attempt to create more than %d implementations", implementation_dim[0]));}
  add_form(`{implement_counter[0]++ ;});
  add_form(`{setprop_int(`{$implementation},`{index}, ${int_exp(index)});});
  return index;
}
/** 11:done **/

void add_imp_constructor(expptr superclass, expptr class, expptr ivars){
  int index = implementation_index(class);
  expptr constructor = string_atom(sformat("new_%s",atom_string(class)));
  expptr structname = string_atom(sformat("%s_struct",atom_string(class)));
  expptr all_ivars = semi_append(complete_ivars(superclass), ivars);
  ucase{all_ivars;
    {$any;}:{berror(sformat("implementation class %s has no instance variables",atom_string(class)));}
    {$any;$rest_ivars}:{
      add_form(`{
	  $superclass $constructor(${semi_to_comma(rest_ivars)}){
	    $class self = ($class) undo_alloc(sizeof($structname));
	    self->implementation_index = ${int_exp(index)};
	    ${init_ivars(rest_ivars)}
	    return ($superclass) self;}
	});}}
}
/** 29:done **/

umacro{defimp{$superclass$class($ivars)}}{
  declare_class(superclass, class, ivars);
  add_imp_constructor(superclass, class, ivars);
  return `{};
}
/** 13:done **/


/** ========================================================================
  methods are macros that expand to an aref to retrieve the method particular to the
  given object.

  all instance variables, including inherited instance variables, get bound to local
  variables of the same name.
========================================================================**/

expptr method_table(expptr f){
  expptr table = getprop(f,`{method_table},NULL);
  if(table)return table;
  table = gensym("method_table");
  add_form(`{voidptr $table[implementation_dim[0]];});
  add_form(`{for(int i = 0;i<implementation_dim[0];i++){$table[i] = NULL;};});
  add_form(`{setprop(`{$f},`{method_table},`{$table});});
  return table;
}
/** 14:done **/

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
/** 15:done **/

expptr method_type(expptr f){
  expptr type = getprop(f,`{type}, NULL);
  if(!type)berror("MC bug: no type for method");
  return type;
}
/** 16:done **/

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
		      $outtype $y = $f_imp(${restexps ? `{$selfvar, $restexps} : selfexp});
		      $y;})};
		}}}}}}}}
  return NULL;
}
/** 17:done **/

int occurs_in(expptr symbol, expptr exp){
  if(atomp(exp))return (symbol == exp);
  if(cellp(exp))return (occurs_in(symbol,car(exp)) || occurs_in(symbol,cdr(exp)));
  return occurs_in(symbol,paren_inside(exp));
}
/** 18:done **/

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
/** 19:done **/

umacro{defmethod{$outtype $f($argvars){$body}}}{
  expptr f_imp = gensym(atom_string(f));
  declare_method(outtype, f, argvars);
  orcase{argvars; {$selfimp self, $any}{$selfimp self}:{
      int index = getprop_int(selfimp, `{index}, -1);
      if(index < 0)berror(sformat("class %s is not an implementation class", atom_string(selfimp)));
      add_form(`{
	  ${method_table(f)}[${int_exp(index)}] = $f_imp;
	});
      expptr ivars = complete_ivars(selfimp);
      ucase{ivars;
	{$any ; $rest_ivars}:{
	  add_form(`{
	      $outtype $f_imp($argvars){
		${localize_ivars(rest_ivars,body)}
		$body}});}}}}
  return `{};
}
/** 20:done **/

/** ========================================================================
    general exceptions
======================================================================== **/

expptr exception[0] = NULL;
/** 21:done **/

expptr exception_value[0];
/** 22:done **/

umacro{catch_excep{$exception}{$body}{$handler}}{
  return `{
    catch({$body})
      if(exception[0]){
	if(exception[0] == `{$exception}){
	  $handler
	  expception[0] = NULL;}
	else continue_throw();}};
}
/** 23:done **/

umacro{throw_excep{$exception;$value}}{
  return `{
    expcetion[0] = `{$exception};
    exception_value[0] = $value;
    throw();};
}
/** 24:done **/

/** ========================================================================
  a macro to prevent "unmentioned variable" compiler errors
======================================================================== **/

umacro{mention($x)}{
  return `{if($x ? $x : $x){}};}
/** 25:done **/


/** ========================================================================
  tests
========================================================================**/


defclass{object foo(expptr x;)};
/** 27:done **/

void f(foo x);
/** 28:done **/

defimp{foo bar(expptr y;)};
/** 30:done **/

foo foovar[0] = new_bar(`{a},`{b});
/** 31:done **/

int_exp(foovar[0]->implementation_index)
/** 33:0 **/

foovar[0]->x
/** 32:a **/

defimp{foo baz(expptr y;)};
/** 34:done **/

foo foovar2[0] = new_baz(`{a},`{b});
/** 35:done **/

int_exp(foovar2[0]->implementation_index)
/** 36:1 **/

defmethod{expptr gety(bar self){return y;}};
/** segment fault --- to resume type p NIDE() **/
