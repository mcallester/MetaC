#include "mc.h"

expptr init_form_macro(expptr e){
  ucase(e){
    {init_form($form)}:{add_init_form(form); return NULL;};};
  return NULL;
  }

expptr umacro_macro(expptr e){
  ucase(e){
    {umacro{$pattern}{$body}}:{
      expptr name = head_symbol(pattern);
      if(name == NULL)berror("illegal pattern in umacro");
      if(getprop(name,`signature,NULL)){
	berror("macro name already defined as non-macro");}
      expptr fname = gensym(name);
      add_init_form(`{set_macro(`{$name},$fname);});
      return `{
	expptr $fname(expptr e){
	  ucase(e){
	    {$pattern}:{$body};};
	  return NULL;}};};};
  return NULL;
  }

expptr init_form_body(explist statements){
  if(!statements)return NULL;
  return mk_connection(space,
		       init_form_body(statements->rest),
		       statements->first);
  }

expptr init_fun_macro(expptr e){
  ucase(e){
    {init_fun($fname)}:{
      return `{void $fname(){${init_form_body(init_forms)}}};};}
  return NULL;}

void mcC_init(){
  preamble = NULL;
  init_forms = NULL;
  set_macro(`umacro,umacro_macro);
  set_macro(`init_form, init_form_macro);
  set_macro(`init_fun, init_fun_macro);
}
