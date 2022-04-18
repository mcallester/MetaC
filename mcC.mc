#include "mc.h"

expptr init_form_macro(expptr e){
  ucase{e; {init_form($form)}:{add_init_form(form); return NULL;}};
  return NULL;
}

expptr umacro_macro(expptr e){
  ucase{e;
    {umacro{$pattern}{$body}}:{
      expptr name = top_atom(pattern);
      if(getprop(name,`signature,NULL))berror("macro name already defined as non-macro");
      if(name == NULL)berror("illegal pattern in umacro");
      char * s = atom_string(name);
      expptr fname = symbolp(name) ? gensym(s) : gensym("connective");
      add_init_form(`{set_macro(`{$name},$fname);});
      return `{
	expptr $fname(expptr e){
	  ucase{e;{$pattern}:{$body}};
	  return NULL;}};}};
  return NULL;
  }

expptr init_fun_macro(expptr e){
  ucase{e; {init_fun($fname)}:{
      return `{void $fname(){$init_forms}};}};
  return nil;}

void mcC_init(){
  init_forms = nil;
  set_macro(`{umacro},umacro_macro);
  set_macro(`{init_form}, init_form_macro);
  set_macro(`{init_fun}, init_fun_macro);
}
