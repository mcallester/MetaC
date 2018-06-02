#include "mc.h"

expptr casecode1(expptr,expptr,expptr,expptr);
expptr casecode2(expptr,expptr,expptr);
expptr constructor_code(char c);

void match_failure(expptr value, expptr patterns){
  fprintf(stderr,"\n match error: the value \n\n");
  gud_pprint(value);
  fprintf(stderr,"Does not match any of\n\n");
  gud_pprint(patterns);
  berror("");
}

expptr ucase_macro(expptr e){
  expptr pattern = `{{ucase{!exp;!rules}}};
  if(constructor(e) != 'A' || constructor(arg2(e)) != '{')match_failure(e,pattern);
  expptr arglist = arg1(arg2(e));
  if(constructor(arglist) != 'O' || arg1(arglist) != spaceop || constructor(op_arg1(arglist)) != ';')match_failure(e,pattern);
  expptr exp = arg1(op_arg1(arglist));
  expptr rules = op_arg2(arglist);
  
  expptr donelabel = gensym(`{done});
  expptr topvar = gensym(`{top});
  return `{
    {expptr ${topvar} = ${exp};
      ${casecode1(rules, topvar, donelabel, NULL)}
      ${donelabel}: ;}};
}

expptr casecode1(expptr rules, expptr topvar, expptr donelabel, expptr patterns){
  expptr rules_patterns =`{{!pattern:!body !rest} {!pattern:!body}};
  if(rules == NULL || constructor(rules) != 'O')match_failure(rules,rules_patterns);
  expptr  op = arg1(rules);
  expptr rule,elseform;
  if(op == spaceop){
    rule = op_arg1(rules);
    if(constructor(rule) != 'O')match_failure(rules,rules_patterns);
    elseform = casecode1(op_arg2(rules),topvar,donelabel,cons(op_arg1(rule),patterns));}
  else if(op == colonop){
    rule = rules;
    elseform = `{match_failure(${topvar},`{${cons(op_arg1(rule),patterns)}});};}
  else match_failure(rules,rules_patterns);
  if(constructor(rule) != 'O' || arg1(rule) != colonop || constructor(op_arg1(rule)) != '{' || constructor(op_arg2(rule)) != '{')match_failure(rules,rules_patterns);
  expptr pattern = arg1(op_arg1(rule));
  expptr body = arg1(op_arg2(rule));
  return `{
    ${casecode2(cons(cons(pattern,topvar),NULL),
		       macroexpand(body),
		       donelabel)}
    ${elseform}};
}

expptr casecode2(expptr pairs, expptr body, expptr donelabel){
  if(pairs==NULL)return`{${body} ; goto ${donelabel} ;};
  expptr restpairs = cdr(pairs);
  expptr thispair = car(pairs);
  expptr pattern = car(thispair);
  expptr valvar = cdr(thispair);
  if(pattern == NULL)return `{if(${valvar} == NULL){${casecode2(restpairs,body,donelabel)}}};
  char c = constructor(pattern);
  if(c == '!' || c == '?'){
    expptr var = arg1(pattern);
    if(var == NULL)berror("illegal use of exclamation point or question mark in ucase pattern");
    expptr rest = casecode2(restpairs,body,donelabel);
    if(c =='?') rest = `{if(constructor(${var}) == 'a'){${rest}}};
    return `{expptr ${var} = ${valvar};${rest}};}
  if(c == 'a' || c == 'o' || string_quotep(c))return `{if(${valvar} == ${quote_code(pattern)}){${casecode2(restpairs,body,donelabel)}}};
  expptr left = gensym(`{leftval});
  expptr right = gensym(`{rightval});
  return `{if(${valvar} && constructor(${valvar}) == ${constructor_code(c)}){
      expptr ${left} = arg1(${valvar});
      expptr ${right} = arg2(${valvar});
      ${casecode2(cons(cons(arg1(pattern),left), cons(cons(arg2(pattern),right), restpairs)),
		  body,
		  donelabel)}}};
}

void mcB_init(){
  set_macro(`{ucase},ucase_macro);
}
