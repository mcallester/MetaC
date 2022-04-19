#include "mc.h"

expptr casecode1(expptr,expptr,expptr,expptr);
expptr casecode2(expptr,expptr,expptr,expptr,expptr);
expptr casecode3(expptr,expptr,expptr);
expptr casecode4(expptr,expptr,expptr);
expptr casecode5(expptr,expptr,expptr);

void match_failure(expptr value, expptr patterns){
  fprintf(stdout,"match error: the value \n\n");
  pp(value);
  fprintf(stdout,"does not match any of\n\n");
  pp(patterns);
  berror("");
}

void check_ucase(expptr e){
  expptr ucase_patterns = `{{ucase{\$exp;\$rules}}};
  if(cellp(e)
     && constructor(cdr(e)) == '{'
     &&     cellp(paren_inside(cdr(e)))
     && cellp(car(paren_inside(cdr(e))))
     &&   cdr(car(paren_inside(cdr(e)))) == semi)
    return;
  match_failure(e,ucase_patterns);
}

expptr ucase_macro(expptr e){
  check_ucase(e);
  expptr exp = car(car(paren_inside(cdr(e))));
  expptr rules =   cdr(paren_inside(cdr(e)));

  expptr donelabel = gensym(`done);
  expptr topvar = gensym(`top);
  return `{{expptr $topvar = $exp;
      ${casecode1(rules, topvar, donelabel, nil)}
      ${donelabel}: ;}};
}

expptr casecode1(expptr rules, expptr topvar, expptr donelabel, expptr patterns){
  if(cellp(rules) && cellp(car(rules)) && cdr(car(rules)) == semi){
    return casecode2(car(car(rules)),cdr(rules),topvar,donelabel,patterns);}
  return casecode2(rules,NULL,topvar,donelabel,patterns);
}

void check_rule(expptr rule){
  expptr rule_patterns = `{{\$pattern:{\$body}}};
  if(cellp(rule)
     && cellp(car(rule))
     && cdr(car(rule)) == colon
     && constructor(cdr(rule)) == '{')return;
  match_failure(rule, rule_patterns);
}

expptr casecode2(expptr first, expptr rest, expptr topvar, expptr donelabel, expptr patterns){
  check_rule(first);
  expptr pattern = car(car(first));
  expptr next_patterns = cons(pattern, patterns);
  if(!rest){
    return cons(casecode3(first,topvar,donelabel),
		`{match_failure($topvar, ${quote_code(next_patterns)});});}
  return cons(casecode3(first,topvar,donelabel),
	      casecode1(rest,topvar,donelabel,next_patterns));
}

void check_pattern(expptr pattern){
  expptr pattern_patterns = `{{{\$pattern}} {{\$pattern}.(\$test)}};
  if(constructor(pattern) == '{')return;
  if(cellp(pattern)
     && cellp(car(pattern))
     && cdr(car(pattern)) == dot
     && constructor(car(car(pattern))) == '{'
     && constructor(cdr(pattern)) == '(')
    return;
  match_failure(pattern,pattern_patterns);
}

expptr casecode3(expptr rule, expptr topvar, expptr donelabel){
  expptr pattern = car(car(rule));
  check_pattern(pattern);
  expptr body = paren_inside(cdr(rule));
  if(constructor(pattern) == '{'){
    return casecode4(paren_inside(pattern), topvar, `{$body goto $donelabel;});}
  expptr test = paren_inside(cdr(pattern));
  pattern = paren_inside(car(car(pattern)));
  return casecode4(pattern, topvar, `{if($test){$body goto $donelabel;}});
}

expptr casecode4(expptr pattern, expptr valexp , expptr body){
  if(pattern == any)return body;
  expptr valvar = gensym(`{});
  return `{expptr $valvar = $valexp; ${casecode5(pattern,valvar,body)}};
}

expptr casecode5(expptr pattern, expptr valvar , expptr body){
  if(atomp(pattern))return `{if($valvar == `{$pattern}){$body}};
  if(parenp(pattern)){
    return
    `{if(parenp($valvar) && constructor($valvar) == ${constructor_code(constructor(pattern))}){
	${casecode4(paren_inside(pattern), `{paren_inside($valvar)}, body)}}};}
  if(car(pattern) == dollar){
    if(symbolp(cdr(pattern))){
      return
      `{{expptr ${cdr(pattern)} = $valvar; $body}};}
    if(cellp(cdr(pattern)) && symbolp(car(cdr(pattern)))){
      return
      `{if(cellp($valvar)){
	  expptr ${car(cdr(pattern))} = car($valvar);
	  ${casecode4(cdr(cdr(pattern)),`{cdr($valvar)},body)}}};}}
  return
  `{if(cellp($valvar)){
      ${casecode4(car(pattern),`{car($valvar)},casecode4(cdr(pattern),`{cdr($valvar)},body))}}};
  }

void mcB_init(){
  set_macro(`{ucase}, ucase_macro);
}
