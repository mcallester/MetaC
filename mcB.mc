#include "mc.h"

expptr casecode1(expptr,expptr,expptr,expptr);
expptr casecode2(expptr,expptr,expptr);
expptr casecode3(expptr,expptr,expptr);
expptr casecode4(expptr,expptr,expptr);

void match_failure(expptr value, expptr patterns){
  fprintf(stdout,"match error: the value \n\n");
  dbpprint(value);
  fprintf(stdout,"does not match any of\n\n");
  dbpprint(patterns);
  berror("");
}

expptr ucase_macro(expptr e){

  expptr ucase_pattern = `{{ucase{\$exp;\$rules}}};

  if(!(cellp(e)
       && constructor(cdr(e)) == '{'
       &&     cellp(paren_inside(cdr(e)))
       && cellp(car(paren_inside(cdr(e))))
       &&   cdr(car(paren_inside(cdr(e)))) == semi))
    match_failure(e,ucase_pattern);
  expptr exp = car(car(paren_inside(cdr(e))));
  expptr rules =   cdr(paren_inside(cdr(e)));

  expptr donelabel = gensym("done");
  expptr topvar = gensym("top");
  return `{{expptr $topvar = $exp;
      ${casecode1(rules, topvar, donelabel, nil)}
      ${donelabel}: ;}};
}

expptr casecode1(expptr rules, expptr topvar, expptr donelabel, expptr patterns){

  expptr rules_patterns = `{{\$pattern:{\$body}} {\$pattern:{\$body} \$rest}};

  if(!(cellp(rules)
       && cellp(car(rules))))
    match_failure(rules,rules_patterns);
  if(cdr(car(rules)) == colon){ //only first pattern possible
    return cons(casecode2(rules,topvar,donelabel),
		`{match_failure($topvar, ${quote_code(cons(car(car(rules)), patterns))});});}
  //only second pattern possible
  if(!cellp(car(car(rules))))match_failure(rules,rules_patterns);
  return cons(casecode2(car(rules),topvar,donelabel),
	      casecode1(cdr(rules),topvar,donelabel,cons(car(car(car(rules))), patterns)));
}

expptr casecode2(expptr rule, expptr topvar, expptr donelabel){

  expptr rule_pattern = `{\$ptest:{\$body}};

  if(!(cellp(rule)
       && cellp(car(rule))
       && cdr(car(rule)) == colon
       && parenp(cdr(rule))
       && constructor(cdr(rule)) == '{'))
    match_failure(rule, rule_pattern);
  expptr ptest = car(car(rule));
  expptr body = paren_inside(cdr(rule));

  expptr ptest_patterns = `{{\$pattern} {{\$pattern}.(\$test)}};

  if(parenp(ptest) && constructor(ptest) == '{'){
    expptr pattern = paren_inside(ptest);
    return casecode3(pattern, topvar, `{$body goto $donelabel;});}

  if(!(cellp(ptest)
       && cellp(car(ptest))
       && cdr(car(ptest)) == period
       && parenp(car(car(ptest)))
       && constructor(car(car(ptest))) == '{'
       && parenp(cdr(ptest))
       && constructor(cdr(ptest)) == '('))
    match_failure(ptest,ptest_patterns);
  expptr pattern = paren_inside(car(car(ptest)));
  expptr test = paren_inside(cdr(ptest));

  return casecode3(pattern, topvar, `{if($test){$body goto $donelabel;}});
}

expptr casecode3(expptr pattern, expptr valexp , expptr body){
  if(pattern == any)return body;
  expptr valvar = gensym("");
  return `{expptr $valvar = $valexp; ${casecode4(pattern,valvar,body)}};
}

expptr casecode4(expptr pattern, expptr valvar , expptr body){
  if(atomp(pattern))return `{if($valvar == `{$pattern}){$body}};
  if(parenp(pattern)){
    return `{if(parenp($valvar) && constructor($valvar) == ${constructor_code(constructor(pattern))}){
	${casecode3(paren_inside(pattern), `{paren_inside($valvar)}, body)}}};}
  if(car(pattern) == dollar){
    if(!(symbolp(cdr(pattern))))berror("illegal syntax for variable in ucase pattern");
    return `{{expptr ${cdr(pattern)} = $valvar; $body}};}
  return
    `{if(cellp($valvar)){
      ${casecode3(car(pattern),`{car($valvar)},casecode3(cdr(pattern),`{cdr($valvar)},body))}}};
}

void mcB_init(){
  set_macro(`{ucase}, ucase_macro);
}
