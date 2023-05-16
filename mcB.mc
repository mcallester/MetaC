#include "mc.h"

expptr casecode1(expptr,expptr,expptr,expptr);
expptr casecode2(expptr,expptr,expptr);
expptr casecode3(expptr,expptr,expptr);
expptr casecode4(expptr,expptr,expptr);

void ucase_error(expptr e){
  fprintf(stdout,"invalid ucase syntax \n\n%s",exp_pps(e));
  berror("");}

void ucase_rule_error(expptr rule){
  fprintf(stdout,"invalid ucase rule \n\n%s",exp_pps(rule));
  berror("");}

void match_failure(expptr value, expptr unmatched_patterns){
  fprintf(stdout,"the value %s \n does not match any of \n %s",
	  exp_pps(value),
	  exp_pps(unmatched_patterns));
  berror("");
  }

expptr ucase_macro(expptr e){
  expptr top = rightarg(e);
  if(connofp(top,space) && parenp(leftarg(top)) && parenp(rightarg(top))){
    expptr valexp = paren_inside(leftarg(top));
    expptr rules = paren_inside(rightarg(top));
    expptr topvar = gensym(`top);
    expptr donelabel = gensym(`done);
    return `{{
	expptr $topvar = $valexp;
	${casecode1(rules, topvar, donelabel, NULL)}
	$donelabel: ;}};}
  ucase_error(e);
  }

expptr casecode1(expptr rules, expptr topvar,
		 expptr donelabel, expptr unmatched){
  if(!rules){return `{match_failure($topvar,${bqcode_ignore_dollar(unmatched)});};}
  if(!connofp(rules,semi)) berror("missing semicolon in rule list in ucase");
  expptr first_rule = leftarg(rules);
  return mk_connection(semi,
		       casecode2(first_rule,topvar,donelabel),
		       casecode1(rightarg(rules),topvar,donelabel,
				 mk_connection(comma,
					       leftarg(first_rule),
					       unmatched)));
  }


expptr casecode2(expptr rule, expptr topvar, expptr donelabel){
  expptr pattern = leftarg(rule);
  expptr body = rightarg(rule);
  if(connofp(rule,colon) &&
     parenp(body) &&
     constructor(body) == leftbrace){
    if(parenp(pattern) && constructor(pattern) == leftbrace){
      return casecode3(paren_inside(pattern), topvar, `{$body goto $donelabel;});}
    if(connofp(pattern,dot)){
      expptr test = rightarg(pattern);
      pattern = leftarg(pattern);
      if(parenp(pattern) && constructor(pattern) == leftbrace){
	if(parenp(test) && constructor(test) == leftparen){
	  return casecode4(paren_inside(pattern),
			   topvar,
			   `{if $test {$body goto $donelabel;}});}}}}
  ucase_rule_error(rule);
  }

expptr casecode3(expptr pattern, expptr valexp , expptr body){
  expptr valvar = gensym(`x);
  return `{{expptr $valvar = $valexp; ${casecode4(pattern,valvar,body)}}};
  }

expptr casecode4(expptr pattern, expptr valvar , expptr body){
  if(!pattern){return `{if(!$valvar){$body}};}
  if(atomp(pattern))return `{if($valvar == `{$pattern}){$body}};
  if(parenp(pattern)){
    return
    `{if(parenp($valvar) &&
	 constructor($valvar) == ${quote_char(constructor(pattern))}){
	
	${casecode3(paren_inside(pattern), `{paren_inside($valvar)}, body)}}};}
  if(connofp(pattern,space) &&
     leftarg(pattern) == dollar &&
     symbolp(rightarg(pattern))){
    if(rightarg(pattern) == `any)return body;
    return `{{expptr ${rightarg(pattern)} = $valvar; $body}};}
  if(connectionp(pattern)){
    return
    `{if(connectionp($valvar) && connector($valvar) == `{${connector(pattern)}})
      ${casecode3(leftarg(pattern),`{leftarg($valvar)},
		  casecode3(rightarg(pattern),`{rightarg($valvar)}
			    ,body))}};}
  ucase_error(pattern);
  }

void mcB_init(){
  set_macro(`ucase, ucase_macro);
}
