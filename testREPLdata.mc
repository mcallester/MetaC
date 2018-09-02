//
/** 2: compilation error **/
//
//
//
//

/** ========================================================================
 Hello world
======================================================================== **/

`{a}
/** 1: a **/


/** ========================================================================
 Procedure defininition
======================================================================== **/

expptr f(expptr exp){return exp;}
/** 2: done **/

f(`{a})
/** 3: a **/

/** ========================================================================
 Imperative Programming
======================================================================== **/

int x[10];
/** 4: done **/

for(int i = 0; i < 10; i++)x[i] = i;
/** 5: done **/

for(int i = 0; i < 10; i++)fprintf(stdout,"%d",x[i]);
/** 6: 0123456789done **/

int_exp(x[5])
/** 7: 5 **/

{int sum = 0; for(int i = 0; i < 10; i++)sum += x[i]; return int_exp(sum);}
/** 8: 45 **/


/** ========================================================================
 C data types
======================================================================== **/

typedef struct myexpstruct{
  char * label;
  struct myexpstruct * car;
  struct myexpstruct * cdr;
} myexpstruct, *myexp;
/** 9: done **/

myexp mycons(char * s, myexp x, myexp y){
      myexp cell = malloc(sizeof(myexpstruct));
      cell->label = s;
      cell->car = x;
      cell->cdr = y;
      return cell;}
/** 10: done **/

expptr myexp_exp(myexp x){
  if(x == NULL) return string_atom("nil");
  return `{${string_atom(x->label)} ${myexp_exp(x->car)} ${myexp_exp(x->cdr)}};
}
/** 11: done **/

myexp_exp(mycons("foo",mycons("bar",NULL,NULL),NULL))
/** 12: foo bar nil nil nil **/


/** ========================================================================
 Variable as x[0].
======================================================================== **/

int y[0] = 2;
/** 13: done **/

y[0] += 1;
/** 14: done **/

int_exp(y[0])
/** 15: 3 **/

expptr friend[0] = `{Bob Givan};
/** 16: done **/

int height[0] = 6;
/** 17: done **/

`{My friend ${friend[0]} is ${int_exp(height[0])} feet tall.}
/** 18: My friend Bob Givan is 6 feet tall. **/

expptr e[0] = `{a+b};
/** 19: done **/

`{bar(${e[0]})}
/** 20: bar(a+b) **/


/** ========================================================================
 redefinition
======================================================================== **/

expptr g(expptr x){return x;}
/** 21: done **/

g(`{a})
/** 2: a **/

expptr g(expptr x){return `{$x $x};}
/** 3: done **/

g(`{a})
/** 4: a a **/

/** ========================================================================
 mutual recursion and redefinition
======================================================================== **/

expptr bar(int i);
/** 1: done **/

expptr foo(int i){
  if(i == 0){return `{foo};}
  return bar(--i);}
/** 2: done **/

expptr bar(int i){
  if(i == 0){return `{bar};}
  return foo(--i);}
/** 3: done **/

foo(1)
/** 4: bar **/

expptr bar(int i){
  if(i == 0){return `{bar2};}
  return foo(--i);}
/** 5: done **/

foo(1)
/** 6: bar2 **/

/** ========================================================================
 macros
======================================================================== **/

umacro{mydolist($x, $L){$body}}{
     expptr rest = gensym("rest");
     return `{for(explist $rest = $L;
                  !atomp($rest);
                  $rest = cdr($rest);)
	 {expptr $x = car($rest); $body}};}
/** 21: done **/

macroexpand(`{mydolist(item,list){f(item);}})
/** 22: for
  (explist _genrest7=list;
   !atomp(_genrest7);
  _genrest7=cdr(_genrest7);
  ){expptr item=car(_genrest7);f(item);}
 **/

/** ========================================================================
 error modes and breakpoints
======================================================================== **/

int numeralp(expptr x){
  if(!atomp(x))return 0;
  char * s= atom_string(x);
  for(int i = 0; s[i] != '\0'; i++){
    if(s[i] < '0' || s[i] > '9')return 0;}
  return 1;
}
/** 1: done **/

int value(expptr e){
  ucase{e;
   {$x+$y}:{return value(x)+value(y);}
   {$x*$y}:{return value(x)*value(y);}
   {($x)}:{return value(x);}
   {$z}.(numeralp(z)):{return atoi(atom_string(z));}}
  return 0;
}
/** 2: done **/

int_exp(value(`{5+2*10}))
/** 3: 25 **/

int_exp(value(`foo))
/** 5: compilation error **/

int_exp(value(`{foo}))
/** 4: execution error (running gdb) **/

expptr bar(){
  breakpt("bar break");
  return `{a};
}
/** 1: done **/

bar()
/** 2: a **/

/** ========================================================================
 no arguments
======================================================================== **/

expptr f(){return `{a};}
/** 1: done **/

f()
/** 2: a **/

/** ========================================================================
 Procedure definition failure should not leave the procedure semi-defined.
 the following should generate a compilation error on both invocations rather than
 a segment fault on the second invocation.
======================================================================== **/

expptr g(expptr exp){returni exp;}
/** 2: compilation error **/

g(`{a})
/** 2: execution error (running gdb) **/


/** ========================================================================
 strange
======================================================================== **/

expptr test(){
  return NULL;//any comment exactly here causes the problem
}
/** 1: done **/

int x[0]; //comment
/** 1: done **/
