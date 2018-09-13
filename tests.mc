//
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

for(int i = 0; i < 10; i++)x[i] = i;

for(int i = 0; i < 10; i++)fprintf(stdout,"%d",x[i]);

int_exp(x[5])

{int sum = 0; for(int i = 0; i < 10; i++)sum += x[i]; return int_exp(sum);}


/** ========================================================================
 C data types
======================================================================== **/

typedef struct myexpstruct{
  char * label;
  struct myexpstruct * car;
  struct myexpstruct * cdr;
} myexpstruct, *myexp;

myexp mycons(char * s, myexp x, myexp y){
      myexp cell = malloc(sizeof(myexpstruct));
      cell->label = s;
      cell->car = x;
      cell->cdr = y;
      return cell;}

expptr myexp_exp(myexp x){
  if(x == NULL) return string_atom("nil");
  return `{${string_atom(x->label)} ${myexp_exp(x->car)} ${myexp_exp(x->cdr)}};
}

myexp_exp(mycons("foo",mycons("bar",NULL,NULL),NULL))


/** ========================================================================
 Variable as x[0].
======================================================================== **/

int y[0] = 2;

y[0] += 1;

int_exp(y[0])

expptr friend[0] = `{Bob Givan};

int height[0] = 6;

`{My friend ${friend[0]} is ${int_exp(height[0])} feet tall.}

expptr e[0] = `{a+b};

`{bar(${e[0]})}


/** ========================================================================
 redefinition
======================================================================== **/

expptr g(expptr x){return x;}

g(`{a})

expptr g(expptr x){return `{$x $x};}

g(`{a})

/** ========================================================================
 mutual recursion and redefinition
======================================================================== **/

expptr bar(int i);

expptr foo(int i){
  if(i == 0){return `{foo};}
  return bar(--i);}

expptr bar(int i){
  if(i == 0){return `{bar};}
  return foo(--i);}

foo(1)

expptr bar(int i){
  if(i == 0){return `{bar2};}
  return foo(--i);}

foo(1)

/** ========================================================================
 macros
======================================================================== **/

umacro{mydolist($x, $L){$body}}{
     expptr rest = gensym("rest");
     return `{for(explist $rest = $L;
                  !atomp($rest);
                  $rest = cdr($rest);)
	 {expptr $x = car($rest); $body}};}

macroexpand(`{mydolist(item,list){f(item);}})


/** ========================================================================
 the "any" variable in patters
======================================================================== **/

macroexpand(`{ucase{`{a;b};{\$a;\$any}:{return a;}}})

ucase{`{a;b};{$a;$any}:{return a;}}

/** ========================================================================
 various
======================================================================== **/

int numeralp(expptr x){
  if(!atomp(x))return 0;
  char * s= atom_string(x);
  for(int i = 0; s[i] != '\0'; i++){
    if(s[i] < '0' || s[i] > '9')return 0;}
  return 1;
}

int value(expptr e){
  ucase{e;
   {$x+$y}:{return value(x)+value(y);}
   {$x*$y}:{return value(x)*value(y);}
   {($x)}:{return value(x);}
   {$z}.(numeralp(z)):{return atoi(atom_string(z));}}
  return 0;
}

int_exp(value(`{5+2*10}))

int_exp(value(`foo))

int_exp(value(`{foo}))

expptr bar(){
  breakpt("bar break");
  return `{a};
}

bar()

/** ========================================================================
 no arguments
======================================================================== **/

expptr f(){return `{a};}

f()

/** ========================================================================
 Procedure definition failure should not leave the procedure semi-defined.
 the following should generate a compilation error on both invocations rather than
 a segment fault on the second invocation.
======================================================================== **/

expptr g(expptr exp){returni exp;}

g(`{a})


/** ========================================================================
 strange
======================================================================== **/

expptr test(){
  return NULL;//a comment here used to cause a problem
}

int x[0]; //a comment here used to cause a problem


/** ========================================================================
 file_expressions
======================================================================== **/

expptr parenthesize(expptr l){
  if(!cellp(l))return l;
  return cons(`{(${car(l)})}, parenthesize(cdr(l)));
}

parenthesize(file_expressions("file-expressions-test-file.mc"))

/** ========================================================================
 printing
======================================================================== **/

mcpprint(`{foo});


/** ========================================================================
 segment fault
======================================================================== **/
expptr e[0];

e[0] = NULL;

e[0]->arg1


/** ========================================================================
 require
======================================================================== **/
#require("include_test")
/** 1: include_test.mc Provided **/

included(`{a})
/** 2: a **/


/** ========================================================================
 expansion error
======================================================================== **/

dolist{}{}


/** ========================================================================
 exp_from_undo_frame
======================================================================== **/

exp_from_undo_frame(`{a})

