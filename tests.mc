//
//
//
//
//

/** ========================================================================
 Hello world
======================================================================== **/

`{a}


/** ========================================================================
 Procedure defininition
======================================================================== **/

expptr f(expptr exp){return exp;}

f(`{a})

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
/** 2:done **/

myexp mycons(char * s, myexp x, myexp y){
      myexp cell = malloc(sizeof(myexpstruct));
      cell->label = s;
      cell->car = x;
      cell->cdr = y;
      return cell;}
/** 4:done **/

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

expptr barf(){
  breakpt("bar break");
  return `{a};
}

barf()

/** ========================================================================
 no arguments
======================================================================== **/

expptr foobar(){return `{a};}

foobar()

/** ========================================================================
 Procedure definition failure should leave the procedure undefined
======================================================================== **/

expptr goo(expptr exp){returni exp;}
/** compilation error **/

goo(`{a})
/** compilation error **/


/** ========================================================================
compilation error of type defintion should not leave trash in file_preamble
======================================================================== **/

typedef struct myexpstruct{
  char * label;
  myexp car;
  struct myexpstruct * cdr;
} myexpstruct, *myexp;
/** compilation error **/

`{a}
/** 3:a **/

/** ========================================================================
 procedure type declaration without procedure definition should result
 in a reasonable state.
======================================================================== **/

expptr goop(expptr exp);

goop(`{a})

/** ========================================================================
 strange
======================================================================== **/

expptr test(){
  return NULL;//a comment here used to cause a problem
}

int y[0]; //a comment here used to cause a problem


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
 load
======================================================================== **/
load("include_test")

included(`{a})


/** ========================================================================
 expansion error
======================================================================== **/

dolist{}{}


/** ========================================================================
 exp_from_undo_frame
======================================================================== **/

exp_from_undo_frame(`{foo(a)})


/** ========================================================================
 illegal signature should not be installed
======================================================================== **/

notype bad(int x){return x;}

int bad(int x){return x;}

int_exp(bad(3))


/** ========================================================================
reader errors
======================================================================== **/

foo(lkj)))`{foo bar}

expptr friend[0] = ‘{Bob Givan};


/** ========================================================================
some version of the system failed to recover from this expansion error
======================================================================== **/

umacro{test()}{return file_expressions("nonexistent_file");}

test()


/** ========================================================================
This example failed to behave in some version.

compilation error expected for foo() because of intexp rather than int_exp
bar() has the bug fixed.
======================================================================== **/

umacro{foo()}{
  add_preamble(`{int z[0]=0;});
  return `{intexp(z[0])};}

foo()

umacro{bar()}{
  add_preamble(`{int z[0]=0;});
  return `{int_exp(z[0])};}

bar()


/** ========================================================================
NULL values
======================================================================== **/
0

`{}

`{value prints as empty string}

nil()


/** ========================================================================
dynamic linking of catch and throw
======================================================================== **/

void throw_test(){
  throw();
}

void catch_test(){
  catch(throw_test(););
}

catch_test();


/** ========================================================================
commacase and spacecase
======================================================================== **/

commacase{`{a c,b};{$x,$y}:{return `{$x,$y};}}
/** 1:a c,b **/

commacase{`{a c};{$x,$y}:{return `{$x ${int_exp(y == NULL)}};}}
/** 2:a c 1 **/

spacecase{`{(a c) b};{($x $y) $z}:{return `{($x $y),$z};}}
/** 1:(a c),b **/

spacecase{`{(a b)};{($x $y) $z}:{return `{($x $y) ${int_exp(z == NULL)}};}}
/** 2:(a b)1 **/

macroexpand(`{defclass{just}})
/** 2:typedef struct just_struct{int imp_index;}*just **/
